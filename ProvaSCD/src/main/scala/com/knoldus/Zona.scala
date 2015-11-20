package com.knoldus

import scalax.collection.GraphPredef._
import scalax.collection.Graph,
       scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge
import scalax.collection.edge.WDiEdge
import scalax.collection.edge.Implicits._

import akka.cluster.ClusterEvent._
import akka.cluster.Member
import akka.cluster.MemberStatus
import akka.cluster.Cluster
import akka.actor.RootActorPath
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import scala.collection.mutable.ArrayBuffer
import scala.xml.XML
import scala.concurrent.Await
import akka.util.Timeout
import scala.concurrent.duration._
import scala.util.Success
import com.typesafe.config.Config
import akka.dispatch.PriorityGenerator
import akka.dispatch.UnboundedStablePriorityMailbox

class Zona (i:String, p:String, ln:ArrayBuffer[String]) extends Actor 
{
	//Zone vicine
	var zoneVicine = ArrayBuffer[ActorRef]();
	//Lista corsie e marciapiedi
	var corsiaIn = ArrayBuffer[ActorRef]();
	var corsiaOut = ArrayBuffer[ActorRef]();
	var corsiaInCont = ArrayBuffer[ActorRef]();
	var corsiaOutCont = ArrayBuffer[ActorRef]();
	var marciapiedeIn = ArrayBuffer[ActorRef]();
	var marciapiedeOut = ArrayBuffer[ActorRef]();
	//Lista fermate
	var fermateIn = ArrayBuffer[ActorRef]();
	var fermateOut = ArrayBuffer[ActorRef]();
	var destinazione: ActorRef =null;
	var incrocio: ActorRef=null;
	var id:String=i;
	//Contatori per la creazione dei vicini
	var zone=0;
	var contZone:Int= -1;
	var nomiZone = ArrayBuffer[String]();
	var nomiZone2 = ArrayBuffer[String]();
	//Porta (per evitare di mandare messaggi a se stesso inutilmente)
	var port:String =p;
	var xml:scala.xml.Elem=null;
	val cluster = Cluster(context.system);
	var mezziRicevuti = ArrayBuffer[String]();
	val maxMezzi: Int= 1;
	var pedoniRicevuti = ArrayBuffer[String]();
	val maxPedoni: Int= 1;

	
	var members = ArrayBuffer[Member]();
	val listaNomi:ArrayBuffer[String]=ln;
	var listeVicini=ArrayBuffer[ArrayBuffer[String]]();
	var grafo=scalax.collection.mutable.Graph(id, id~id);

	override def preStart(): Unit = 
	{
		cluster.subscribe(self, initialStateMode = InitialStateAsEvents,classOf[MemberEvent], classOf[UnreachableMember]);
	}

  	override def receive: Actor.Receive = 
	{
		case num:Int => if(("Z"+num.toString)==id) sender!id;
		case Start => start;

		case Grafo => chiediVicini;
		case Blocco2 => if (listeVicini.size!=members.size-1) self!Blocco2; else creaGrafo;
		case ChiediVicini => sender!new CostruzioneGrafo(ArrayBuffer[String](id)++nomiZone2);
		case c: CostruzioneGrafo => salvaVicini(c.vicini);
		case MemberUp(member:Member) => members+=member; if(contZone!=zone) { registra(member); }
		case Vicini => if(zone==0 || contZone== -1 || zone!=contZone) self!Vicini; else creaResto;
		case Incroci => creaIncrocio;
		case Strade => creaStrade;
		case Marciapiedi => creaMarciapiedi;
		case AggiornaIncrocio => if(incrocio!=null) {incrocio!new containerActrf(corsiaOut, marciapiedeOut);
		                      	   incrocio!"Manda"; }
		case Movimenti => spostamenti;
		case s:String => aggiungiVicino(s, sender);		

		case p:personaDeviata => sender!true; deviaPersona(p); //Check
		//case p:Persona => if (id=="Z5") sender!false; else { sender!true; /*if(checkPedoni(p))*/ inviaPedone(p); }
		case p:Persona => if (id=="Z5") sender!false; else { sender!true; inviaPedone(p); }
		case m:Mezzo => sender!true; if(checkMezzi(m)) inviaMezzo(m);
		case p:containerPersonaDeviata => deviaPersona(p.persona);
	} 

	def deviaPersona (pd:personaDeviata) : Unit =
	{
		if(pd.trattiDaFare.size==0 && pd.trattiFatti.size==0)
		{
			var go=true;
			val p: Persona=pd.persona;
			var arrivato=p.next;
			var percorso=p.percorso;
			for (k<-1 to 4) //Guardo i prossimi 4 punti perché nel caso peggiore posso avere 6I1_1-F-6I1_2-6O1_1-F-6O1_2
			{
				if( (percorso(arrivato+k).contains("FINE") || percorso(arrivato+k).contains("X") || percorso(arrivato+k).contains("F")))
				{
					//
					//
					//SEGNO CHE E'E' UNA DESTINAZIONE NON RAGGIUNGIBILE E CHIUDO TUTTO
					//
					go=false;
				}
			}
			if(go)
			{
				//Prendo la zona dopo prossimaZona
				var next:String=p.to2Zona;
				//percorso minimo
				grafo-="Z"+p.toZona;
				val gr=grafo; //Non vuole la variabile grafo
				def n(outer: String): gr.NodeT = gr get outer
				println("Deviazione da "+id+" a Z"+next);
				val spO = n(id) shortestPathTo n("Z"+next); //ECCEZIONE java.util.NoSuchElementException se non lo trova
				val sp = spO.get;
				var listaZone:String=sp.nodes.toString.substring(6)
				listaZone=listaZone.substring(0, listaZone.size-1)
				var abz=ArrayBuffer[String]();
				abz++=listaZone.split(", ");
				//Creo lista in personaDeviata
				pd.trattiDaFare=abz;	
			}
		}
		pd.resetPercorso;
		pd.trattoFatto;
		if(pd.trattiFatti.size==1)
		{
			pd.percorso+=id.substring(1)+"MI"; //CONTROLLO CHE LO FACCIA VERAMENTE
			var marciapiede:String=pd.persona.nxt.substring(pd.persona.nxt.indexOf("O")+1);
			pd.percorso(0)+=marciapiede; //CONTROLLO CHE LO FACCIA VERAMENTE
		}
		else if(pd.trattiDaFare.size!=0)
		{ 
			pd.next= -1;
			pd.percorso+=id.substring(1)+"MI"; 
			var marciapiede:String="";
			for (k<-0 to nomiZone2.size-1)
			{	
				if(nomiZone2(k)==pd.trattiFatti(pd.trattiFatti.size-2))
					marciapiede=(k+1).toString;
			}
			//marciapiede+=1;
			pd.percorso(0)+=marciapiede; 
		}

		if(pd.trattiDaFare.size!=0)
		{
			pd.percorso+=id.substring(1)+"MO";
			var marciapiede:String="";
			for (k<-0 to nomiZone2.size-1)
			{
				if(nomiZone2(k)==pd.trattiDaFare(0))
					marciapiede=(k+1).toString;
			}
			//marciapiede+=1;
			if(marciapiede.startsWith("0"))
				marciapiede=marciapiede.substring(1); //Tolgo lo 0 all'inizio altrimenti in incrocio ciclerà
			pd.percorso(1)+=marciapiede; 
			marciapiedeIn(pd.percorso(0).substring(3).toInt-1)!pd;
		}
		else
		{ 
			var pedone:Persona=pd.persona;
			while(!pedone.to.startsWith(id.substring(1)))
			{
				pedone.inc;
			}
			//pedone.inc;
			var dove=pedone.to;
			dove=dove.substring(3);
			marciapiedeIn(dove.toInt-1)!pedone;		
		}
	}

	def creaGrafo: Unit =
	{
		//var g=scalax.collection.mutable.Graph(id, id~id);
		for(x<-0 to listeVicini.size-1)
		{
			val l=listeVicini(x);
			grafo+=l(0)
		}
		for(x<-0 to nomiZone2.size-1)
		{
			if(nomiZone2(x)!="null")
				grafo += (id~nomiZone2(x));
		}
		for(x<-0 to listeVicini.size-1)
		{
			val l=listeVicini(x);
			for(y<-1 to l.size-1)
			{
				if(l(y)!="null")
					grafo += (l(0)~l(y));
			}
		}
		grafo -= (id~id);
	}

	def chiediVicini: Unit =
	{
		for (x<- 0 to members.size-1)
		{		
			context.system.actorSelection(RootActorPath(members(x).address)/"user"/"Map")!new ChiediVicini(id);
		}
	}

	def salvaVicini (vi: ArrayBuffer[String]): Unit =
	{
		listeVicini+=vi;
	}

	def checkPedoni(p:Persona): Boolean =
	{
		val idPedone=p.id;
		val result= pedoniRicevuti.contains(idPedone);
		if(pedoniRicevuti.size==maxPedoni)
		{
			pedoniRicevuti.remove(0);	
		}
		pedoniRicevuti+=idPedone;
		!result;
	}

	def checkMezzi(m:Mezzo): Boolean =
	{
		val idMezzo=m.id;
		val result= mezziRicevuti.contains(idMezzo);
		if(mezziRicevuti.size==maxMezzi)
		{
			mezziRicevuti.remove(0);	
		}
		mezziRicevuti+=idMezzo;
		!result;
	}

	def creaResto:Unit =
	{
		self!Grafo;
		self!Blocco2;
		//Creo la destinazione
		destinazione=context.actorOf(Props(classOf[Destinazione], self), id+"Destinazione");
		self!Incroci;
		self!Strade;
		self!Marciapiedi;
		self!AggiornaIncrocio;	
		//Avvio mezzi e pedoni	
		self!Movimenti;	
	}

	def aggiungiVicino(is: String, z: ActorRef): Unit =
	{		
		for (x<- 0 to nomiZone.size-1)
		{
			if(nomiZone(x)==is)
			{
				zoneVicine(x)=z;
				nomiZone(x)="null";
				if (contZone == -1) 
					contZone+=1;
				contZone+=1;
			}
		}
	}

	def inviaPedone(p: Persona): Unit =
	{
		p.incZona;
		var dove=p.to;
		dove=dove.substring(3);
		marciapiedeIn(dove.toInt-1)!p;
	}

	def inviaMezzo(m: Mezzo): Unit =
	{
		m.incZona;
		var dove=m.to;
		val pos=dove.indexOf("_"); 
		if (pos>=0)
			dove=dove.substring(2, pos);
		else
			dove=dove.substring(2);
		corsiaIn(dove.toInt-1)!m;
	}

	def start: Unit  =
	{
		var act=context.system.actorOf(Props(classOf[MapActor],id,self), "Map");
		val cluster = Cluster(context.system);
		//Leggo il file XML
		var nome="/home/marco/Scrivania/ProvaSCD/src/main/scala/com/knoldus/"+id+".xml";
		xml=XML.loadFile(nome);
		id=xml\"Zona"\"@id" text;
		leggiVicini;
		//Creo i pezzi
		self!Vicini;	
	}

	def registra(member:Member): Unit =
	{	
		for(x<- 0 to nomiZone.size-1)
		{
			var nome:String=nomiZone(x);
			if (nome!="null" && !RootActorPath(member.address).toString.contains(port))
			{
				context.system.actorSelection(RootActorPath(member.address)/"user"/nome)!nome.substring(1).toInt;	 	
			}
		}
	}

	def spostamenti: Unit =
	{
		var x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;} 

		//Creo prima i pedoni
		var spostamenti = (xml \ "Zona" \ "Pedoni" \ "Pedone");
		for(x <- 0 to spostamenti.size-1)
		{
			var identificativo:String= spostamenti(x)\"@id" text;
			var percorso:String=spostamenti(x)\"@percorso" text;
			var ab=ArrayBuffer[String]();
			ab++=percorso.split(",");
			var zone:String=spostamenti(x)\"@zone" text;
			var abz=ArrayBuffer[String]();
			abz++=zone.split(",");
			var prsn= new Pedone (identificativo, ab, abz);
			self!prsn;
		}

		//Poi le automobili
		spostamenti = (xml \ "Zona" \ "Automobili" \ "Automobile");
		for(x <- 0 to spostamenti.size-1)
		{
			var identificativo:String= spostamenti(x)\"@id" text;
			var percorso:String=spostamenti(x)\"@percorso" text;	
			var guidatore:String=spostamenti(x)\"@guidatore" text;
			var ab=ArrayBuffer[String]();
			ab++=percorso.split(",");
			var zone:String=spostamenti(x)\"@zone" text;
			var abz=ArrayBuffer[String]();
			abz++=zone.split(",");
			var mzz= new Auto (identificativo, ab, new Pedone(guidatore, ArrayBuffer[String](), ArrayBuffer[String]()), abz);
			self!mzz;
		}	

		//Ed infine gli autobus
		spostamenti = (xml \ "Zona" \ "Autobus" \ "Bus");
		for(x <- 0 to spostamenti.size-1)
		{
			var identificativo:String= spostamenti(x)\"@id" text;
			var percorso:String=spostamenti(x)\"@percorso" text;	
			var ab=ArrayBuffer[String]();
			ab++=percorso.split(",");
			var zone:String=spostamenti(x)\"@zone" text;
			var abz=ArrayBuffer[String]();
			abz++=zone.split(",");
			//Dopo aver letto il percorso e l'id devo prendere gli eventuali passeggeri
			var passeggeri=ArrayBuffer[Persona]();
			var  pass= (spostamenti \ "Passeggeri" \ "Passeggero");
			for(x <- 0 to pass.size-1)
			{
				var identificativop:String= pass(x)\"@id" text;
				var percorsop:String=pass(x)\"@percorso" text;
				var abp=ArrayBuffer[String]();
				abp++=percorsop.split(",");
				var passeggero=new Pedone (identificativop, abp, ArrayBuffer[String]());
				passeggero.inc;
				passeggeri+=passeggero;
			}
			var mzz= new Autobus (identificativo, ab, abz);
			if(pass.size!=0)
				mzz.passeggeri=passeggeri;
			self!mzz;
		}
	}

	def leggiVicini: Unit =
	{
		var vicini = (xml \ "Zona" \ "Vicini" \ "Vicino");
		for(x <- 0 to vicini.size-1)
		{
			zoneVicine+=null;
			var attr:String= vicini(x)\"@id" text;
			if(attr=="Nessuno")
			{
				nomiZone+="null";
				nomiZone2+="null";		
			}	
			else
			{
				nomiZone+=attr;
				nomiZone2+=attr;
				zone+=1;
			}
		}
	}

	def creaStrade: Unit =
	{
		var dest=new containerDestinazione(destinazione);
		//CORSIE ENTRANTI
		var strade = (xml \ "Zona" \ "Strade" \ "StradeIn" \ "StradaIn");
		for(x <- 0 to strade.size-1)
		{
			fermateIn+=null;
			var attr:String= strade(x)\"@id" text;
			corsiaIn+=context.actorOf(Props(classOf[Corsia], self), attr);
			corsiaIn(x)!attr;
			corsiaInCont+=null;
			var fermata:String= strade(x)\"@fermata" text;
			if(fermata!="")
			{
				fermateIn(x)=context.actorOf(Props[Fermata], fermata);
				fermateIn(x)!self;
				fermateIn(x)!fermata;
				var conteiner=new containerFermata(fermateIn(x));
				corsiaIn(x)!conteiner;
			}
		} 
		strade = (xml \ "Zona" \ "StradeCont" \ "StradeInCont" \ "StradaInCont");
		//Imposto le destinazioni delle strade
		for(x <- 0 to strade.size-1)
		{
			var attr:String= strade(x)\"@id" text;
			var pos= attr.substring(2, 3).toInt; //1I1_2
			corsiaInCont(pos-1)=context.actorOf(Props(classOf[Corsia], self), attr);
			corsiaInCont(pos-1)!attr;
		}
		if(corsiaIn.size!=0)
		{
			for(x <- 0 to (corsiaIn.size-1))
			{
				corsiaIn(x)!dest;
				if(corsiaInCont(x)!=null)
					corsiaIn(x)!corsiaInCont(x);
				else if(incrocio!=null)
					corsiaIn(x)!incrocio;
			}
		}
		if(corsiaInCont.size!=0)
		{
			for(x <- 0 to (corsiaInCont.size-1))
			{
				
				if(corsiaInCont(x)!=null)
				{
					corsiaInCont(x)!dest;
					corsiaInCont(x)!incrocio;
				}
			}
		}

		//CORSIE USCENTI
		strade = (xml \ "Zona" \ "Strade" \ "StradeOut" \ "StradaOut");
		for(x <- 0 to strade.size-1)
		{
			fermateOut+=null;
			var attr:String= strade(x)\"@id" text;
			corsiaOut+=context.actorOf(Props(classOf[Corsia], self), attr);
			corsiaOut(x)!attr;
			corsiaOutCont+=null;
			var fermata:String= strade(x)\"@fermata" text;
			if(fermata!="")
			{
				fermateOut(x)=context.actorOf(Props[Fermata], fermata);
				fermateOut(x)!self;
				fermateOut(x)!fermata;
				var conteiner=new containerFermata(fermateOut(x));
				corsiaOut(x)!conteiner;
			}
		}
		strade = (xml \ "Zona" \ "StradeCont" \ "StradeOutCont" \ "StradaOutCont");
		for(x <- 0 to strade.size-1)
		{
			var attr:String= strade(x)\"@id" text;
			var pos= attr.substring(2, 3).toInt; //1O3_2
			corsiaOutCont(pos-1)=context.actorOf(Props(classOf[Corsia], self), attr);
			corsiaOutCont(pos-1)!attr;
		}	
		//Imposto le destinazioni delle strade
		for(x <- 0 to (corsiaOut.size-1))
		{
			corsiaOut(x)!dest;
			if(corsiaOutCont(x)!=null)
				corsiaOut(x)!corsiaOutCont(x);
			else if (zoneVicine(x)!=null)
				corsiaOut(x)!new containerZona(zoneVicine(x));
		}		
		for(x <- 0 to (corsiaOutCont.size-1))
		{
			if(corsiaOutCont(x)!=null)
			{
				corsiaOutCont(x)!dest;
				if (zoneVicine(x)!=null)
					corsiaOutCont(x)!new containerZona(zoneVicine(x));
			}
		}
	}

	def creaMarciapiedi: Unit =
	{
		var dest=new containerDestinazione(destinazione);
		//MARCIAPIEDI ENTRANTI
		var marciapiedi = (xml \ "Zona" \ "Marciapiedi" \ "MarciapiediIn" \ "MarciapiedeIn");
		for(x <- 0 to marciapiedi.size-1)
		{
			var attr:String= marciapiedi(x)\"@id" text;
			marciapiedeIn+=context.actorOf(Props(classOf[Marciapiede], self), attr);
			marciapiedeIn(x)!attr;
			var fermata:String= marciapiedi(x)\"@fermata" text;
			if(fermata!="")
			{
				var conteiner=new containerFermata(fermateIn(x));
				marciapiedeIn(x)!conteiner;
			}
		}

		//MARCIAPIEDI USCENTI
		marciapiedi = (xml \ "Zona" \ "Marciapiedi" \ "MarciapiediOut" \ "MarciapiedeOut");
		for(x <- 0 to marciapiedi.size-1)
		{
			var attr:String= marciapiedi(x)\"@id" text;
			marciapiedeOut+=context.actorOf(Props(classOf[Marciapiede], self), attr);
			marciapiedeOut(x)!attr;
			var fermata:String= marciapiedi(x)\"@fermata" text;
			if(fermata!="")
			{
				var conteiner=new containerFermata(fermateIn(x));
				marciapiedeOut(x)!conteiner;
			}
		}
		//Imposto le destinazioni dei vari marciapiedi
		for(x <- 0 to (marciapiedeIn.size-1))
		{
			marciapiedeIn(x)!dest;
			marciapiedeIn(x)!marciapiedeIn.size;
		}
		for(x <- 0 to (marciapiedeOut.size-1))
		{
			marciapiedeOut(x)!dest;
			marciapiedeOut(x)!marciapiedeOut.size;
		}
		for(x <- 0 to (marciapiedeIn.size-1))
		{	
			if (x==0)
				marciapiedeIn(x)!ArrayBuffer(incrocio, marciapiedeOut(marciapiedeOut.size-1));
			else
				marciapiedeIn(x)!ArrayBuffer(incrocio, marciapiedeOut(x-1));	

			if (zoneVicine(x)!=null)
				marciapiedeOut(x)!new containerZona(zoneVicine(x));
		}
	}

	def creaIncrocio: Unit =
	{
		//Creo l'incrocio
		var tratti = ArrayBuffer[ActorRef]();
		var strisce = ArrayBuffer[ActorRef]();

		//TRATTI
		var tratto = (xml \ "Zona" \ "Incrocio" \ "Tratti" \ "Tratto");
		if(tratto.size!=0)
		{
			//Creo l'incrocio
			incrocio=context.actorOf(Props[Incrocio].withDispatcher("prio-dispatcher3"), "Incrocio"+id);
			incrocio!"Start";			
			for(x <- 0 to tratto.size-1)
			{
				var attr:String= tratto(x)\"@id" text;
				tratti+=context.actorOf(Props[Tratto].withDispatcher("prio-dispatcher"), attr);
				tratti(x)!attr;
				if(x%2==0)
					tratti(x)!Verde;
				else
					tratti(x)!Rosso;
			}
		}

		//STRISCE
		var striscia = (xml \ "Zona" \ "Incrocio" \ "StriscePedonali" \ "StrisciaPedonale");
		if(striscia.size!=0)
		{
			for(x <- 0 to striscia.size-1)
			{
				var attr:String= striscia(x)\"@id" text;
				strisce+=context.actorOf(Props[StrisciaPedonale].withDispatcher("prio-dispatcher2"), attr);
				strisce(x)!attr;
				strisce(x)!Rosso;
			}
			//Invio tratti e strisce all'incrocio
			for(x <- 0 to (tratti.size-1))
			{
				incrocio!ArrayBuffer(tratti(x), strisce(x));
			}
		}
	}
}

class ZonaPriorityActorMailbox(settings: ActorSystem.Settings, config: Config) extends UnboundedStablePriorityMailbox(
PriorityGenerator 
{
	case Start => 0
	case num:Int => 1
	case Vicini => 3
		
	case Grafo => 4
	case c: ChiediVicini => 4
	case c: CostruzioneGrafo => 4
	case Blocco2 => 5
	
	case Incroci => 6
	case Strade => 6
	case Marciapiedi => 6
	case AggiornaIncrocio => 6	
	case Movimenti => 6
	case m:mezzoDeviato => 7
	case p:personaDeviata => 7
	case m:Mezzo => 7
	case p:Persona => 7	
	case m:containerMezzoDeviato => 7
	case p:containerPersonaDeviata => 7
	case _ => 2
})
