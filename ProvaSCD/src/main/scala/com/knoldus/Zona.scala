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

	var zoneCadute = ArrayBuffer[String]();
	var members = ArrayBuffer[Member]();
	val listaNomi:ArrayBuffer[String]=ln;
	var listeVicini=ArrayBuffer[ArrayBuffer[String]]();
	var grafo=scalax.collection.mutable.Graph(id, id~id);



	var simul:Int=0;


	override def preStart(): Unit = 
	{
		cluster.subscribe(self, initialStateMode = InitialStateAsEvents,classOf[MemberEvent], classOf[UnreachableMember]);
	}

	override def postStop(): Unit = 
	{
		cluster.unsubscribe(self);
	}

  	override def receive: Actor.Receive = 
	{
		case num:Int => if(("Z"+num.toString)==id) sender!id;
		case Start => start;

		case FermaTutto => stop; 
		case c: ZonaCaduta => println("Mi hanno detto di togliere il nodo "+c.id+" dal grafo."); grafo-= c.id;
		case c: ZonaTornata => println("Mi hanno detto di riaggiungere il nodo "+c.id+" al grafo."); aggiornaGrafo(c.id);
		case Grafo => chiediVicini;
		case Blocco2 => if (listeVicini.size!=members.size-1) self!Blocco2; else creaGrafo;
		case ChiediVicini => sender!new CostruzioneGrafo(ArrayBuffer[String](id)++nomiZone2);
		case c: CostruzioneGrafo => salvaVicini(c.vicini);
		case MemberUp(member:Member) => println("\n\nMemberUp!\n\n"); members+=member; if(contZone!=zone || zoneCadute!=0) { registra(member); }
		case Vicini => if(zone==0 || contZone== -1 || zone!=contZone) self!Vicini;  else creaResto;
		case Incroci => creaIncrocio;
		case Strade => creaStrade;
		case Marciapiedi => creaMarciapiedi;
		case AggiornaIncrocio => if(incrocio!=null) {incrocio!new containerActrf(corsiaOut, marciapiedeOut);
		                      	   incrocio!"Manda"; }
		case Movimenti => spostamenti;
		case s:String => aggiungiVicino(s, sender);		

		case m:mezzoDeviato => sender!true; deviaMezzo(m); 
		case p:personaDeviata => sender!true; deviaPersona(p); 
		//case p:Persona => if (id=="Z15") sender!false; else { sender!true; /*if(checkPedoni(p))*/ inviaPedone(p); }
		case p:Persona => if (id=="Z15" && simul<21) { sender!false; simul+=1; } else if (id=="Z15" && simul>=21) { sender!true; inviaPedone(p); } else { sender!true; inviaPedone(p); }
		//case m:Mezzo => sender!true; if(checkMezzi(m)) inviaMezzo(m);
		case m:Mezzo => if (id=="Z15") sender!false; else { sender!true; inviaMezzo(m); }
		case p:containerPersonaDeviata => deviaPersona(p.persona);
		case m:containerMezzoDeviato => deviaMezzo(m.mezzo);
	} 

	def stop: Unit =
	{
		for (x<- 0 to zoneVicine.size-1)
		{		
			if(zoneVicine(x)!=null)
				zoneVicine(x)!FermaTutto;
		}
		for (x<- 0 to fermateIn.size-1)
		{
			if(fermateIn(x)!=null)
				context.stop(fermateIn(x));
		}
		for (x<- 0 to fermateOut.size-1)
		{
			if(fermateOut(x)!=null)
				context.stop(fermateOut(x));
		}
		for (x<- 0 to marciapiedeIn.size-1)
		{
			if(marciapiedeIn(x)!=null)
				context.stop(marciapiedeIn(x));
			if(marciapiedeOut(x)!=null)
				context.stop(marciapiedeOut(x));
		}
		for (x<- 0 to corsiaIn.size-1)
		{
			if(corsiaIn(x)!=null)
				context.stop(corsiaIn(x));
			if(corsiaInCont(x)!=null)
				context.stop(corsiaInCont(x));
			if(corsiaOut(x)!=null)
				context.stop(corsiaOut(x));
			if(corsiaOutCont(x)!=null)
				context.stop(corsiaOutCont(x));
		}
		if(destinazione!=null)
			context.stop(destinazione);
		if(incrocio!=null)
			context.stop(incrocio);
		context.stop(self);
		println("Zona "+id+" fermata");
	}

	def deviaMezzo (md:mezzoDeviato) : Unit =
	{
		var stop=false;
		if(md.trattiDaFare.size==0 && md.trattiFatti.size==0)
		{
			var go=true;
			val m: Mezzo=md.mezzo;
			var arrivato=m.next;
			var percorso=m.percorso;
			arrivato+=1;
			while( percorso(arrivato)!="R" && percorso(arrivato)!="F" && percorso(arrivato)!="FINE" && percorso(arrivato)!="X" && percorso(arrivato).startsWith(m.toZona) )
			{
				arrivato+=1;
			}
			if(percorso(arrivato)=="R" || percorso(arrivato)=="F" || percorso(arrivato)=="FINE" || percorso(arrivato)=="X" || percorso(arrivato)=="R")
			{
				println("La zona irraggiungibile è importante!");
				go=false;
				stop=true;
				self!FermaTutto;
			}
			if(go)
			{
				//Prendo la zona dopo prossimaZona
				var next:String=m.to2Zona;
				//percorso minimo
				val zonaCaduta:String="Z"+m.toZona;
				//Avviso tutte le zone che una è caduta. Se non lo faccio, potrebbero verificarsi dei lup (io calcolo un percorso, quello dopo ne calcola un altro e lo rimanda a me)
				grafo find zonaCaduta match 
				{
				    	case Some(i) => avvisoZonaCaduta(zonaCaduta); grafo-=zonaCaduta; zoneCadute+=zonaCaduta;
					case None => println("Non devo togliere il nodo "+zonaCaduta+" perché non c'è già più.");
				}		
				val gr=grafo; //Non vuole la variabile grafo
				def n(outer: String): gr.NodeT = gr get outer
				println("Deviazione mezzo da "+id+" a Z"+next);
				try 
				{
					val spO = n(id) shortestPathTo n("Z"+next); //ECCEZIONE java.util.NoSuchElementException se non lo trova
					val sp = spO.get;
					var listaZone:String=sp.nodes.toString.substring(6)
					listaZone=listaZone.substring(0, listaZone.size-1)
					var abz=ArrayBuffer[String]();
					abz++=listaZone.split(", ");
					//Creo lista in personaDeviata
					md.trattiDaFare=abz;
				}
				catch
				{
					case e: NoSuchElementException => println("La zona Z"+next+" è irraggiungibile!"); stop=true; self!FermaTutto;
				}
			}
		}
		if(!stop)
		{
			md.resetPercorso;
			md.trattoFatto;
			var pos:Int=0;
			if(md.trattiFatti.size==1)
			{
				md.percorso+=id.substring(1)+"I"; 
				var corsia:String=md.mezzo.nxt.substring(md.mezzo.nxt.indexOf("O")+1);
				md.percorso(pos)+=corsia; 
				if(corsiaInCont(corsia.toInt-1)!=null)
				{
					md.percorso(pos)+="_1";
					pos+=1;
					md.percorso+=id.substring(1)+"I"+corsia+"_2";
				}
			}
			else if(md.trattiDaFare.size!=0)
			{ 
				md.next= -1;
				md.percorso+=id.substring(1)+"I"; 
				var corsia:String="";
				for (k<-0 to nomiZone2.size-1)
				{	
					if(nomiZone2(k)==md.trattiFatti(md.trattiFatti.size-2))	
					{
						corsia=(k+1).toString;
						if(corsiaInCont(k)!=null)
						{
							md.percorso(pos)+=corsia+"_1";
							md.percorso+=id.substring(1)+"I"+"_2"+corsia;
							pos+=1;
						}
						else
							md.percorso(pos)+=corsia; 
					}
				}
			}
			pos+=1;
			if(md.trattiDaFare.size!=0)
			{
				md.percorso+=id.substring(1)+"O";
				var corsia:String="";
				for (k<-0 to nomiZone2.size-1)
				{
					if(nomiZone2(k)==md.trattiDaFare(0))
					{
						corsia=(k+1).toString;
						if(corsia.startsWith("0"))
						corsia=corsia.substring(1); //Tolgo lo 0 all'inizio altrimenti in incrocio ciclerà
						if(corsiaOutCont(k)!=null)
						{
							md.percorso(pos)+=corsia+"_1";
							md.percorso+=id.substring(1)+"O"+"_2"+corsia;
							pos+=1;
						}
						else
							md.percorso(pos)+=corsia; 
					}
				}	
				val dove:String=md.percorso(0);	
				if(dove.contains("_1"))
					corsiaIn(dove.substring(id.length,dove.indexOf("_")).toInt-1)!md;
				else	
					corsiaIn(dove.substring(id.length).toInt-1)!md;
			}
			else
			{ 
				var mezzo:Mezzo=md.mezzo;
				while(!mezzo.to.startsWith(id.substring(1)))
				{
					mezzo.inc;
				}
				var dove=mezzo.to;
				if(dove.contains("_1"))
					dove=dove.substring(id.length,dove.indexOf("_"));
				else	
					dove=dove.substring(id.length);
				corsiaIn(dove.toInt-1)!mezzo;		
			}
		}
	}

	def deviaPersona (pd:personaDeviata) : Unit =
	{
		var stop=false;
		if(pd.trattiDaFare.size==0 && pd.trattiFatti.size==0)
		{
			var go=true;
			val p: Persona=pd.persona;
			var arrivato=p.next;
			var percorso=p.percorso;
			arrivato+=1;
			while( percorso(arrivato)!="R" && !percorso(arrivato).contains("F") && percorso(arrivato)!="FINE" && percorso(arrivato)!="X" && percorso(arrivato).startsWith(p.toZona) )
			{
				arrivato+=1;
			}
			if(percorso(arrivato)=="R" || percorso(arrivato).contains("F") || percorso(arrivato)=="FINE" || percorso(arrivato)=="X" || percorso(arrivato)=="R")
			{
				println("La zona irraggiungibile è importante!");
				go=false;
				stop=true;
				self!FermaTutto;
			}
			if(go)
			{
				//Prendo la zona dopo prossimaZona
				var next:String=p.to2Zona;
				//percorso minimo
				val zonaCaduta:String="Z"+p.toZona;
				//Avviso tutte le zone che una è caduta. Se non lo faccio, potrebbero verificarsi dei lup (io calcolo un percorso, quello dopo ne calcola un altro e lo rimanda a me)
				grafo find zonaCaduta match 
				{
				    	case Some(i) => avvisoZonaCaduta(zonaCaduta); grafo-=zonaCaduta; zoneCadute+=zonaCaduta;
					case None => println("Non devo togliere il nodo "+zonaCaduta+" perché non c'è già più.");
				}
				val gr=grafo; //Non vuole la variabile grafo
				def n(outer: String): gr.NodeT = gr get outer
				println("Deviazione persona da "+id+" a Z"+next);
				try 
				{
					val spO = n(id) shortestPathTo n("Z"+next); //ECCEZIONE java.util.NoSuchElementException se non lo trova
					val sp = spO.get;
					var listaZone:String=sp.nodes.toString.substring(6)
					listaZone=listaZone.substring(0, listaZone.size-1)
					var abz=ArrayBuffer[String]();
					abz++=listaZone.split(", ");
					//Creo lista in personaDeviata
					pd.trattiDaFare=abz;
				}
				catch
				{
					case e: NoSuchElementException => println("La zona Z"+next+" è irraggiungibile!"); stop=true; self!FermaTutto;
				}	
			}
		}
		if(!stop)
		{
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
				if(marciapiede.startsWith("0"))
					marciapiede=marciapiede.substring(1); //Tolgo lo 0 all'inizio altrimenti in incrocio ciclerà
				pd.percorso(1)+=marciapiede; 
				marciapiedeIn(pd.percorso(0).substring(id.length+1).toInt-1)!pd;
			}
			else
			{ 
				var pedone:Persona=pd.persona;
				while(!pedone.to.startsWith(id.substring(1)))
				{
					pedone.inc;
				}
				var dove=pedone.to;
				dove=dove.substring(id.length+1);
				marciapiedeIn(dove.toInt-1)!pedone;		
			}
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


		println("\n\n"+grafo+"\n\n");


	}

	def aggiornaGrafo(iz:String): Unit =
	{
		grafo += iz;	
		for(x<-0 to listeVicini.size-1)
		{
			val l=listeVicini(x);
			if(l(0)==iz)
			{
				for(y<-1 to l.size-1)
				{	
					if(l(y)!="null")
						grafo += (l(0)~l(y));
				}
			}
		}

		println("\n\n"+grafo+"\n\n");

	}

	def chiediVicini: Unit =
	{
		for (x<- 0 to members.size-1)
		{		
			context.system.actorSelection(RootActorPath(members(x).address)/"user"/"Map")!new ChiediVicini(id);
		}
	}

	def avvisoZonaCaduta(idz:String): Unit =
	{
		for (x<- 0 to members.size-1)
		{		
			context.system.actorSelection(RootActorPath(members(x).address)/"user"/"Map")!new ZonaCaduta(idz);
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
			if( (nomiZone(x)==is) || (zoneCadute.contains(is) && nomiZone2(x)==is) )
			{


				if(zoneCadute.contains(is))
				{
					println("\n\n"+is+" è tornata UP\n\n");
					for (x<- 0 to members.size-1)
					{		
						context.system.actorSelection(RootActorPath(members(x).address)/"user"/"Map")!new ZonaTornata(is);
					}					
				}
				else
				{
					zoneVicine(x)=z;
					nomiZone(x)="null";
					if (contZone == -1) 
						contZone+=1;
					contZone+=1;
				}
			}
		}
	}

	def inviaPedone(p: Persona): Unit =
	{
		p.incZona;
		var dove=p.to;
		dove=dove.substring(id.length+1);
		marciapiedeIn(dove.toInt-1)!p;
	}

	def inviaMezzo(m: Mezzo): Unit =
	{
		m.incZona;
		var dove=m.to;
		val pos=dove.indexOf("_"); 
		if (pos>=0)
			dove=dove.substring(id.length, pos);
		else
			dove=dove.substring(id.length);
		corsiaIn(dove.toInt-1)!m;
	}

	def start: Unit  =
	{
		var act=context.system.actorOf(Props(classOf[MapActor],id,self), "Map");
		val cluster = Cluster(context.system);
		//Leggo il file XML
		var nome="/home/marco/Scrivania/ProvaSCD/src/main/scala/com/knoldus/Zone.xml";
		xml=XML.loadFile(nome);		
		leggiVicini;
		//Creo i pezzi
		self!Vicini;	
	}

	def registra(member:Member): Unit =
	{	
		if(zoneCadute.size==0)
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
		else
		{

			
			println("\n\nUn vicino è tornato UP!\n\n");


			for(x<- 0 to zoneCadute.size-1)
			{
				var nome:String=zoneCadute(x);
				if (nome!="null" && !RootActorPath(member.address).toString.contains(port))
				{
					context.system.actorSelection(RootActorPath(member.address)/"user"/nome)!nome.substring(1).toInt;	 	
				}
			}
		}
	}

	def spostamenti: Unit =
	{
		var x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;} 

		var arrZone=(xml \ "Zone" \ "Zona" );
		for(z <- 0 to arrZone.size-1)
		{
			var idz:String= arrZone(z)\"@id" text;
			if(idz==id)
			{
				//Creo prima i pedoni
				var spostamenti = (arrZone(z) \\ "Pedoni" \ "Pedone");
				if(spostamenti.size!=0)
				{
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
				}
				//Poi le automobili
				spostamenti = (arrZone(z) \\ "Automobili" \ "Automobile");
				if(spostamenti.size!=0)
				{
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
				}
				//Ed infine gli autobus
				spostamenti = (arrZone(z) \\ "Autobus" \ "Bus");
				if(spostamenti.size!=0)
				{
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

			}	
		}		
	}

	def leggiVicini: Unit =
	{
		var arrZone=(xml \ "Zone" \ "Zona" );
		for(z <- 0 to arrZone.size-1)
		{
			var idz:String= arrZone(z)\"@id" text;
			if(idz==id)
			{
				var vicini = (arrZone(z) \\ "Vicini" \ "Vicino");
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
		}
	}

	def creaStrade: Unit =
	{
		var dest=new containerDestinazione(destinazione);

		var arrZone=(xml \ "Zone" \ "Zona" );
		for(z <- 0 to arrZone.size-1)
		{
			var idz:String= arrZone(z)\"@id" text;
			if(idz==id)
			{
				//CORSIE ENTRANTI
				var strade = (arrZone(z) \\ "Strade" \ "StradeIn" \ "StradaIn");
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
				strade = (arrZone(z) \\ "StradeCont" \ "StradeInCont" \ "StradaInCont");
				//Imposto le destinazioni delle strade
				for(x <- 0 to strade.size-1)
				{
					var attr:String= strade(x)\"@id" text;
					var pos= attr.substring(id.length, id.length+1).toInt; //1I1_2
					corsiaInCont(pos-1)=context.actorOf(Props(classOf[Corsia], self), attr);
					corsiaInCont(pos-1)!attr;
				}
				if(corsiaIn.size!=0 && corsiaIn.size!=2)
				{
					for(x <- 0 to (corsiaIn.size-1))
					{
						corsiaIn(x)!dest;
						if(corsiaInCont(x)!=null)
							corsiaIn(x)!corsiaInCont(x);
						else if(corsiaInCont(x)==null && incrocio!=null)
							corsiaIn(x)!incrocio;
					}
				}
				if(corsiaInCont.size!=0 && corsiaIn.size!=2)
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
				strade = (arrZone(z) \\ "Strade" \ "StradeOut" \ "StradaOut");
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
				strade = (arrZone(z) \\ "StradeCont" \ "StradeOutCont" \ "StradaOutCont");
				for(x <- 0 to strade.size-1)
				{
					var attr:String= strade(x)\"@id" text;
					var pos= attr.substring(id.length, id.length+1).toInt; //1O3_2
					corsiaOutCont(pos-1)=context.actorOf(Props(classOf[Corsia], self), attr);
					corsiaOutCont(pos-1)!attr;
				}	
				//Imposto le destinazioni delle strade uscenti
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

				if(corsiaIn.size==2)
				{
					for(x <- 0 to 1)
					{
						corsiaIn(x)!dest;
						if(corsiaInCont(x)!=null)
						{
							corsiaIn(x)!corsiaInCont(x);
							corsiaInCont(x)!corsiaOut(corsiaIn.size-1-x);
						}
						else
							corsiaIn(x)!corsiaOut(corsiaIn.size-1-x);
					}
				}
			}
		}
	}

	def creaMarciapiedi: Unit =
	{
		var dest=new containerDestinazione(destinazione);
		var arrZone=(xml \ "Zone" \ "Zona");
		for(z <- 0 to arrZone.size-1)
		{
			var idz:String= arrZone(z)\"@id" text;
			if(idz==id)
			{
				//MARCIAPIEDI ENTRANTI
				var marciapiedi = (arrZone(z) \\ "Marciapiedi" \ "MarciapiediIn" \ "MarciapiedeIn");
				if(marciapiedi.size!=0)
				{
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
				}
				//MARCIAPIEDI USCENTI
				marciapiedi = (arrZone(z) \\ "Marciapiedi" \ "MarciapiediOut" \ "MarciapiedeOut");
				if(marciapiedi.size!=0)
				{
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
				//Se ho solo due strade, vuol dire che non mi serve l'incrocio e imposto le destinazioni a mano
				if(marciapiedeIn.size==2)
				{
				
					marciapiedeIn(0)!ArrayBuffer(marciapiedeOut(1), marciapiedeOut(1));
					marciapiedeIn(1)!ArrayBuffer(marciapiedeOut(0), marciapiedeOut(0));
					marciapiedeOut(0)!new containerZona(zoneVicine(0));
					marciapiedeOut(1)!new containerZona(zoneVicine(1));
				}
				else
				{
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

			}
		}
	}

	def creaIncrocio: Unit =
	{
		//Creo l'incrocio
		var tratti = ArrayBuffer[ActorRef]();
		var strisce = ArrayBuffer[ActorRef]();

		var arrZone=(xml \ "Zone" \ "Zona" );
		for(z <- 0 to arrZone.size-1)
		{
			var idz:String= arrZone(z)\"@id" text;
			if(idz==id)
			{
				//TRATTI
				var tratto = (arrZone(z) \\ "Incrocio" \ "Tratti" \ "Tratto");
				if(tratto.size!=0)
				{
					//Creo l'incrocio
					incrocio=context.actorOf(Props(classOf[Incrocio], id).withDispatcher("prio-dispatcher3"), "Incrocio"+id);
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
				var striscia = (arrZone(z) \\ "Incrocio" \ "StriscePedonali" \ "StrisciaPedonale");
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
	}
}

class ZonaPriorityActorMailbox(settings: ActorSystem.Settings, config: Config) extends UnboundedStablePriorityMailbox(
PriorityGenerator 
{
	//"Creazione" vicini
	case Start => 0
	case num:Int => 1
	case Vicini => 3
	//Creazione e gestione grafo	
	case FermaTutto => 3
	case Grafo => 4
	case c: ChiediVicini => 4
	case c: CostruzioneGrafo => 4
	case c: ZonaTornata => 4
	case c: ZonaCaduta => 4
	case Blocco2 => 5
	//Creazione componenti
	case Incroci => 6
	case Strade => 6
	case Marciapiedi => 6
	case AggiornaIncrocio => 6	
	case Movimenti => 6
	//Spostamenti
	case m:mezzoDeviato => 7
	case p:personaDeviata => 7
	case m:Mezzo => 7
	case p:Persona => 7	
	case m:containerMezzoDeviato => 7
	case p:containerPersonaDeviata => 7
	case _ => 2
})
