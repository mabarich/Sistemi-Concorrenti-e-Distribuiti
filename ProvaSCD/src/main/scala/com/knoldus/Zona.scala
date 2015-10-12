package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import scala.collection.mutable.ArrayBuffer
import scala.xml.XML

class Zona extends Actor 
{
	var corsiaIn = ArrayBuffer[ActorRef]();
	var corsiaOut = ArrayBuffer[ActorRef]();
	var corsiaInCont = ArrayBuffer[ActorRef]();
	var corsiaOutCont = ArrayBuffer[ActorRef]();
	var marciapiedeIn = ArrayBuffer[ActorRef]();
	var marciapiedeOut = ArrayBuffer[ActorRef]();
	var fermateIn = ArrayBuffer[ActorRef]();
	var fermateOut = ArrayBuffer[ActorRef]();
	var destinazione: ActorRef =null;
	var incrocio: ActorRef=null;
	var id:String="";

  	override def receive: Actor.Receive = 
	{
		case "Start" => start;
		case p:Persona => inviaPedone(p);
		case m:Mezzo => inviaMezzo(m);
	}

	def inviaPedone(p: Persona): Unit =
	{
		var dove=p.to;
		dove=dove.substring(3);
		marciapiedeIn(dove.toInt-1)!p;
	}

	def inviaMezzo(m: Mezzo): Unit =
	{
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
		println("Eccomi");
		//Creo la destinazione
		destinazione=context.actorOf(Props[Destinazione], "Destinazione");
		destinazione!self;
		//Creo l'incrocio
		incrocio=context.actorOf(Props[Incrocio].withDispatcher("prio-dispatcher3"), "Incrocio"+id);
		incrocio!"Start";
		//Leggo il file XML
		var xml=XML.loadFile("/home/marco/Scrivania/ProvaSCD/src/main/scala/com/knoldus/Z1.xml");
		id=xml\"Zona"\"@id" text;
		//Creo i pezzi
		creaIncrocio(xml);
		creaStrade(xml);
		creaMarciapiedi(xml);
		incrocio!new containerActrf(corsiaOut, marciapiedeOut);
		incrocio!"Manda";	
		//Avvio mezzi e pedoni	
		messaggi;			
	}

	def messaggi: Unit =
	{
		var x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;} 

		var prsn= new Pedone ("P0", ArrayBuffer("1MI1", "FA0", "1FO3", "1MO3", "X", "1MI3", "1MO1", "R"));
		//var prsn= new Pedone ("P0", ArrayBuffer("1MI1", "1MO3", "X", "1MI3", "1MO1", "R"));
		     self!prsn;

		x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;}

		//var mzz= new Auto ("M0", ArrayBuffer("1I1_1", "1I1_2", "1O2", "X", "1I2", "1O1", "R") , new Pedone ("P1", ArrayBuffer("")));
		//var mzz= new Autobus ("A0", ArrayBuffer("1I1_1", "F", "1I1_2", "1O2", "X"));
		var mzz= new Autobus ("A0", ArrayBuffer("1I1_1", "F", "1I1_2", "1O3_1", "F", "1O3_2","R"));
		//var mzz= new Autobus ("A0", ArrayBuffer("1I2", "1I1_2", "1I1_1", "X"));
		//var mzz= new Auto ("M0", ArrayBuffer("1I2", "1O1", "X") , new Pedone ("P1", ArrayBuffer("")));
		self!mzz;
	}

	def creaStrade(xml:scala.xml.Elem): Unit =
	{
		var dest=new containerDestinazione(destinazione);
		//CORSIE ENTRANTI
		var strade = (xml \ "Zona" \ "Strade" \ "StradeIn" \ "StradaIn");
		for(x <- 0 to strade.size-1)
		{
			fermateIn+=null;
			var attr:String= strade(x)\"@id" text;
			corsiaIn+=context.actorOf(Props[Corsia], attr);
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
			corsiaInCont(pos-1)=context.actorOf(Props[Corsia], attr);
			corsiaInCont(pos-1)!attr;
		}
		if(corsiaIn.size!=0)
		{
			for(x <- 0 to (corsiaIn.size-1))
			{
				corsiaIn(x)!dest;
				if(corsiaInCont(x)!=null)
					corsiaIn(x)!corsiaInCont(x);
				else
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
			corsiaOut+=context.actorOf(Props[Corsia], attr);
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
			corsiaOutCont(pos-1)=context.actorOf(Props[Corsia], attr);
			corsiaOutCont(pos-1)!attr;
		}	
		//Imposto le destinazioni delle strade
		for(x <- 0 to (corsiaOut.size-1))
		{
			corsiaOut(x)!dest;
			if(corsiaOutCont(x)!=null)
				corsiaOut(x)!corsiaOutCont(x);
			else
				corsiaOut(x)!incrocio;
		}		
		for(x <- 0 to (corsiaOutCont.size-1))
		{
			if(corsiaOutCont(x)!=null)
				corsiaOutCont(x)!dest;
				//corsiaOutCont(x)!incrocio;
			//DEVO METTERCI UNA ZONA
		}
	}

	def creaMarciapiedi(xml:scala.xml.Elem): Unit =
	{
		var dest=new containerDestinazione(destinazione);
		//MARCIAPIEDI ENTRANTI
		var marciapiedi = (xml \ "Zona" \ "Marciapiedi" \ "MarciapiediIn" \ "MarciapiedeIn");
		for(x <- 0 to marciapiedi.size-1)
		{
			var attr:String= marciapiedi(x)\"@id" text;
			marciapiedeIn+=context.actorOf(Props[Marciapiede], attr);
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
			marciapiedeOut+=context.actorOf(Props[Marciapiede], attr);
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
			val ind=x+1;
			marciapiedeIn(x)!ArrayBuffer(incrocio, marciapiedeOut((4-ind)%4));
		}
	}

	def creaIncrocio(xml:scala.xml.Elem): Unit =
	{
		//Creo l'incrocio
		var tratti = ArrayBuffer[ActorRef]();
		var strisce = ArrayBuffer[ActorRef]();

		//TRATTI
		var tratto = (xml \ "Zona" \ "Incrocio" \ "Tratti" \ "Tratto");
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

		//STRISCE
		var striscia = (xml \ "Zona" \ "Incrocio" \ "StriscePedonali" \ "StrisciaPedonale");
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
