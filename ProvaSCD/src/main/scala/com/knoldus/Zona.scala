package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import scala.collection.mutable.ArrayBuffer

class Zona extends Actor 
{
	var corsiaIn = ArrayBuffer[ActorRef]();
	var corsiaOut = ArrayBuffer[ActorRef]();
	var corsiaInCont = ArrayBuffer[ActorRef]();
	var corsiaOutCont = ArrayBuffer[ActorRef]();
	var marciapiedeIn = ArrayBuffer[ActorRef]();
	var marciapiedeOut = ArrayBuffer[ActorRef]();
	var destinazione: ActorRef =null;
	var incrocio: ActorRef=null;
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
		destinazione=context.actorOf(Props[Destinazione], "Destinazione");
		destinazione!self;
		inizializzazioneProva;	


		var x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;} 

		//var prsn= new Pedone ("P0", ArrayBuffer("1MI1", "FA0", "1FO3", "1MO3", "X", "1MI3", "1MO1", "R"));
		var prsn= new Pedone ("P0", ArrayBuffer("1MI1", "1MO3", "X", "1MI3", "1MO1", "R"));
		self!prsn;

		x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;}

		var mzz= new Auto ("M0", ArrayBuffer("1I1_1", "1I1_2", "1O2", "X", "1I2", "1O1", "R") , new Pedone ("P1", ArrayBuffer("")));
		//var mzz= new Autobus ("A0", ArrayBuffer("1I1_1", "F", "1I1_2", "1O2", "X"));
		//var mzz= new Autobus ("A0", ArrayBuffer("1I1_1", "F", "1I1_2", "1O3_1", "F", "1O3_2","X"));
		//var mzz= new Autobus ("A0", ArrayBuffer("1I2", "1I1_2", "1I1_1", "X"));
		//var mzz= new Auto ("M0", ArrayBuffer("1I2", "1O1", "X") , new Pedone ("P1", ArrayBuffer("")));
		self!mzz;		
	}

	def inizializzazioneProva: Unit =
	{
		var dest=new containerDestinazione(destinazione);
		//Creo l'incrocio
		var tratti = ArrayBuffer[ActorRef]();
		var strisce = ArrayBuffer[ActorRef]();
		incrocio=context.actorOf(Props[Incrocio].withDispatcher("prio-dispatcher3"), "1Incrocio");
		incrocio!"Start";
		//Creo tratti e strisce
		tratti+=context.actorOf(Props[Tratto].withDispatcher("prio-dispatcher"), "1T1");
		tratti(0)!"1T1";
		tratti(0)!Verde;
		tratti+=context.actorOf(Props[Tratto].withDispatcher("prio-dispatcher"), "1T2");
		tratti(1)!"1T2";
		tratti(1)!Rosso;
		tratti+=context.actorOf(Props[Tratto].withDispatcher("prio-dispatcher"), "1T3");
		tratti(2)!"1T3";
		tratti(2)!Verde;
		tratti+=context.actorOf(Props[Tratto].withDispatcher("prio-dispatcher"), "1T4");
		tratti(3)!"1T4";
		tratti(3)!Rosso;
		strisce+=context.actorOf(Props[StrisciaPedonale].withDispatcher("prio-dispatcher2"), "1S1");
		strisce(0)!"1S1";
		strisce(0)!Rosso;
		strisce+=context.actorOf(Props[StrisciaPedonale].withDispatcher("prio-dispatcher2"), "1S2");
		strisce(1)!"1S2";
		strisce(1)!Rosso;
		strisce+=context.actorOf(Props[StrisciaPedonale].withDispatcher("prio-dispatcher2"), "1S3");
		strisce(2)!"1S3";
		strisce(2)!Rosso;
		strisce+=context.actorOf(Props[StrisciaPedonale].withDispatcher("prio-dispatcher2"), "1S4");
		strisce(3)!"1S4";
		strisce(3)!Rosso;
		for(x <- 0 to (tratti.size-1))
		{
			incrocio!ArrayBuffer(tratti(x), strisce(x));
		}
		//STRADE IN
		corsiaIn+=context.actorOf(Props[Corsia], "1I1_1");
		corsiaIn(0)!"1I1_1";
		var fermata=context.actorOf(Props[Fermata], "1FI1");
		fermata!self;
		fermata!"1FI1";
		var contei=new containerFermata(fermata);
		corsiaIn(0)!contei;
		corsiaIn+=context.actorOf(Props[Corsia], "1I2");
		corsiaIn(1)!"1I2";
		corsiaIn+=context.actorOf(Props[Corsia], "1I3");
		corsiaIn(2)!"1I3";
		corsiaIn+=context.actorOf(Props[Corsia], "1I4");
		corsiaIn(3)!"1I4";
		//STRADE IN CONT
		corsiaInCont+=context.actorOf(Props[Corsia], "1I1_2");
		corsiaInCont(0)!"1I1_2";
		corsiaInCont+=null;
		corsiaInCont+=null;
		corsiaInCont+=null;
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
		//STRADE OUT
		corsiaOut+=context.actorOf(Props[Corsia], "1O1");
		corsiaOut(0)!"1O1";
		corsiaOut+=context.actorOf(Props[Corsia], "1O2");
		corsiaOut(1)!"1O2";
		corsiaOut+=context.actorOf(Props[Corsia], "1O3_1");
		corsiaOut(2)!"1O3_1";
		var fermata2=context.actorOf(Props[Fermata], "1FO3");
		fermata2!self;
		fermata2!"1FO3";
		var contei2=new containerFermata(fermata2);
		corsiaOut(2)!contei2;
		corsiaOut+=context.actorOf(Props[Corsia], "1O4");
		corsiaOut(3)!"1O4";
		
		corsiaOutCont+=null;
		corsiaOutCont+=null;
		corsiaOutCont+=context.actorOf(Props[Corsia], "1O3_2");
		corsiaOutCont(2)!"1O3_2";
		corsiaOutCont+=null;
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

		//MARCIAPIEDI IN
		marciapiedeIn+=context.actorOf(Props[Marciapiede], "1MI1");
		marciapiedeIn(0)!"1MI1";
		marciapiedeIn(0)!contei;
		marciapiedeIn+=context.actorOf(Props[Marciapiede], "1MI2");
		marciapiedeIn(1)!"1MI2";
		marciapiedeIn+=context.actorOf(Props[Marciapiede], "1MI3");
		marciapiedeIn(2)!"1MI3";
		marciapiedeIn+=context.actorOf(Props[Marciapiede], "1MI4");
		marciapiedeIn(3)!"1MI4";
		//MARCIAPIEDI OUT
		marciapiedeOut+=context.actorOf(Props[Marciapiede], "1MO1");
		marciapiedeOut(0)!"1MO1";
		marciapiedeOut+=context.actorOf(Props[Marciapiede], "1MO2");
		marciapiedeOut(1)!"1MO2";
		marciapiedeOut+=context.actorOf(Props[Marciapiede], "1MO3");
		marciapiedeOut(2)!"1MO3";
		marciapiedeOut(2)!contei2;
		marciapiedeOut+=context.actorOf(Props[Marciapiede], "1MO4");
		marciapiedeOut(3)!"1MO4";
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
		incrocio!new containerActrf(corsiaOut, marciapiedeOut);
		incrocio!"Manda";
	}
}
