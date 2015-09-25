package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import scala.collection.mutable.ArrayBuffer

class Zona extends Actor 
{
	var corsiaIn = ArrayBuffer[ActorRef]();
	var corsiaOut = ArrayBuffer[ActorRef]();
	var incrocio: ActorRef=null;
  	override def receive: Actor.Receive = 
	{
		case "Start" => start;
	}

	def start: Unit  =
	{
		println("Eccomi");
		//INCROCIO
		var tratti = ArrayBuffer[ActorRef]();
		incrocio=context.actorOf(Props[Incrocio], "1Incrocio");
		incrocio!"Start";
		tratti+=context.actorOf(Props[Tratto], "1T1");
		tratti(0)!"1T1";
		tratti+=context.actorOf(Props[Tratto], "1T2");
		tratti(1)!"1T2";
		tratti+=context.actorOf(Props[Tratto], "1T3");
		tratti(2)!"1T3";
		tratti+=context.actorOf(Props[Tratto], "1T5");
		tratti(3)!"1T4";
		for(x <- 0 to (tratti.size-1))
		{
			incrocio!tratti(x);
		}
		incrocio!"Manda";
		//IN
		corsiaIn+=context.actorOf(Props[Corsia], "1I1");
		corsiaIn(0)!"1I1";
		corsiaIn+=context.actorOf(Props[Corsia], "1I2");
		corsiaIn(1)!"1I2";
		corsiaIn+=context.actorOf(Props[Corsia], "1I3");
		corsiaIn(2)!"1I3";
		corsiaIn+=context.actorOf(Props[Corsia], "1I4");
		corsiaIn(3)!"1I4";
		for(x <- 0 to (corsiaIn.size-1))
		{
			corsiaIn(x)!incrocio;
		}
		//OUT
		corsiaOut+=context.actorOf(Props[Corsia], "1O1");
		corsiaOut(0)!"1O1";
		corsiaOut+=context.actorOf(Props[Corsia], "1O2");
		corsiaOut(1)!"1O2";
		corsiaOut+=context.actorOf(Props[Corsia], "1O3");
		corsiaOut(2)!"1O3";
		corsiaOut+=context.actorOf(Props[Corsia], "1O4");
		corsiaOut(3)!"1O4";
		incrocio!corsiaOut;
		//DA TOGLIERE		
		for(x <- 0 to (corsiaOut.size-1))
		{
			corsiaOut(x)!incrocio;
		}
		//DA TOGLIERE	
		//var mzz= new Auto ("M0", ArrayBuffer("1I1", "1O4", "1O3", "X") , new Pedone ("P1", ArrayBuffer("")));
		var mzz= new Auto ("M0", ArrayBuffer("1I1", "1O2", "X") , new Pedone ("P1", ArrayBuffer("")));
		corsiaIn(0)!mzz;
	}
}
