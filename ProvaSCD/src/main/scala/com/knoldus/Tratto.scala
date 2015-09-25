package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import scala.collection.mutable.ArrayBuffer

class Tratto extends Actor 
{	
	var tratti = ArrayBuffer[ActorRef]();
	var nextActor:ActorRef=null;
	var id="";

  	override def receive: Actor.Receive = 
	{
		case "AA" => println("Contattato da incrocio");
		case m:String => start(m);
		case m:ActorRef => nextReceived(m);	
		case m:ArrayBuffer[ActorRef] => trattiRicevuti(m);
		case m:Mezzo =>	gestisci (m);		   
	}

	def start (m:String): Unit  =
	{
		id=m;
	}

	def trattiRicevuti (m:ArrayBuffer[ActorRef]): Unit  =
	{
		tratti=m;
	}

	def nextReceived (m:ActorRef): Unit  =
	{
		nextActor=m;
	}

	def gestisci (m:Mezzo): Unit =
	{
		println("Mezzo "+m.id+" arrivato al tratto "+id);
		val dove=m.to;
		val io=id.substring(2);
		val to=dove.substring(2);
		if(io==to)
			nextActor!m;
		else
			tratti(to.toInt-1)!m;
	}	
}
