package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef

class Corsia extends Actor 
{	
	var nextActor:ActorRef=null;
	var id="";

  	override def receive: Actor.Receive = 
	{
		case "AA" => println("Contattato da incrocio");
		case m:String => start(m);
		case m:ActorRef => nextReceived(m);	
		case m:Mezzo =>	gestisci (m);		   
	}

	def start (m:String): Unit  =
	{
		id=m;
		//println("Corsia "+m);
	}

	def nextReceived (m:ActorRef): Unit  =
	{
		nextActor=m;
		//println(id+" ha ricevuto un attore.");
		nextActor!id;
	}

	def gestisci (m:Mezzo): Unit =
	{
		println("Mezzo "+m.id+" arrivato sulla strada "+id);
		m.inc;
		val dove=m.to;
		if(dove!="X")
			nextActor!m;
		else
			println("Mezzo "+m.id+" arrivato a destinazione sulla strada "+id);
	}	
}
