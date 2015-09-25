package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef

class Marciapiede extends Actor 
{	
	var nextActor:ActorRef=null;
	var id="";

  	override def receive: Actor.Receive = 
	{
		case "AA" => println("Contattato da incrocio");
		case p:String => start(p);
		case p:ActorRef => nextReceived(p);	
		case p:Persona => gestisci (p);		   
	}

	def start (p:String): Unit  =
	{
		id=p;
		//println("Marciapiede "+p);
	}

	def nextReceived (p:ActorRef): Unit  =
	{
		nextActor=p;
		//println(id+" ha ricevuto un attore.");
		nextActor!id;
	}

	def gestisci (p:Persona): Unit =
	{
		println("Persona "+p.id+" arrivato sul marciapiede "+id);
		p.inc;
		val dove=p.to;
		if(dove!="X")
			nextActor!p;
		else
			println("Persona "+p.id+" arrivato a destinazione sul marciapiede "+id);
	}	
}
