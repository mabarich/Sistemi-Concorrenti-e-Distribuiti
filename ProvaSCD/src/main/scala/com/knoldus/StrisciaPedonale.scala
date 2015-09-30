package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import scala.collection.mutable.ArrayBuffer

class StrisciaPedonale extends Actor 
{	
	var marciapiede:ActorRef=null;
	var nextS:ActorRef=null;
	var id="";

  	override def receive: Actor.Receive = 
	{
		case "AA" => println("Contattato da incrocio");
		case p:String => start(p);
		case p:ArrayBuffer[ActorRef] => datiRicevuti(p);
		case p:Persona => gestisci (p);		   
	}
	
	//Setto l'id
	def start (p:String): Unit  =
	{
		id=p;
	}

	//Imposta il destinatario di tutti i messaggi (uno è l'uscita e l'altro è la striscia pedonale successiva)
	def datiRicevuti (p:ArrayBuffer[ActorRef]): Unit  =
	{
		marciapiede=p(0);
		nextS=p(1);
	}

	def gestisci (p:Persona): Unit =
	{
		println("Persona "+p.id+" arrivata alla striscia "+id);
		val dove=p.to;
		val io=id.substring(2); //1S3
		val to=dove.substring(3); //1MO3
		if(io==to)
			marciapiede!p;
		else
			nextS!p;
	}	
}
