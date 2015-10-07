package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import scala.collection.mutable.ArrayBuffer

class Marciapiede extends Actor 
{	
	var nextActor:ActorRef=null;
	var nextMarciapiede:ActorRef=null;
	var fermata:ActorRef=null;
	var id="";
	var numMarciapiedi=0;

  	override def receive: Actor.Receive = 
	{
		case p:Int => numMarciapiedi=p;
		case "AA" => println("Contattato da incrocio");
		case f:containerFermata => creaFermata(f);	
		case p:String => start(p);
		case p:ArrayBuffer[ActorRef] => nextReceived(p);	
		case p:Persona => gestisci (p);		   
	}

	//Imposta l'id
	def start (p:String): Unit  =
	{
		id=p;
	}

	//Crea una fermata
	def creaFermata (f:containerFermata): Unit  =
	{
		fermata=f.fermata;
	}

	//Imposta il destinatario di tutti i messaggi (uno è l'incrocio e l'altro è il marciapiede successivo nel caso il pedone non debba andare per forza all'incrocio)
	def nextReceived (p:ArrayBuffer[ActorRef]): Unit  =
	{
		nextActor=p(0);
		nextMarciapiede=p(1);
	}

	def gestisci (p:Persona): Unit =
	{
		//Prendo il pezzo successivo della stringa (inc fa ++ e to mi prende quello dopo ancora)
		println("Persona "+p.id+" arrivato sul marciapiede "+id);
		p.inc;
		val dove=p.to;
		//Se è X allora è arrivato a destinazione, altrimenti lo mando al prossimo attore
		if(dove!="X")
		{
			if(dove=="F")
			{
				fermata!p;
			}
			else
			{			
				//Se deve andare al marciapiede adiacente, non deve per forza passare per l'incrocio
				var dv=dove.substring(3).toInt;
				var io=id.substring(3).toInt;
				if((numMarciapiedi-(io-1))!=dv)
					nextActor!p;
				else
					nextMarciapiede!p;
			}
		}
		else
			println("Persona "+p.id+" arrivato a destinazione sul marciapiede "+id);
	}	
}