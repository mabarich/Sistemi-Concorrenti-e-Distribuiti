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
	var destinazione:ActorRef = null;

  	override def receive: Actor.Receive = 
	{
		case p:Int => numMarciapiedi=p;
		case "AA" => println("Contattato da incrocio");
		case f:containerFermata => creaFermata(f);	
		case z:containerDestinazione => destinazione=z.destinazione;
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
			if (dove!="R")
			{
				if (dove=="FINE")
				{
					println("Pedone "+p.id+" arrivato a destinazione sul marciapiede "+id+" e morirà");
				}
				else
				{
					if(dove.indexOf("F") != -1)
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
			}
			else
			{
				p.reset;
				destinazione!p;
				println("Pedone "+p.id+" arrivato a destinazione sul marciapiede "+id+". Il pedone ha completato il giro e ripartirà.");
			}
		}
		else
		{
			p.inc;
			destinazione!p;
			println("Persona "+p.id+" arrivato a destinazione sul marciapiede "+id);
		}
	}	
}
