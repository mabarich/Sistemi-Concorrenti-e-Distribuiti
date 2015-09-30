package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef

class Corsia extends Actor 
{	
	var nextActor:ActorRef=null;
	var fermata:ActorRef=null;
	var id="";

  	override def receive: Actor.Receive = 
	{
		case "AA" => println("Contattato da incrocio");
		case m:String => start(m);
		case f:containerFermata => creaFermata(f);
		case m:ActorRef => nextReceived(m);	
		case m:Mezzo =>	gestisci (m);		   
	}

	//Imposta l'id
	def start (m:String): Unit  =
	{
		id=m;
	}

	//Crea una fermata
	def creaFermata (f:containerFermata): Unit  =
	{
		fermata=f.fermata;
	}

	//Imposta il destinatario di tutti i messaggi
	def nextReceived (m:ActorRef): Unit  =
	{
		nextActor=m;
	}

	def gestisci (m:Mezzo): Unit =
	{
		println("Mezzo "+m.id+" arrivato sulla strada "+id);
		//Prendo il pezzo successivo della stringa (inc fa ++ e to mi prende quello dopo ancora)
		m.inc;
		val dove=m.to;
		//Se è X allora è arrivato a destinazione, altrimenti lo mando al prossimo attore
		if(dove!="X")
		{
			if(dove=="F")
				fermata!m;
			else
			{
				if(dove=="Riparti")
					m.next = -1;
				nextActor!m;
			}
		}
		else
			println("Mezzo "+m.id+" arrivato a destinazione sulla strada "+id);
	}	
}
