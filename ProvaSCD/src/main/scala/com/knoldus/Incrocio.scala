package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import scala.collection.mutable.ArrayBuffer

class Incrocio extends Actor 
{
	var corsiaOut = ArrayBuffer[ActorRef]();	
	var tratti = ArrayBuffer[ActorRef]();	
	var strisce = ArrayBuffer[ActorRef]();

  	override def receive: Actor.Receive = 
	{
		case m:ActorRef => start(m);
		case m:ArrayBuffer[ActorRef] => corsieRicevute(m);
		case m:Mezzo =>	gestisciMezzo (m);
		case "Manda" =>	mandaTratti;
	}

	def start (m:ActorRef): Unit  =
	{
		tratti+=m;
	}

	def mandaTratti: Unit  =
	{
		for(x <- 0 to (tratti.size-1))
		{
			tratti(x)!tratti;
		}
	}

	def corsieRicevute (m:ArrayBuffer[ActorRef]): Unit  =
	{
		corsiaOut=m;
		for(x <- 0 to (tratti.size-1))
		{
			tratti(x)!corsiaOut(x);
		}
	}

	def gestisciMezzo (m:Mezzo): Unit  =
	{
		println("Mezzo "+m.id+" arrivato all'incrocio");
		var dove=m.nxt;
		val pos=dove.indexOf("_"); 
		if (pos>=0)
			dove=dove.substring(2, pos);
		else
			dove=dove.substring(2);
		val tratto=tratti.size-(dove.toInt%tratti.size);		
		if(tratto==tratti.size)
			tratti(0)!m;
		else
			tratti(tratto)!m;
	}
}
