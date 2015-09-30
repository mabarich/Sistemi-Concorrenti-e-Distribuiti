package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import scala.collection.mutable.ArrayBuffer

class Incrocio extends Actor 
{
	var corsiaOut = ArrayBuffer[ActorRef]();	
	var marciapiedeOut = ArrayBuffer[ActorRef]();
	var tratti = ArrayBuffer[ActorRef]();	
	var strisce = ArrayBuffer[ActorRef]();

  	override def receive: Actor.Receive = 
	{
		case m:containerActrf => ricevuto(m);
		case m:ArrayBuffer[ActorRef] => start(m);
		case m:Mezzo =>	gestisciMezzo (m);
		case p:Persona => gestisciPedone (p);
		case "Manda" =>	aggiornaTrattiEStrisce;
	}

	//Aggiunge un tratto ed una striscia
	def start (m:ArrayBuffer[ActorRef]): Unit  =
	{	
		tratti+=m(0);
		strisce+=m(1);
	}

	//Riceve la lista delle strade e dei marciapiedi in uscita
	def ricevuto (m:containerActrf): Unit  =
	{
		corsiaOut=m.buffCorsia;
		for(x <- 0 to (tratti.size-1))
		{
			tratti(x)!corsiaOut(x);
		}
		marciapiedeOut=m.buffMarciapiede;
	}

	//Passa ad ogni striscia e ad ogni tratto i mittenti per i loro messaggi
	def aggiornaTrattiEStrisce: Unit  =
	{
		//Passo ad ogni tratto la lista degli altri tratti
		for(x <- 0 to (tratti.size-1))
		{
			tratti(x)!tratti;
		}
		//Passo ad ogni striscia la coppia con il marciapiede in uscita e la striscia successiva
		for(x <- 1 to (strisce.size-1))
		{
			strisce(x)!ArrayBuffer(marciapiedeOut(x), strisce(x-1));
		}
		strisce(0)!ArrayBuffer(marciapiedeOut(0), strisce(strisce.size-1));
	}

	//Invia i mezzi ai tratti giusti
	def gestisciMezzo (m:Mezzo): Unit  =
	{
		println("Mezzo "+m.id+" arrivato all'incrocio");
		//Guardo su che tratto deve andare
		var dove=m.nxt;
		val pos=dove.indexOf("_"); 
		if (pos>=0)
			dove=dove.substring(2, pos);
		else
			dove=dove.substring(2);
		//Caso particolare del calcolo	
		val tratto=tratti.size-(dove.toInt%tratti.size);		
		if(tratto==tratti.size)
			tratti(0)!m;
		else
			tratti(tratto)!m;
	}

	//Invia i pedoni alle strisce giuste
	def gestisciPedone (p:Persona): Unit  =
	{
		println("Persona "+p.id+" arrivato all'incrocio");
		//Guardo su che striscia deve andare
		var dove=p.nxt;
		dove=dove.substring(3);
		val striscia=((dove.toInt+2)%strisce.size);	
		//Caso particolare del calcolo	
		if(striscia==2)
			strisce(strisce.size)!p;
		else
			strisce(striscia-1)!p;
	}
}
