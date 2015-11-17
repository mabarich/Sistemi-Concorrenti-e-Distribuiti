package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import scala.collection.mutable.ArrayBuffer
import akka.dispatch.UnboundedStablePriorityMailbox
import com.typesafe.config.Config
import akka.dispatch.PriorityGenerator
import akka.actor.ActorSystem

class Tratto extends Actor 
{	
	var tratti = ArrayBuffer[ActorRef]();
	var nextActor:ActorRef=null;
	var id="";
	var mezzi = ArrayBuffer[Mezzo]();

	//Semaforo verde -> invia
  	override def receive: Actor.Receive = 
	{
		case m:String => start(m); //Imposta l'ID
		case m:ActorRef => nextReceived(m); //Salva la corsia uscente	
		case m:ArrayBuffer[ActorRef] => trattiRicevuti(m); //Salva tutti i tratti dell'incrocio
		case m:Mezzo =>	gestisci (m); //Invia i mezzi perché il semaforo è verde
		case m:mezzoPiuPriorita => gestisci (m.mezzo);
		case m:containerMezzo => gestisci (m); //Invia i mezzi
		case Rosso =>	context.become(redReceiver, false);    
	}

	//Semaforo rosso -> accoda
	def redReceiver: Receive = 
	{
		case m:String => start(m);
		case m:ActorRef => nextReceived(m);	
		case m:ArrayBuffer[ActorRef] => trattiRicevuti(m);
		case m:Mezzo =>	mezzi+=m; println("Mezzo "+m.id+" in coda al tratto "+id); //Accoda i mezzi perché il semaforo è rosso
		case m:containerMezzo => gestisci (m); //Invia i mezzi. Devo separare i due casi perché se il semaforo è rosso e ricevo un mezzo da un altro tratto, questo sarebbe accodato invece che spedito
		case Verde =>	context.unbecome();
				while(!mezzi.isEmpty)
				{
					var m:Mezzo=mezzi(0);
					mezzi.remove(0);
					self!new mezzoPiuPriorita(m);
				}	   
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

	//Gestisce solo i mezzi provenienti dall'incrocio.
	def gestisci (m:Mezzo): Unit =
	{
		println("Mezzo "+m.id+" arrivato al tratto "+id);
		var dove=m.to;
		val pos=dove.indexOf("_"); 
		if (pos>=0)
			dove=dove.substring(2, pos);
		else
			dove=dove.substring(2);
		tratti(dove.toInt-1)!new containerMezzo(m);
	}

	//Se il semaforo fosse rosso e ricevessi un mezzo da un altro tratto, verrebbe accodato, quindi devo separare i mezzi che arrivano dall'incrocio da quelli che arrivano da altri tratti
	def gestisci (cm:containerMezzo): Unit =
	{
		var m:Mezzo=cm.mezzo;
		println("Mezzo "+m.id+" arrivato al tratto "+id);
		var dove=m.to;
		nextActor!m;
	}	
}

class TrattoPriorityActorMailbox(settings: ActorSystem.Settings, config: Config) extends UnboundedStablePriorityMailbox(
PriorityGenerator 
{
	case Verde => 0
	case Rosso => 0
	case m: mezzoPiuPriorita => 1
	case m: mezzoDeviatoPiuPriorita => 1
	case m: Mezzo => 2
	case m: mezzoDeviato => 2
	case m: containerMezzo => 2
	case m: containerMezzoDeviato => 2
	case _ => 3
})
