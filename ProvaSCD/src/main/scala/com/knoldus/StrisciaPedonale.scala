package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import scala.collection.mutable.ArrayBuffer
import akka.dispatch.UnboundedStablePriorityMailbox
import com.typesafe.config.Config
import akka.dispatch.PriorityGenerator
import akka.actor.ActorSystem

class StrisciaPedonale extends Actor 
{	
	var marciapiede:ActorRef=null;
	var nextS:ActorRef=null;
	var id="";
	var pedoni = ArrayBuffer[Persona]();

	//Semaforo verde -> invia
  	override def receive: Actor.Receive = 
	{
		case p:String => start(p);
		case p:ArrayBuffer[ActorRef] => datiRicevuti(p);
		case p:Persona => gestisci (p);
		case p:containerPersona => gestisci (p);
		case p:personaPiuPriorita => gestisci (p.persona);
		case Rosso =>	context.become(redReceiver, false);		   
	}

	//Semaforo rosso -> accoda
	def redReceiver: Receive = 
	{
		case p:String => start(p);
		case p:ArrayBuffer[ActorRef] => datiRicevuti(p);
		case p:Persona => pedoni+=p; println("Persona "+p.id+" in coda alla striscia "+id);	
		case p:containerPersona => gestisci (p);
		case Verde =>	context.unbecome();	
				while(!pedoni.isEmpty)
				{
					var p:Persona=pedoni(0);
					pedoni.remove(0);
					self!new personaPiuPriorita(p);
				}	   
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

	def gestisci (cp:containerPersona): Unit =
	{
		var p:Persona=cp.persona;
		println("Persona "+p.id+" arrivata alla striscia "+id);
		val dove=p.to;
		val io=id.substring(2); //1S3
		val to=dove.substring(3); //1MO3
		if(io==to)
			marciapiede!p;
		else
			self!p;
	}
}

class StrisciaPriorityActorMailbox(settings: ActorSystem.Settings, config: Config) extends UnboundedStablePriorityMailbox(
PriorityGenerator 
{
	case Verde => 0
	case Rosso => 0
	case m: personaPiuPriorita => 1
	case m: Persona => 2
	case m: containerPersona => 2
	case _ => 3
})
