package com.knoldus

import scala.concurrent.Await
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

import akka.actor.Actor
import akka.actor.ActorRef
import java.util.concurrent.TimeoutException

import akka.dispatch.UnboundedStablePriorityMailbox
import com.typesafe.config.Config
import akka.dispatch.PriorityGenerator
import akka.actor.ActorSystem

class Corsia (z:ActorRef, i: String, f: ActorRef, d: ActorRef) extends Actor 
{	
	var nextActor:ActorRef=null;
	var fermata:ActorRef=f;
	var id=i;
	var destinazione:ActorRef = d;
	var zona:ActorRef=z;
	val maxTentativi: Int = 4;
	var nextDown: Boolean= false;
	
  	override def receive: Actor.Receive = 
	{
		case v:containerZona => nextActor=v.zona;
		case m:ActorRef => nextReceived(m);	
		case m:mezzoDeviato =>	gestisciD (m);
		case m:Mezzo =>	gestisci (m);		   
	}

	//Imposta il destinatario di tutti i messaggi
	def nextReceived (m:ActorRef): Unit  =
	{
		nextActor=m;
	}

	def gestisciD (m:mezzoDeviato): Unit =
	{
		println("Mezzo (deviato) "+m.id+" arrivato sulla strada "+id);
		//Prendo il pezzo successivo della stringa (inc fa ++ e to mi prende quello dopo ancora)
		val dove=m.to;
		m.inc;
		if(id.contains("I")  || id.contains("_1"))
			nextActor!m;
		//Uso il Future solo se mando ad altre zone, ovvero se ho una corsia uscente
		else
		{
			implicit val timeout = Timeout(10 seconds)
			var result: Boolean=false;
			var tentativi: Int=0;
			while(!result && tentativi<maxTentativi)
			{
				val future = nextActor ? m; 
				val bool = Await.result(future, timeout.duration).asInstanceOf[Boolean];
				if (bool)
					result=true;
				else
					tentativi+=1;
			}
			if(tentativi==maxTentativi)
			{
				println("La zona non risponde. Il pedone (deviato) dovrà seguire un'altra strada.");
				nextDown=true;
				zona!new containerMezzoDeviato(new mezzoDeviato(m.mezzo.id, m.mezzo));
			}
			else if(nextDown)
			{
				nextDown=false;
				zona!new CorreggiGrafo("Z"+m.zona);
			}
		}

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
			if (dove!="R")
			{
				if(dove=="F")
					fermata!m;
				else
				{
					if (dove=="FINE")
					{
						println("Mezzo "+m.id+" arrivato a destinazione sulla corsia "+id+" e morirà");
					}
					else
					{
						if(dove=="Riparti")
							m.next = -1;
						//nextActor!m;
						
						if(id.contains("I") || id.contains("_1"))
							nextActor!m;
						//Uso il Future solo se mando ad altre zone, ovvero se ho una corsia uscente
						else
						{
							implicit val timeout = Timeout(10 seconds)
							var result: Boolean=false;
							var tentativi: Int=0;
							while(!result && tentativi<maxTentativi)
							{
								try
								{
									val future = nextActor ? m; 
									val bool = Await.result(future, timeout.duration).asInstanceOf[Boolean];
									if (bool)
										result=true;
									else
										tentativi+=1;
								}
								catch
								{
									case e: TimeoutException => println("\n\nTimeout\n\n"); tentativi+=1;
								}
							}
							if(tentativi==maxTentativi)
							{
								println("La zona non risponde. Il mezzo dovrà seguire un'altra strada.");
								nextDown=true;
								zona!new containerMezzoDeviato(new mezzoDeviato(m.id, m));
							}
							else if(nextDown)
							{
								nextDown=false;
								zona!new CorreggiGrafo("Z"+m.toZona);
							}
						}

					}
				}
			}
			else
			{
				m.reset;
				destinazione!m;
				println("Mezzo "+m.id+" arrivato a destinazione sulla strada "+id+". Il mezzo ha completato il giro e ripartirà.");
			}
		}
		else
		{
			m.inc;
			m.incZona;
			m.incZona;
			destinazione!m;
			println("Mezzo "+m.id+" arrivato a destinazione sulla strada "+id);
		}
	}	
}

class CorsiaPriorityActorMailbox(settings: ActorSystem.Settings, config: Config) extends UnboundedStablePriorityMailbox(
PriorityGenerator 
{
	case m:mezzoDeviato => 1
	case m:Mezzo => 1
	case _ => 0
})
