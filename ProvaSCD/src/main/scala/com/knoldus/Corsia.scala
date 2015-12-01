package com.knoldus

import scala.concurrent.Await
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

import akka.actor.Actor
import akka.actor.ActorRef
import java.util.concurrent.TimeoutException

class Corsia (z:ActorRef) extends Actor 
{	
	var nextActor:ActorRef=null;
	var fermata:ActorRef=null;
	var id="";
	var destinazione:ActorRef = null;
	var zona:ActorRef=z;
	val maxTentativi: Int = 4;
	
  	override def receive: Actor.Receive = 
	{
		case v:containerZona => nextActor=v.zona;
		case m:String => start(m);
		case f:containerFermata => creaFermata(f);
		case z:containerDestinazione => destinazione=z.destinazione;
		case m:ActorRef => nextReceived(m);	
		case m:mezzoDeviato =>	gestisciD (m);
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
				zona!new containerMezzoDeviato(new mezzoDeviato(m.mezzo.id, m.mezzo));
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
								zona!new containerMezzoDeviato(new mezzoDeviato(m.id, m));
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
