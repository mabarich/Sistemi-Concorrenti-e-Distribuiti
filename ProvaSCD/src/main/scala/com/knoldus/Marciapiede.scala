package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import scala.collection.mutable.ArrayBuffer

import scala.concurrent.Await
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

class Marciapiede (z:ActorRef) extends Actor 
{	
	var nextActor:ActorRef=null;
	var nextMarciapiede:ActorRef=null;
	var fermata:ActorRef=null;
	var id="";
	var numMarciapiedi=0;
	var destinazione:ActorRef = null;
	var zona:ActorRef=z;
	val maxTentativi: Int = 4;

  	override def receive: Actor.Receive = 
	{
		case v:containerZona => nextActor=v.zona;
		case p:Int => numMarciapiedi=p;
		case f:containerFermata => creaFermata(f);	
		case z:containerDestinazione => destinazione=z.destinazione;
		case p:String => start(p);
		case p:ArrayBuffer[ActorRef] => nextReceived(p);
		case p:personaDeviata => gestisciD (p);		
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

	def gestisciD (p:personaDeviata): Unit =
	{
		//Prendo il pezzo successivo della stringa (inc fa ++ e to mi prende quello dopo ancora)
		println("Persona (deviata) "+p.id+" arrivato sul marciapiede "+id);
		val dove=p.to;
		p.inc;
		//Se deve andare al marciapiede adiacente, non deve per forza passare per l'incrocio
		var dv=dove.substring(3).toInt;
		var io=id.substring(3).toInt;
		if (id.contains("MI") && ((io==1 && dv==numMarciapiedi) || (io!=1 && dv==io-1)))
		{
			nextMarciapiede!p;
		}						
		else
		{
			if(id.contains("I"))
				nextActor!p;
			//Uso il Future solo se mando ad altre zone, ovvero se ho una corsia uscente
			else
			{
				implicit val timeout = Timeout(10 seconds)
				var result: Boolean=false;
				var tentativi: Int=0;
				while(!result && tentativi<maxTentativi)
				{
					val future = nextActor ? p; 
					val bool = Await.result(future, timeout.duration).asInstanceOf[Boolean];
					if (bool)
						result=true;
					else
						tentativi+=1;
				}
				if(tentativi==maxTentativi)
				{
					println("La zona non risponde. Il pedone (deviato) dovrà seguire un'altra strada.");
					zona!new containerPersonaDeviata(new personaDeviata(p.id, p.persona));
				}
			}
		}
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
						if (id.contains("MI") && ((io==1 && dv==numMarciapiedi) || (io!=1 && dv==io-1)))
						{
							nextMarciapiede!p;
						}						
						else
						{
							if(id.contains("MI"))
								nextActor!p;
							//Uso il Future solo se mando ad altre zone, ovvero se ho un marciapiede uscente
							else
							{
								implicit val timeout = Timeout(15 seconds)
								var result: Boolean=false;
								var tentativi: Int=0;
								while(!result && tentativi<maxTentativi)
								{
									val future = nextActor ? p; 
									val bool = Await.result(future, timeout.duration).asInstanceOf[Boolean];
									if (bool)
										result=true;
									else
										tentativi+=1;
								}
								if(tentativi==maxTentativi)
								{
									println("La zona non risponde. Il pedone dovrà seguire un'altra strada.");
									zona!new containerPersonaDeviata(new personaDeviata(p.id, p));
								}
							}
						}
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
			p.incZona;
			p.incZona;
			destinazione!p;
			println("Persona "+p.id+" arrivato a destinazione sul marciapiede "+id);
		}
	}	
}
