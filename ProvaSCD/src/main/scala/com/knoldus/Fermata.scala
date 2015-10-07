package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import scala.collection.mutable.ArrayBuffer

class Fermata extends Actor 
{	
	var codaInEntrata = ArrayBuffer[Persona]();
	var codaInUscita = ArrayBuffer[Persona]();
	var zona: ActorRef=null;
	var id="";

  	override def receive: Actor.Receive = 
	{
		case z:ActorRef => zona=z;
		case m:String => start(m);
		case p:Persona => personaRicevuta(p);	
		case m:Autobus =>	corrieraRicevuta (m, sender);		   
	}

	def start (m:String): Unit  =
	{
		id=m;
	}

	def personaRicevuta (p:Persona): Unit  =
	{
		println("Persona "+p.id+" arrivata alla fermata"); 
		codaInEntrata+=p;
	}

	def corrieraRicevuta (m:Autobus, sender: ActorRef): Unit  =
	{
		println("Mezzo "+m.id+" arrivato alla fermata"); 
		var uscita:ArrayBuffer[Persona]=null;
		uscita=m.togli;
		var restanti= ArrayBuffer[Persona]();
		if(codaInEntrata.size!=0)
		{
			for (p <- 0 to codaInEntrata.size-1)
			{	
				var dove=codaInEntrata(p).to; //CONTROLLARE
				if (true) //DA CAMBIARE CONDIZIONE
				{
					m.aggiungi(codaInEntrata(p));
					println("Il passeggero "+codaInEntrata(p).id+" è salito sulla corriera");
				}
				else
					restanti+=codaInEntrata(p);
			}
		}
		codaInEntrata=restanti;
		if (uscita!=null && uscita.size!=0)
		{
			for (p <- 0 to uscita.size-1)
			{	
				var dove=uscita(p).to; //CONTROLLARE
				zona!p;
			}
		}
		sender!m;
	}	
}