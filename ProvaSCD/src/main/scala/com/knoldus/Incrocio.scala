package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import akka.dispatch.UnboundedStablePriorityMailbox
import com.typesafe.config.Config
import akka.dispatch.PriorityGenerator
import akka.actor.ActorSystem

class Incrocio (iz:String) extends Actor 
{
	var corsiaOut = ArrayBuffer[ActorRef]();	
	var marciapiedeOut = ArrayBuffer[ActorRef]();
	var tratti = ArrayBuffer[ActorRef]();	
	var strisce = ArrayBuffer[ActorRef]();
	var stato:Int=0;
	val idZona: String = iz;
	

	override def preStart: Unit = 
	{
		import context.dispatcher
		this.context.system.scheduler.schedule(0 seconds, 8 seconds, self, CambiaSemafori)
	}

  	override def receive: Actor.Receive = 
	{
		case m:containerActrf => ricevuto(m);
		case m:ArrayBuffer[ActorRef] => start(m);
		case m:mezzoDeviato =>	gestisciMezzoD (m);
		case p:personaDeviata => gestisciPedoneD (p);
		case m:Mezzo =>	gestisciMezzo (m);
		case p:Persona => gestisciPedone (p);
		case "Manda" =>	aggiornaTrattiEStrisce;
		case CambiaSemafori =>	cambiaSemafori;
	}

	//Cambia i semafori
	def cambiaSemafori: Unit  =
	{	
		stato match 
		{
			case 0 => for (p<-0 to strisce.size-1) //Pedoni rossi, semafori pari verdi, dispari rossi
				  	strisce(p)!Rosso;
				  for (m<-0 to tratti.size-1)
				  {
				  	if(m%2==0)
				 		tratti(m)!Verde;
				 	else
				 		tratti(m)!Rosso;
				  }
				 println("Strisce: rosso; Tratti pari: verde; Tratti dispari: rosso");
			case 1 => for (m<-0 to tratti.size-1) //Semafori rossi, pedoni verdi
				 	tratti(m)!Rosso;
				  for (p<-0 to strisce.size-1)
					strisce(p)!Verde;
				  println("Strisce: verde; Tratti rosso");
			case 2 => for (p<-0 to strisce.size-1) //Pedoni rossi, semafori dispari verdi, pari rossi
				 	strisce(p)!Rosso;
				  for (m<-0 to tratti.size-1)
				  {
				 	if(m%2!=0)
						tratti(m)!Verde;
					else
						tratti(m)!Rosso;
				  }
				  println("Strisce: rosso; Tratti dispari: verde; Tratti pari: rosso");
			case 3 => for (m<-0 to tratti.size-1) //Semafori rossi, pedoni verdi
				 	tratti(m)!Rosso;
				  for (p<-0 to strisce.size-1)
					strisce(p)!Verde;
				  println("Strisce: verde; Tratti rosso");
		}
		stato+=1;
		if(stato==4)
			stato=0;	
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
			dove=dove.substring(idZona.length, pos);
		else
			dove=dove.substring(idZona.length);
		//Caso particolare del calcolo	
		val tratto=dove.toInt;		
		if(tratto==1)
			tratti(tratti.size-1)!m;
		else
			tratti(tratto-2)!m;
	}

	//Invia i mezzi ai tratti giusti
	def gestisciMezzoD (m:mezzoDeviato): Unit  =
	{
		println("Mezzo (deviato) "+m.id+" arrivato all'incrocio");
		//Guardo su che tratto deve andare
		var dove=m.nxt;
		val pos=dove.indexOf("_"); 
		if (pos>=0)
			dove=dove.substring(idZona.length, pos);
		else
			dove=dove.substring(idZona.length);
		//Caso particolare del calcolo	
		val tratto=dove.toInt;		
		if(tratto==1)
			tratti(tratti.size-1)!m;
		else
			tratti(tratto-2)!m;
	}

	//Invia i pedoni alle strisce giuste
	def gestisciPedone (p:Persona): Unit  =
	{
		println("Persona "+p.id+" arrivata all'incrocio");
		//Guardo su che striscia deve andare
		var dove=p.nxt;
		dove=dove.substring(idZona.length+1);	
		//Casi particolari del calcolo	
		if(dove.toInt==1)
			strisce(strisce.size-2)!p;
		else if(dove.toInt==2)
			strisce(strisce.size-1)!p;
		else
			strisce(dove.toInt-3)!p;
	}

	//Invia i pedoni alle strisce giuste
	def gestisciPedoneD (p:personaDeviata): Unit  =
	{
		println("Persona (deviata) "+p.id+" arrivata all'incrocio");
		//Guardo su che striscia deve andare
		var dove=p.nxt;
		dove=dove.substring(idZona.length+1);	
		//Casi particolari del calcolo	
		if(dove.toInt==1)
			strisce(strisce.size-2)!p;
		else if(dove.toInt==2)
			strisce(strisce.size-1)!p;
		else
			strisce(dove.toInt-3)!p;
	}
}

class IncrocioPriorityActorMailbox(settings: ActorSystem.Settings, config: Config) extends UnboundedStablePriorityMailbox(
PriorityGenerator 
{
	case CambiaSemafori => 0
	case _ => 1
})
