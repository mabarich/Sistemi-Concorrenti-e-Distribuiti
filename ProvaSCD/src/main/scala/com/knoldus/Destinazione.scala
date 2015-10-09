package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import scala.collection.mutable.ArrayBuffer

class Destinazione extends Actor 
{	
	var zona:ActorRef=null;
  	override def receive: Actor.Receive = 
	{
		case z:ActorRef => zona=z;
		case p:Persona => p.inc; zona!p;
		case m:Mezzo =>	m.inc; zona!m;		   
	}
}
