package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import scala.collection.mutable.ArrayBuffer

class Destinazione (z:ActorRef) extends Actor 
{	
	var zona:ActorRef=z;
  	override def receive: Actor.Receive = 
	{
		case p:Persona => zona!p;
		case m:Mezzo => zona!m;		   
	}
}
