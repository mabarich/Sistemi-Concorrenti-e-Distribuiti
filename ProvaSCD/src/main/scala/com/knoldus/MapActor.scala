package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef

class MapActor (i: String, z:ActorRef) extends Actor 
{	
	var zona:ActorRef=z;
	var idZona:String=i;

  	override def receive: Actor.Receive = 
	{
		case c:String => if(c==idZona) { sender!new containerVicino(idZona,zona); }	   
	}
}
