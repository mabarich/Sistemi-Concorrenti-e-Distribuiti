package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.Await
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

class MapActor (i: String, z:ActorRef) extends Actor 
{	
	var zona:ActorRef=z;
	var idZona:String=i;

  	override def receive: Actor.Receive = 
	{
		case c:String => if(c==idZona) { sender!new containerVicino(idZona,zona); }	   
		case c:ChiediVicini => if (c.id!=idZona) chiedi(sender);
	}

	def chiedi (sender:ActorRef): Unit =
	{
		implicit val timeout = Timeout(10 seconds);
		val future = zona?ChiediVicini;
		val cg:CostruzioneGrafo = Await.result(future, timeout.duration).asInstanceOf[CostruzioneGrafo];		
		sender!cg;
	}
}
