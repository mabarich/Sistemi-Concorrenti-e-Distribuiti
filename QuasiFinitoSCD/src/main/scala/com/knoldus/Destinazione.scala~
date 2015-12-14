package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import scala.collection.mutable.ArrayBuffer

class Destinazione (z:ActorRef) extends Actor 
{	
	var zona:ActorRef=z;
  	override def receive: Actor.Receive = 
	{
		case p:Persona => waitABit; waitABit; waitABit; waitABit; waitABit; waitABit; waitABit; waitABit; zona!p;
		case m:Mezzo => waitABit; waitABit; waitABit; waitABit; waitABit; waitABit; waitABit; waitABit; zona!m;		   
	}

	def waitABit:Unit = 
	{
		var x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;} 
		x=0; for (a<-0 to 999999) {x+=1;}
	}
}
