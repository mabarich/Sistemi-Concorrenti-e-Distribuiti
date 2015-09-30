package com.knoldus

import akka.actor.ActorRef
import scala.collection.mutable.ArrayBuffer

class containerActrf (c: ArrayBuffer[ActorRef], m: ArrayBuffer[ActorRef])
{
	var _buffCorsia = c;
	var _buffMarciapiede = m;

	def buffCorsia = _buffCorsia 
	def buffCorsia_= (value:ArrayBuffer[ActorRef]):Unit =  { _buffCorsia = value; }

	def buffMarciapiede = _buffMarciapiede 
	def buffMarciapiede_= (value:ArrayBuffer[ActorRef]):Unit =  { _buffMarciapiede = value; }
}

class containerFermata (f: ActorRef)
{
	var _fermata = f;

	def fermata = _fermata 
	def fermata_= (value:ActorRef):Unit =  { _fermata = value; }
}
