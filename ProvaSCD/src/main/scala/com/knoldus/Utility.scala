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

class containerMezzo (m: Mezzo)
{
	var _mezzo = m;

	def mezzo = _mezzo 
	def mezzo_= (value:Mezzo):Unit =  { _mezzo = value; }
}

class containerPersona (p: Persona)
{
	var _persona = p;

	def persona = _persona 
	def persona_= (value:Persona):Unit =  { _persona = value; }
}

case object Rosso
case object Verde
case object CambiaSemafori

class mezzoPiuPriorita (m: Mezzo)
{
	var _mezzo: Mezzo = m;

	def mezzo = _mezzo 
	def mezzo_= (value:Mezzo):Unit =  { _mezzo = value; }
}

class personaPiuPriorita (p: Persona)
{
	var _persona:Persona = p;

	def persona = _persona 
	def persona_= (value:Persona):Unit =  { _persona = value; }
}