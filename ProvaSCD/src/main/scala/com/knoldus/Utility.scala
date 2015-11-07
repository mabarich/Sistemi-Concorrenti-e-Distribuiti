package com.knoldus

import akka.actor.ActorRef
import scala.collection.mutable.ArrayBuffer
import scalafx.application.JFXApp
import scalafx.scene.layout.HBox

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

class containerDestinazione (d: ActorRef)
{
	var _destinazione = d;

	def destinazione = _destinazione 
	def destinazione_= (value:ActorRef):Unit =  { _destinazione = value; }
}

class containerZona (z: ActorRef)
{
	var _zona = z;

	def zona = _zona 
	def zona_= (value:ActorRef):Unit =  { _zona = value; }
}

class containerVicino (i: String, z: ActorRef)
{
	var _zona = z;
	var _id = i;

	def zona = _zona 
	def zona_= (value:ActorRef):Unit =  { _zona = value; }

	def id = _id 
	def id_= (value:String):Unit =  { _id = value; }
}

class containerId (m: String)
	{
		var _id = m;

		def id = _id 
		def id_= (value:String):Unit =  { _id = value; }
	}

class containerHBox (h: HBox)
{
	var _hb = h;

	def hb = _hb 
	def hb_= (value:HBox):Unit =  { _hb = value; }
}


case object Rosso
case object Verde
case object CambiaSemafori
case object RequestID
case object Vicini
case object Strade
case object Marciapiedi
case object Incroci
case object AggiornaIncrocio
case object Movimenti
case object Start

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
