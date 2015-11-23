package com.knoldus

import scala.collection.mutable.ArrayBuffer

trait Persona 
{
	protected var _id: String="";
	protected var _percorso = ArrayBuffer [String]();
	protected var _zone = ArrayBuffer [String]();
	protected var _next: Int = -1;
	protected var _nextZona: Int = -1;
	protected var _deviata:Boolean = false;

	def reset: Unit= 
	{
		_next = -1;
		_nextZona = -1;
	}

	def inc: Unit = 
	{
		_next+=1;
	}

	def incZona: Unit = 
	{
		_nextZona+=1;
	}

	def to: String = 
	{
		val n=_next+1;
		_percorso(n);
	}

	def nxt: String = 
	{
		_percorso(_next);
	}

	def nxtZona: String = 
	{
		_zone(_nextZona);
	}

	def toZona: String = 
	{
		val n=_nextZona+1;
		_zone(n);
	}

	def to2Zona: String = 
	{
		val n=_nextZona+2;
		_zone(n);
	}

	def id = _id 
	def id_= (value:String):Unit =  { _id = value; }

	def next = _next 
	def next_= (value:Int):Unit =  { _next = value; }

	def percorso = _percorso 
	def percorso_= (value:ArrayBuffer [String]):Unit =  { _percorso = value; }

	def zone = _zone 
	def zone_= (value:ArrayBuffer [String]):Unit =  { _zone = value; }

	def deviata = _deviata 
	def deviata_= (value:Boolean):Unit =  { _deviata = value; }
}

class personaDeviata (i:String, p:Persona) extends Persona with Serializable
{
	protected var _persona:Persona=p;
	protected var _trattiFatti=ArrayBuffer[String]();	
	protected var _trattiDaFare=ArrayBuffer[String]();

	_deviata=true;

	_id=i;
	
	def persona = _persona 
	def persona_= (value:Persona):Unit =  { _persona = value; }

	def trattiFatti = _trattiFatti 
	def trattiFatti_= (value:ArrayBuffer[String]):Unit =  { _trattiFatti = value; }
		
	def trattiDaFare = _trattiDaFare 
	def trattiDaFare_= (value:ArrayBuffer[String]):Unit =  { _trattiDaFare = value; }

	def trattoFatto: Unit =
	{	
		val tratto:String=trattiDaFare(0);
		trattiDaFare.remove(0);
		trattiFatti+=tratto;
	}

	override def toZona: String = 
	{
		val n=_nextZona+1;
		_trattiDaFare(n);
	}

	override def to2Zona: String = 
	{
		val n=_nextZona+2;
		_trattiDaFare(n);
	}

	override def nxtZona: String = 
	{
		_zone(_nextZona);
	}

	def resetPercorso: Unit = 
	{
		_percorso = ArrayBuffer [String]();
	}
}

class Pedone (i: String, p: ArrayBuffer [String], z: ArrayBuffer [String]) extends Persona with Serializable
{
	id = i;
	percorso = p;
	zone=z;
}
