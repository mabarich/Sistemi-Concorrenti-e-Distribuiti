package com.knoldus

import scala.collection.mutable.ArrayBuffer

trait Persona 
{
	protected var _id: String="";
	protected var _percorso = ArrayBuffer [String]();
	protected var _next: Int = -1;

	def reset: Unit= _next = -1;

	def inc: Unit = 
	{
		_next+=1;
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

	def id = _id 
	def id_= (value:String):Unit =  { _id = value; }

	def percorso = _percorso 
	def percorso_= (value:ArrayBuffer [String]):Unit =  { _percorso = value; }
}

class personaDeviata (p:Persona) extends Serializable
{
	var _persona:Persona=p;
	var _trattiFatti=ArrayBuffer[String]();	
	var _trattiDaFare=ArrayBuffer[String]();

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
}

class Pedone (i: String, p: ArrayBuffer [String]) extends Persona with Serializable
{
	id = i;
	percorso = p;
}
