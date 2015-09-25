package com.knoldus

import scala.collection.mutable.ArrayBuffer

trait Persona 
{
	protected var _id: String="";
	protected var _percorso = ArrayBuffer [String]();
	protected var _next: Int = -1;

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

class Pedone (i: String, p: ArrayBuffer [String]) extends Persona 
{
	id = i;
	percorso = p;
}
