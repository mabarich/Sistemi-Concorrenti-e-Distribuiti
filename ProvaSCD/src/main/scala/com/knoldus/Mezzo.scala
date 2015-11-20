package com.knoldus

import scala.collection.mutable.ArrayBuffer

trait Mezzo 
{
	protected var _id: String="";
	protected var _percorso = ArrayBuffer [String]();
	protected var _next: Int = -1;
	protected var _nextZona: Int = -1;
	protected var _zone = ArrayBuffer [String]();

	def reset: Unit= _next = -1;

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

	def id = _id 
	def id_= (value:String):Unit =  { _id = value; }

	def percorso = _percorso 
	def percorso_= (value:ArrayBuffer [String]):Unit =  { _percorso = value; }

	def next = _next 
	def next_= (value:Int):Unit =  { _next = value; }

	def zone = _zone 
	def zone_= (value:ArrayBuffer [String]):Unit =  { _zone = value; }
}

class mezzoDeviato (i:String, m:Mezzo) extends Serializable
{
	protected var _mezzo:Mezzo=m;
//	protected var _id: String=i;
//	protected var _percorso = ArrayBuffer [String]();
	protected var _trattiFatti=ArrayBuffer[String]();	
	protected var _trattiDaFare=ArrayBuffer[String]();

	/*def id = _id 
	def id_= (value:String):Unit =  { _id = value; }*/

	def mezzo = _mezzo 
	def mezzo_= (value:Mezzo):Unit =  { _mezzo = value; }

	def trattiFatti = _trattiFatti 
	def trattiFatti_= (value:ArrayBuffer[String]):Unit =  { _trattiFatti = value; }
		
	def trattiDaFare = _trattiDaFare 
	def trattiDaFare_= (value:ArrayBuffer[String]):Unit =  { _trattiDaFare = value; }

	/*def percorso = _percorso 
	def percorso_= (value:ArrayBuffer [String]):Unit =  { _percorso = value; }*/

	def trattoFatto: Unit =
	{	
		val tratto:String=trattiDaFare(0);
		trattiDaFare.remove(0);
		trattiFatti+=tratto;
	}
}

class Auto (i: String, p: ArrayBuffer [String], g: Persona, z: ArrayBuffer [String]) extends Mezzo with Serializable
{
	protected var _guidatore:Persona=null;

	id = i;
	percorso = p;
	guidatore = g;
	zone=z;


	def guidatore = _guidatore 
	def guidatore_= (value:Persona):Unit =  { _guidatore = value; }
}

class Autobus (i: String, p: ArrayBuffer [String], z: ArrayBuffer [String]) extends Mezzo with Serializable
{
	protected var _passeggeri = ArrayBuffer [Persona]();
	protected val _limite:Int = 20;

	id = i;
	percorso = p;
	zone=z;

	def limite = _limite 

	def postiLiberi = _limite-_passeggeri.size

	def togli (idFermata:String) : ArrayBuffer [Persona] =
	{
		var discesa= ArrayBuffer [Persona]();
		if (_passeggeri.size!=0)
		{
			var restanti= ArrayBuffer [Persona]();
			for (p <- 0 to _passeggeri.size-1)
			{	
				var dove=_passeggeri(p).nxt; 
				if (dove==idFermata) 
				{
					discesa+=_passeggeri(p);
					println("Il passeggero "+_passeggeri(p).id+" è sceso dalla corriera");
				}
					
				else
					restanti+=_passeggeri(p);
			}
			_passeggeri=restanti;
		}
		discesa;
	}

	def aggiungi (p: Persona): Unit =
	{
		_passeggeri+=p;
	}

	def passeggeri = _passeggeri 
	def passeggeri_= (value:ArrayBuffer [Persona]):Unit =  { _passeggeri = value; }
}
