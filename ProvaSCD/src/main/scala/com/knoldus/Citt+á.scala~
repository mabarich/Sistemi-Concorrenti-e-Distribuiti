package com.knoldus;

import scala.collection.mutable.ArrayBuffer
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import scala.concurrent.duration._
import akka.actor.Props
import com.typesafe.config.ConfigFactory
import scala.xml.XML

object Città
{
	//var zone = ArrayBuffer[Pezzo]();
	var listaNomi = ArrayBuffer[String]();
	def main(args: Array[String]): Unit =
	{
		var nome="/home/marco/Scrivania/ProvaSCD/src/main/scala/com/knoldus/Zone.xml";
		val xml=XML.loadFile(nome);
		val zone=xml \ "Zone" \ "Zona";
		for(x <- 0 to zone.size-1)
		{
			val id:String=zone(x)\"@id" text;
			listaNomi+=id;
		}
		if(args.size==0)
		{		
			for(x <- 0 to listaNomi.size-1)
			{
				val port=2551+x;
				new Pezzo(port.toString, listaNomi(x), listaNomi).start;
			}
		}
		else if (args.size==2)
		{
			new Pezzo(args(1), args(0), listaNomi).start;
		}
	}
}

class Pezzo (p:String, i:String, ln:ArrayBuffer[String])
{
	var port:String=p;
	var id:String=i;
	var listaNomi:ArrayBuffer[String]=ln;
	
	def start:Unit=
	{
		val config = ConfigFactory.parseString(s"akka.remote.netty.tcp.port=$port").withFallback(ConfigFactory.parseString("akka.cluster.roles = [Zona]")).withFallback(ConfigFactory.load())
		val system = ActorSystem("Priority", config);
		val zona = system.actorOf(Props(classOf[Zona],id,port, listaNomi).withDispatcher("prio-dispatcher4"), id);
		zona!Start;
	}
}


