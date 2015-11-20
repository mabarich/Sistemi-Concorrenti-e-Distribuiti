package com.knoldus;

import scala.collection.mutable.ArrayBuffer
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import scala.concurrent.duration._
import akka.actor.Props
import com.typesafe.config.ConfigFactory

object Città
{
	var zone = ArrayBuffer[Pezzo]();
	var listaNomi = ArrayBuffer[String]();
	def main(args: Array[String]): Unit =
	{
		listaNomi+="Z1";
		listaNomi+="Z2";
		//listaNomi+="Z3";
		listaNomi+="Z4";
		
		listaNomi+="Z5";
		listaNomi+="Z6";
		listaNomi+="Z7";


		zone+=new Pezzo("2551","Z1", listaNomi)
		zone+=new Pezzo("2552","Z2", listaNomi)
		//zone+=new Pezzo("2553","Z3", listaNomi)
		zone+=new Pezzo("2554","Z4", listaNomi)
		zone+=new Pezzo("2555","Z5", listaNomi)
		zone+=new Pezzo("2556","Z6", listaNomi)
		zone+=new Pezzo("2557","Z7", listaNomi)

		for (x<-0 to zone.size-1)
			zone(x).start;
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


