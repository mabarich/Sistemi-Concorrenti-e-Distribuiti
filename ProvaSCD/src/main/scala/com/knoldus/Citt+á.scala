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
		listaNomi+="Z11";
		listaNomi+="Z12";
		//listaNomi+="Z3";
		listaNomi+="Z14";
		
		listaNomi+="Z15";
		listaNomi+="Z16";
		listaNomi+="Z17";


		zone+=new Pezzo("2551","Z11", listaNomi)
		zone+=new Pezzo("2552","Z12", listaNomi)
		//zone+=new Pezzo("2553","Z13", listaNomi)
		zone+=new Pezzo("2554","Z14", listaNomi)
		zone+=new Pezzo("2555","Z15", listaNomi)
		zone+=new Pezzo("2556","Z16", listaNomi)
		zone+=new Pezzo("2557","Z17", listaNomi)

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

		
		println("\n\n\n"+id+"\n\n\n");		


		val zona = system.actorOf(Props(classOf[Zona],id,port, listaNomi).withDispatcher("prio-dispatcher4"), id);
		zona!Start;
	}
}


