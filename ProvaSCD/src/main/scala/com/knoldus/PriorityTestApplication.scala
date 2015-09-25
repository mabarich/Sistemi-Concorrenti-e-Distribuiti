package com.knoldus

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import scala.concurrent.duration._
import akka.actor.Props
import com.typesafe.config.ConfigFactory

object PriorityTestApplication extends App 
{

	val system = ActorSystem("priority", ConfigFactory.load);
	//val myPriorityActor = system.actorOf(Props[MyPriorityActor].withDispatcher("prio-dispatcher"));
	val zonaProva = system.actorOf(Props[Zona], "zonaProva");
	zonaProva!"Start";
}


