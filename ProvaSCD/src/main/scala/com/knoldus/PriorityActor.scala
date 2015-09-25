package com.knoldus

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.dispatch.UnboundedStablePriorityMailbox
import com.typesafe.config.Config
import akka.dispatch.PriorityGenerator
import akka.actor.Stash

case object Change

class MyPriorityActor extends Actor {

  var lista:List[Int]=List()

  //Stop: devo accodare
  override def receive: Receive = 
  {
    case Change => context.become(printingBehavior, false)
    case m:Int => println("Stampo int1: "+m)
    case m:String => println("Stampo stringa1: "+m)
    
  }

  //Start: devo stampare
  def printingBehavior: Receive = 
  {
    case m:Int => println("Stampo int2: "+m)
    case m:String => println("Stampo stringa2: "+m)
    case Change => context.unbecome()
  }

}

class MyPriorityActorMailbox(settings: ActorSystem.Settings, config: Config) extends UnboundedStablePriorityMailbox(
  // Create a new PriorityGenerator, lower prio means more important
  PriorityGenerator {

    case m: String => 1
    case m: Int => 2
    case Change => 0
  })
