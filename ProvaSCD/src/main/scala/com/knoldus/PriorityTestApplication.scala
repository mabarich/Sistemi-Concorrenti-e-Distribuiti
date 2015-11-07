package com.knoldus;


import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import scalafx.scene.paint.{LinearGradient, Stops}
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text
import scalafx.scene.layout.HBox
import scalafx.scene.layout.BorderPane


import javafx.application.Application;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.stage.Stage;
import javafx.scene.control.Label;
import javafx.scene.layout.StackPane;


import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import scala.concurrent.duration._
import akka.actor.Props
import com.typesafe.config.ConfigFactory

object PriorityTestApplication extends JFXApp
{
	override def main(args: Array[String]) =
	{
		new Via("2551","Z1").parti;
		new Via("2552","Z2").parti;
		new Via("2553","Z3").parti;
		new Via("2554","Z4").parti;
	}
}

class Via (p:String, i:String)
{
	var port:String=p;
	var id:String=i;
	
	def parti:Unit=
	{
		val config = ConfigFactory.parseString(s"akka.remote.netty.tcp.port=$port").withFallback(ConfigFactory.parseString("akka.cluster.roles = [Zona]")).withFallback(ConfigFactory.load())
		val system = ActorSystem("Priority", config);
		val zona = system.actorOf(Props(classOf[Zona],id,port).withDispatcher("prio-dispatcher4"), id);
		zona!Start;
	}
}


