package com.knoldus


import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import scalafx.scene.paint.{LinearGradient, Stops}
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text
import scalafx.scene.layout.HBox


import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import scala.concurrent.duration._
import akka.actor.Props
import com.typesafe.config.ConfigFactory

object PriorityTestApplication extends JFXApp 
{

	val system = ActorSystem("priority", ConfigFactory.load);
	//val myPriorityActor = system.actorOf(Props[MyPriorityActor].withDispatcher("prio-dispatcher"));
	val zonaProva = system.actorOf(Props[Zona], "zonaProva");

	
	stage = new PrimaryStage 
	{
		title.value = "Hello Stage"
		width = 600
		height = 450
		scene = new Scene 
		{
			fill = LightGreen
			content = new HBox 
			{
				
				/*children = Seq( new Text 
				{
					text="SFX";
					style = "-fx-font-size: 20pt"
					fill = new LinearGradient(
					endX = 0,
					stops = Stops(PaleGreen, SeaGreen))
				},
				new Text
				{
					text="           SFX2";
					style = "-fx-font-size: 20pt"
					fill = new LinearGradient(
					endX = 0,
					stops = Stops(PaleGreen, SeaGreen))
				} )*/


				children = Seq( new HBox 
				{
					fill = Red
					children = Seq( new Text 
					{
						text="SFX";
						style = "-fx-font-size: 20pt"
						fill = new LinearGradient(
						endX = 0,
						stops = Stops(PaleGreen, SeaGreen))
					},
					new Text
					{
						text="           SFX2";
						style = "-fx-font-size: 20pt"
						fill = new LinearGradient(
						endX = 0,
						stops = Stops(PaleGreen, SeaGreen))
					} )
					zonaProva!this;
				},
				new HBox
				{
					fill = Blue
					children = Seq( new Text 
					{
						text="SFX";
						style = "-fx-font-size: 20pt"
						fill = new LinearGradient(
						endX = 0,
						stops = Stops(PaleGreen, SeaGreen))
					},
					new Text
					{
						text="           SFX2";
						style = "-fx-font-size: 20pt"
						fill = new LinearGradient(
						endX = 0,
						stops = Stops(PaleGreen, SeaGreen))
					} )
					zonaProva!this;
				} ,
				new HBox
				{
					fill = Blue
					children = Seq( new Text 
					{
						text="SFX";
						style = "-fx-font-size: 20pt"
						fill = new LinearGradient(
						endX = 0,
						stops = Stops(PaleGreen, SeaGreen))
					},
					new Text
					{
						text="           SFX2";
						style = "-fx-font-size: 20pt"
						fill = new LinearGradient(
						endX = 0,
						stops = Stops(PaleGreen, SeaGreen))
					} )
					zonaProva!this;
				},
				new HBox
				{
					fill = Blue
					children = Seq( new Text 
					{
						text="SFX";
						style = "-fx-font-size: 20pt"
						fill = new LinearGradient(
						endX = 0,
						stops = Stops(PaleGreen, SeaGreen))
					},
					new Text
					{
						text="           SFX2";
						style = "-fx-font-size: 20pt"
						fill = new LinearGradient(
						endX = 0,
						stops = Stops(PaleGreen, SeaGreen))
					} )
					zonaProva!this;
				}




				,
				new HBox
				{
					fill = Blue
					children = Seq( new Text 
					{
						text="SFX";
						style = "-fx-font-size: 20pt"
						fill = new LinearGradient(
						endX = 0,
						stops = Stops(PaleGreen, SeaGreen))
					},
					new Text
					{
						text="           SFX2";
						style = "-fx-font-size: 20pt"
						fill = new LinearGradient(
						endX = 0,
						stops = Stops(PaleGreen, SeaGreen))
					} )
					zonaProva!new containerHBox(this);
				},
				new HBox
				{
					fill = Blue
					children = Seq( new Text 
					{
						text="SFX";
						style = "-fx-font-size: 20pt"
						fill = new LinearGradient(
						endX = 0,
						stops = Stops(PaleGreen, SeaGreen))
					},
					new Text
					{
						text="           SFX2";
						style = "-fx-font-size: 20pt"
						fill = new LinearGradient(
						endX = 0,
						stops = Stops(PaleGreen, SeaGreen))
					} )
					zonaProva!new containerHBox(this);
				},
				new HBox
				{
					fill = Blue
					children = Seq( new Text 
					{
						text="SFX";
						style = "-fx-font-size: 20pt"
						fill = new LinearGradient(
						endX = 0,
						stops = Stops(PaleGreen, SeaGreen))
					},
					new Text
					{
						text="           SFX2";
						style = "-fx-font-size: 20pt"
						fill = new LinearGradient(
						endX = 0,
						stops = Stops(PaleGreen, SeaGreen))
					} )
					zonaProva!new containerHBox(this);
				},
				new HBox
				{
					fill = Blue
					children = Seq( new Text 
					{
						text="SFX";
						style = "-fx-font-size: 20pt"
						fill = new LinearGradient(
						endX = 0,
						stops = Stops(PaleGreen, SeaGreen))
					},
					new Text
					{
						text="           SFX2";
						style = "-fx-font-size: 20pt"
						fill = new LinearGradient(
						endX = 0,
						stops = Stops(PaleGreen, SeaGreen))
					} )
					zonaProva!new containerHBox(this);
				})

			}
			//zonaProva!this;			
		}
	}


	zonaProva!"Start";
}


