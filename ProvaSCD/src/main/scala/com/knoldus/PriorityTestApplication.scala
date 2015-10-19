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
	val z1 = system.actorOf(Props[Zona], "Z1");
	z1!"Z1";
	val z2 = system.actorOf(Props[Zona], "Z2");
	z2!"Z2";
	val z3 = system.actorOf(Props[Zona], "Z3");
	z3!"Z3";
	val z4 = system.actorOf(Props[Zona], "Z4");
	z4!"Z4";
	
	z1!new containerZona(null);
	z1!new containerZona(z2);
	z1!new containerZona(z3);
	z1!new containerZona(null);

	z2!new containerZona(null);
	z2!new containerZona(null);
	z2!new containerZona(z4);
	z2!new containerZona(z1);

	z3!new containerZona(z1);
	z3!new containerZona(z4);
	z3!new containerZona(null);
	z3!new containerZona(null);
	
	z4!new containerZona(z2);
	z4!new containerZona(null);
	z4!new containerZona(null);
	z4!new containerZona(z3);
	
	/*stage = new PrimaryStage 
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
	}*/


	z1!"Start";
	z2!"Start";
	z3!"Start";
	z4!"Start";
}


