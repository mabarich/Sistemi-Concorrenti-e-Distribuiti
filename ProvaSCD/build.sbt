name := "priorityActor"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")


// Add multiple dependencies
libraryDependencies ++= Seq(
	"com.typesafe.akka" 	%% 	"akka-actor" 		% 	"2.4-SNAPSHOT",
	"org.scala-lang.modules" %% 	"scala-xml" 		% 	"1.0.2"
)

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"
