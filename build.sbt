name := "nodes"<br>

scalaVersion := "2.10.4"<br>

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"<br>

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.10" % "2.3.9"<br>

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"<br>

scalacOptions ++= Seq("-feature", "-deprecation", "unchecked")<br>

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2")<br>
