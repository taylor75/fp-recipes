name := "FP-Recipes"

version := "1.0"

scalaVersion := "2.10.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1"

scalacOptions += "-language:implicitConversions"

scalacOptions += "-feature"
