name := "task-manager"

version := "0.1"

scalaVersion := "2.13.4"

val AkkaVersion = "2.6.13"
val Specs2Version = "4.10.6"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed"     % AkkaVersion,
  "org.specs2"        %% "specs2-core"          % Specs2Version,
  "org.specs2"        %% "specs2-junit"         % Specs2Version,
  "org.specs2"        %% "specs2-matcher-extra" % Specs2Version,
  "org.specs2"        %% "specs2-mock"          % Specs2Version,
  "org.specs2"        %% "specs2-scalacheck"    % Specs2Version
)
