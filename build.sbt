val scala3Version = "3.5.0"

val akkaVersion = "2.9.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "programming_course",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    resolvers += "Akka library repository".at("https://repo.akka.io/maven"),
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
    libraryDependencies += ("com.typesafe.akka" %% "akka-actor-typed" % akkaVersion)
      .cross(CrossVersion.for3Use2_13)
  )
