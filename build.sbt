name := "dotmaster"

version := "0.1"

scalaVersion := "2.13.0"

resolvers += "yuiwai repo" at "https://s3-us-west-2.amazonaws.com/repo.yuiwai.com"

libraryDependencies ++= Seq(
  "com.yuiwai" %%% "yachiyo-zio" % "0.2.2-SNAPSHOT",
  "org.scala-js" %%% "scalajs-dom" % "0.9.2"
)

scalaJSUseMainModuleInitializer := true

enablePlugins(ScalaJSPlugin)

Compile / fastOptJS / artifactPath := baseDirectory.value / "dotmaster.js"
