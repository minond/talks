scalaVersion := "2.12.2"

scalacOptions := Seq("-deprecation",
                     "-Ywarn-unused",
                     "-Ywarn-unused-import",
                     "-Ywarn-dead-code",
                     "-Ywarn-numeric-widen")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
