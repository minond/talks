scalacOptions := Seq("-deprecation",
                     "-Xfatal-warnings",
                     "-Ywarn-unused",
                     "-Ywarn-unused-import",
                     "-Ywarn-dead-code",
                     "-Ywarn-numeric-widen")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
