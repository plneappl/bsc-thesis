lazy val root = (project in file(".")).
	settings(commonSettings: _*
)

val sext = "com.github.nikita-volkov" % "sext" % "0.2.3"
val parboiled2 = "org.parboiled" %% "parboiled" % "2.1.0"

lazy val commonSettings = Seq(
  libraryDependencies += sext,
  libraryDependencies += parboiled2
)

unmanagedJars in Compile <++= baseDirectory map { base =>
    val baseDirectories = (base / "prologLib") 
    val customJars = (baseDirectories ** "*.jar")
    customJars.classpath
}

fork := true
connectInput in run := true
outputStrategy        :=   Some(StdoutOutput) // Get rid of output prefix
scalacOptions ++= Seq("-unchecked", "-deprecation")

