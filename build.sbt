lazy val root = (project in file(".")).
	settings(commonSettings: _*
)

val sext = "com.github.nikita-volkov" % "sext" % "0.2.3"
val parboiled2 = "org.parboiled" %% "parboiled" % "2.1.0"

lazy val commonSettings = Seq(
  libraryDependencies += sext,
  libraryDependencies += parboiled2
)

