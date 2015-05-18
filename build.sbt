lazy val root = (project in file(".")).
	settings(commonSettings: _*
)

val sext = "com.github.nikita-volkov" % "sext" % "0.2.3"

lazy val commonSettings = Seq(
  libraryDependencies += sext
)
