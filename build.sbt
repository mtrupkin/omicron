name := """omicron"""

version := "1.0"

scalaVersion := "2.11.5"

resolvers ++= Seq(
	Resolver.url("me.mtrupkin ivy repo", url("http://dl.bintray.com/mtrupkin/ivy/"))(Resolver.ivyStylePatterns)
)

libraryDependencies += "org.pegdown" % "pegdown" % "1.5.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6" % "test"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.4.0-M2"

libraryDependencies += "me.mtrupkin.console" %% "console-core" % "0.7-SNAPSHOT"

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.20-R6"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)

libraryDependencies += "org.scalafx" %% "scalafxml-core-sfx8" % "0.2.2"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.3.2"

jreHome := (target in buildLauncher).value / "jre"

lazy val extractJRE = TaskKey[File]("extract-jre", "Extract embedded JRE")

extractJRE := {
  val launcherHome = (target in buildLauncher).value
  launcherHome.mkdirs()
  val jreZip = sourceDirectory.value / "build" / "windows" / "jre.zip"
  IO.unzip(jreZip, launcherHome)
  jreHome.value
}

buildLauncher <<= buildLauncher.dependsOn(extractJRE)

