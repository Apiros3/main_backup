import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.15"
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"
}
