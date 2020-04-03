name := "LeetCodeScala"

version := "0.1"

scalaVersion := "2.13.1"

lazy val root = (project in file(".")).
  settings(
    name := "leetcode-scala",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % Test
  )