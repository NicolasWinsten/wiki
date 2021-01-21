ThisBuild / organization := "com.nicolaswinsten"
ThisBuild / version := "0.1"
ThisBuild / scalaVersion := "2.13.4"

name := "wiki"

// HTML scraper
libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "2.2.0"


githubTokenSource := TokenSource.GitConfig("github.token")
githubOwner := "NicolasWinsten"
githubRepository := "wiki"