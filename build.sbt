ThisBuild / organization := "com.nicolaswinsten"
ThisBuild / version := "0.2"
ThisBuild / scalaVersion := "2.13.4"

name := "wiki"

// HTML scraper
libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "2.2.0"

// my personal utility package
externalResolvers += "util packages" at "https://maven.pkg.github.com/NicolasWinsten/util"
libraryDependencies += "com.nicolaswinsten" %% "util" % "0.1"

githubTokenSource := TokenSource.GitConfig("github.token")
githubOwner := "NicolasWinsten"
githubRepository := "wiki"