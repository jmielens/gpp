name := "gpp"

version := "0.1"

organization := "edu.utexas"

scalaVersion := "2.10.1"

resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies += "org.scalanlp" % "chalk" % "1.1.3-SNAPSHOT"

libraryDependencies += "org.scalanlp" % "nak" % "1.1.2"

libraryDependencies += "org.rogach" %% "scallop" % "0.8.1"

retrieveManaged := true

crossPaths := false
