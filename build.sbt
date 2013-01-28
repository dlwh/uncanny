import AssemblyKeys._ 
name := "uncanny"

version := "1.0"

organization := "dlwh"

scalaVersion := "2.10.0"

resolvers ++= Seq(
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
)

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.5" % "test",
  "org.jsoup" % "jsoup" % "1.6.3",
  "org.scalanlp" %% "breeze-process" % "0.2-SNAPSHOT",
  "org.scalanlp" %% "breeze-learn" % "0.2-SNAPSHOT",
  "org.scalanlp" %% "breeze-math" % "0.2-SNAPSHOT",
  "org.scalanlp" %% "breeze-core" % "0.2-SNAPSHOT",
  "org.apache.lucene" % "lucene-core" % "4.0.0-BETA",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.0.0-BETA",
  "org.apache.lucene" % "lucene-queryparser" % "4.0.0-BETA"
)


libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
  sv match {
    case "2.9.2" =>
      (deps :+ ("org.scalatest" % "scalatest" % "1.4.RC2" % "test"))
    case x if x.startsWith("2.8") =>
      (deps :+ ("org.scalatest" % "scalatest" % "1.3" % "test")
            :+ ("org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "test"))
    case _       =>
     (deps :+ ("org.scalacheck" %% "scalacheck" % "1.10.0" % "test")
           :+ ("org.scalatest" %% "scalatest" % "2.0.M5b" % "test"))
  }
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

scalacOptions ++= Seq("-no-specialization","-deprecation")

javaOptions += "-Xmx2g"



seq(assemblySettings: _*)

seq(sbtjflex.SbtJFlexPlugin.jflexSettings: _*)
