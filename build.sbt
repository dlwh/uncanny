import AssemblyKeys._ 
name := "uncanny"

version := "1.0"

organization := "dlwh"

scalaVersion := "2.9.2"

resolvers ++= Seq(
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
)

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.5" % "test",
  "org.jsoup" % "jsoup" % "1.6.3",
  "org.scalanlp" %% "breeze-process" % "0.1-SNAPSHOT",
  "org.scalanlp" %% "breeze-learn" % "0.1-SNAPSHOT",
  "org.scalanlp" %% "breeze-math" % "0.1-SNAPSHOT",
  "org.apache.lucene" % "lucene-core" % "4.0.0-BETA",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.0.0-BETA"
)

libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
  sv match {
    case "2.9.2" =>
      (deps :+ ("org.scalatest" % "scalatest" % "1.4.RC2" % "test")
            :+ ("org.scala-tools.testing" % "scalacheck_2.9.1" % "1.9" % "test"))
    case x if x.startsWith("2.8") =>
      (deps :+ ("org.scalatest" % "scalatest" % "1.3" % "test")
            :+ ("org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "test"))
    case x  => error("Unsupported Scala version " + x)
  }
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

javacOptions ++= Seq("-source", "1.5", "-target", "1.5")

scalacOptions ++= Seq("-no-specialization","-deprecation","-target:jvm-1.5")

javaOptions += "-Xmx2g"



seq(assemblySettings: _*)

seq(sbtjflex.SbtJFlexPlugin.jflexSettings: _*)
