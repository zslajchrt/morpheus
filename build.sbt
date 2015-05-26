name := "Morpheus"

organization := "org.morpheus"

version := "0.7"

scalaVersion := "2.11.5"

val paradiseVersion = "2.1.0-M5"

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.5"

libraryDependencies += "cglib" % "cglib" % "2.2.2"

libraryDependencies += "asm" % "asm-all" % "3.3.1"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.1.0"

libraryDependencies += "junit" % "junit" % "4.4"

//addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)

exportJars := true