import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import scala.sys.process._

organization in ThisBuild := "me.shadaj"

lazy val scala211Version = "2.11.12"
lazy val scala212Version = "2.12.9"
lazy val scala213Version = "2.13.1"
lazy val supportedScalaVersions = List(scala211Version, scala212Version, scala213Version)

scalaVersion in ThisBuild := scala213Version

lazy val scalapy = project.in(file(".")).aggregate(
  macrosJVM, macrosNative,
  coreJVM, coreNative,
).settings(
  publish := {},
  publishLocal := {}
)

addCommandAlias(
  "publishSignedAll",
  (scalapy: ProjectDefinition[ProjectReference])
    .aggregate
    .map(p => s"+ ${p.asInstanceOf[LocalProject].project}/publishSigned")
    .mkString(";", ";", "")
)

lazy val macros = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("coreMacros"))
  .settings(
    name := "scalapy-macros",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  ).jvmSettings(
    crossScalaVersions := supportedScalaVersions,
  ).nativeSettings(
    scalaVersion := scala211Version
  )

lazy val macrosJVM = macros.jvm
lazy val macrosNative = macros.native

lazy val core = crossProject(JVMPlatform, NativePlatform)
  .in(file("core"))
  .dependsOn(macros)
  .settings(
    name := "scalapy-core",
    sourceGenerators in Compile += Def.task {
      val fileToWrite = (sourceManaged in Compile).value / "TupleReaders.scala"
      val methods = (2 to 22).map { n =>
        val tupleElements = (1 to n).map(t => s"r$t.read(orArr(${t - 1}))")
          .mkString(", ")
        s"""implicit def tuple${n}Reader[${(1 to n).map(t => s"T$t").mkString(", ")}](implicit ${(1 to n).map(t => s"r$t: Reader[T$t]").mkString(", ")}): Reader[(${(1 to n).map(t => s"T$t").mkString(", ")})] = {
           |  new Reader[(${(1 to n).map(t => s"T$t").mkString(", ")})] {
           |    override def read(or: PyValue) = {
           |      val orArr = or.getTuple
           |      ($tupleElements)
           |    }
           |  }
           |}"""
      }
    
      val toWrite =
        s"""package me.shadaj.scalapy.readwrite
           |import me.shadaj.scalapy.interpreter.PyValue
           |trait TupleReaders {
           |${methods.mkString("\n")}
           |}""".stripMargin
    
      IO.write(fileToWrite, toWrite)
      Seq(fileToWrite)
    },
    sourceGenerators in Compile += Def.task  {
      val fileToWrite = (sourceManaged in Compile).value / "TupleWriters.scala"
      val methods = (2 to 22).map { n =>
        val seqArgs = (1 to n).map(t => s"r$t.write(v._" + t + ")").mkString(", ")
        s"""implicit def tuple${n}Writer[${(1 to n).map(t => s"T$t").mkString(", ")}](implicit ${(1 to n).map(t => s"r$t: Writer[T$t]").mkString(", ")}): Writer[(${(1 to n).map(t => s"T$t").mkString(", ")})] = {
           |  new Writer[(${(1 to n).map(t => s"T$t").mkString(", ")})] {
           |    override def write(v: (${(1 to n).map(t => s"T$t").mkString(", ")})): PyValue = {
           |      CPythonInterpreter.createTuple(Seq(${seqArgs}))
           |    }
           |  }
           |}"""
      }
    
      val toWrite =
        s"""package me.shadaj.scalapy.readwrite
           |import me.shadaj.scalapy.interpreter.{CPythonInterpreter, PyValue}
           |trait TupleWriters {
           |${methods.mkString("\n")}
           |}""".stripMargin
    
      IO.write(fileToWrite, toWrite)
      Seq(fileToWrite)
    },
    sourceGenerators in Compile += Def.task {
      val fileToWrite = (sourceManaged in Compile).value / "FunctionReaders.scala"
      val methods = (0 to 22).map { n =>
        val functionArgs = (1 to n).map(t => s"w$t.write(i$t)")
          .mkString(", ")
        s"""implicit def function${n}Reader[${((1 to n).map(t => s"T$t") :+ "O").mkString(", ")}](implicit ${((1 to n).map(t => s"w$t: Writer[T$t]") :+ "oReader: Reader[O]").mkString(", ")}): Reader[(${(1 to n).map(t => s"T$t").mkString(", ")}) => O] = {
           |  new Reader[(${(1 to n).map(t => s"T$t").mkString(", ")}) => O] {
           |    override def read(orig: PyValue) = {
           |      (${(1 to n).map(t => s"i$t: T$t").mkString(", ")}) => {
           |        oReader.read(CPythonInterpreter.call(orig, Seq($functionArgs), Seq()))
           |      }
           |    }
           |  }
           |}"""
      }
    
      val toWrite =
        s"""package me.shadaj.scalapy.readwrite
           |import me.shadaj.scalapy.interpreter.{CPythonInterpreter, PyValue}
           |trait FunctionReaders {
           |${methods.mkString("\n")}
           |}""".stripMargin
    
      IO.write(fileToWrite, toWrite)
      Seq(fileToWrite)
    },
    sourceGenerators in Compile += Def.task  {
      val fileToWrite = (sourceManaged in Compile).value / "FunctionWriters.scala"
      val methods = (0 to 22).map { n =>
        val seqArgs = (1 to n).map(t => s"r$t.read(args(${t - 1}))").mkString(", ")
        s"""implicit def function${n}Writer[${((1 to n).map(t => s"T$t") :+ "O").mkString(", ")}](implicit ${((1 to n).map(t => s"r$t: Reader[T$t]") :+ "oWriter: Writer[O]").mkString(", ")}): Writer[(${(1 to n).map(t => s"T$t").mkString(", ")}) => O] = {
           |  new Writer[(${(1 to n).map(t => s"T$t").mkString(", ")}) => O] {
           |    override def write(v: (${(1 to n).map(t => s"T$t").mkString(", ")}) => O): PyValue = {
           |      CPythonInterpreter.createLambda(args => oWriter.write(v($seqArgs)))
           |    }
           |  }
           |}"""
      }
    
      val toWrite =
        s"""package me.shadaj.scalapy.readwrite
           |import me.shadaj.scalapy.interpreter.{CPythonInterpreter, PyValue}
           |trait FunctionWriters {
           |${methods.mkString("\n")}
           |}""".stripMargin
    
      IO.write(fileToWrite, toWrite)
      Seq(fileToWrite)
    }
  ).settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    unmanagedSourceDirectories in Compile += {
      val sharedSourceDir = (baseDirectory in ThisBuild).value / "core/shared/src/main"
      if (scalaVersion.value.startsWith("2.13.")) sharedSourceDir / "scala-2.13"
      else sharedSourceDir / "scala-2.11_2.12"
    }
  ).jvmSettings(
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies += "net.java.dev.jna" % "jna" % "5.6.0",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.2" % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
    fork in Test := true,
    javaOptions in Test += s"-Djna.library.path=${"python3-config --prefix".!!.trim}/lib"
  ).nativeSettings(
    scalaVersion := scala211Version,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.1.0-RC3" % Test,
    libraryDependencies += "com.github.lolgab" %%% "scalacheck" % "1.14.1" % Test,
    nativeLinkStubs := true,
    nativeLinkingOptions ++= "python3-config --ldflags".!!.split(' ').map(_.trim).filter(_.nonEmpty).toSeq
  )

lazy val coreJVM = core.jvm
lazy val coreNative = core.native

lazy val docs = project
  .in(file("built-docs"))
  .settings(
    moduleName := "built-docs",
  )
  .enablePlugins(MdocPlugin, DocusaurusPlugin)
  .dependsOn(coreJVM)
  .settings(
    fork := true,
    connectInput := true,
    javaOptions += s"-Djna.library.path=${"python3-config --prefix".!!.trim}/lib",
    docusaurusCreateSite := {
      mdoc.in(Compile).toTask(" ").value
      Process(List("yarn", "install"), cwd = DocusaurusPlugin.website.value).!
      Process(List("yarn", "run", "build"), cwd = DocusaurusPlugin.website.value).!
      val out = DocusaurusPlugin.website.value / "build"
      out
    }
  )

lazy val bench = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("bench"))
  .settings(
    name := "scalapy-bench"
  ).jvmSettings(
    fork := true,
    crossScalaVersions := supportedScalaVersions,
    javaOptions += s"-Djna.library.path=${"python3-config --prefix".!!.trim}/lib"
  ).nativeSettings(
    scalaVersion := scala211Version,
    nativeLinkingOptions ++= "python3-config --ldflags".!!.split(' ').map(_.trim).filter(_.nonEmpty).toSeq,
    nativeMode := "release-fast"
  ).dependsOn(core)

lazy val benchJVM = bench.jvm
lazy val benchNative = bench.native
