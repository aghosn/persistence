import sbt._
import Keys._

object build extends Build {
  lazy val sharedSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.11.0-RC1",
    crossVersion := CrossVersion.full,
    version := "0.1.0-SNAPSHOT",
    organization := "org.scalareflect",
    description := "AST persistence for Project Palladium",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    publishMavenStyle := true,
    publishArtifact in Compile := false,
    publishArtifact in Test := false,
    scalacOptions ++= Seq("-deprecation", "-feature", "-optimise"),
    parallelExecution in Test := false, // hello, reflection sync!!
    logBuffered := false,
    scalaHome := {
      val scalaHome = System.getProperty("persistence.scala.home")
      if (scalaHome != null) {
        println(s"Going for custom scala home at $scalaHome")
        Some(file(scalaHome))
      } else None
    }
    // TODO: how to I make this recursion work?
    // run <<= run in Compile in sandbox,
    // test <<= test in Test in tests
  )

  lazy val publishableSettings = sharedSettings ++ Seq(
    publishMavenStyle := true,
    publishArtifact in Compile := true,
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    pomIncludeRepository := { x => false },
    pomExtra := (
      <url>https://github.com/scalareflect/persistence</url>
      <inceptionYear>2014</inceptionYear>
      <licenses>
        <license>
          <name>BSD-like</name>
          <url>http://www.scala-lang.org/downloads/license.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/scalareflect/persistence.git</url>
        <connection>scm:git:git://github.com/scalareflect/persistence.git</connection>
      </scm>
      <issueManagement>
        <system>GitHub</system>
        <url>https://github.com/scalareflect/persistence/issues</url>
      </issueManagement>
    ),
    credentials ++= loadCredentials()
  )

  def loadCredentials(): List[Credentials] = {
    val mavenSettingsFile = System.getProperty("maven.settings.file")
    if (mavenSettingsFile != null) {
      println("Loading Sonatype credentials from " + mavenSettingsFile)
      try {
        import scala.xml._
        val settings = XML.loadFile(mavenSettingsFile)
        def readServerConfig(key: String) = (settings \\ "settings" \\ "servers" \\ "server" \\ key).head.text
        List(Credentials(
          "Sonatype Nexus Repository Manager",
          "oss.sonatype.org",
          readServerConfig("username"),
          readServerConfig("password")
        ))
      } catch {
        case ex: Exception =>
          println("Failed to load Maven settings from " + mavenSettingsFile + ": " + ex)
          Nil
      }
    } else {
      val mavenSettingsEnv = sys.env.get("MAVEN_SETTINGS_ENV")
      if (mavenSettingsEnv.isDefined) {
        println("Loading Sonatype credentials from environment variables")
        List(Credentials(
          sys.env("SCALAREFLECT_MAVEN_REALM"),
          sys.env("SCALAREFLECT_MAVEN_DOMAIN"),
          sys.env("SCALAREFLECT_MAVEN_USER"),
          sys.env("SCALAREFLECT_MAVEN_PASSWORD")
        ))
      } else {
        Nil
      }
    }
  }

  lazy val usePluginSettings = Seq(
    scalacOptions in Compile <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
      val addPlugin = "-Xplugin:" + jar.getAbsolutePath
      // Thanks Jason for this cool idea (taken from https://github.com/retronym/boxer)
      // add plugin timestamp to compiler options to trigger recompile of
      // main after editing the plugin. (Otherwise a 'clean' is needed.)
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(addPlugin, dummy)
    }
  )

  lazy val plugin = Project(
    id   = "persistence",
    base = file("plugin")
  ) settings (
    publishableSettings: _*
  ) settings (
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    scalacOptions ++= Seq()
  )

  lazy val sandbox = Project(
    id   = "sandbox",
    base = file("sandbox")
  ) settings (
    sharedSettings ++ usePluginSettings: _*
  ) settings (
    scalacOptions ++= Seq()
  )

  lazy val tests = Project(
    id   = "tests",
    base = file("tests")
  ) settings (
    sharedSettings ++ usePluginSettings: _*
  ) settings (
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.0" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
    scalacOptions ++= Seq()
  )
}
