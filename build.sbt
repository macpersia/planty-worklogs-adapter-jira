name := "planty-jira-view"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  //   "net.sf.jopt-simple" % "jopt-simple" % "4.9",
  //   "org.apache.karaf.shell" % "org.apache.karaf.shell.console" % "2.4.3",
  "com.github.scopt" %% "scopt" % "3.3.0",
  "com.atlassian.jira" % "jira-rest-java-client" % "1.0"
  // "com.atlassian.jira" %% "jira-rest-java-client" % "1.0"
)

// resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
resolvers ++= Seq(
  Resolver.mavenLocal,
  "Atlassian - Public" at "https://maven.atlassian.com/public",
  "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/",
  Resolver.sonatypeRepo("public")
)

// enablePlugins(UniversalPlugin)
enablePlugins(JavaAppPackaging)


fork in run := true