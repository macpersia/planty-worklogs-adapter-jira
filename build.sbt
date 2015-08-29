name := "planty-jira-view"

version := "1.0"

scalaVersion := "2.11.7"

// resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
resolvers ++= Seq(
    Resolver.mavenLocal,
    "Atlassian - Public" at "https://maven.atlassian.com/public"
)

libraryDependencies ++= Seq(
	"com.atlassian.jira" % "jira-rest-java-client" % "1.0"
    // "com.atlassian.jira" %% "jira-rest-java-client" % "1.0"
)



