package com.github.macpersia.planty.views.jira

import java.io.{File, FileNotFoundException}
import java.net.{URI, URISyntaxException}
import java.time.LocalDate
import java.util.TimeZone

import com.github.macpersia.planty.views.jira.JiraWorklogReporter._
import com.github.macpersia.planty.views.jira.model.JiraWorklogFilter
import com.github.macpersia.planty.worklogs.model.WorklogFilter
import com.typesafe.scalalogging.LazyLogging
import resource._
import scopt.OptionParser

import scala.concurrent.ExecutionContext.Implicits._

object App extends LazyLogging {

  @throws(classOf[URISyntaxException])
  @throws(classOf[FileNotFoundException])
  def main(args: Array[String]) {

    val parser = new OptionParser[AppParams]("scopt") {
      head("scopt", "3.x")
      opt[String]('b', "baseUrl") action { (x, c) => c.copy(baseUrl = new URI(x)) }
      opt[String]('u', "username") action { (x, c) => c.copy(username = x) }
      opt[String]('p', "password") action { (x, c) => c.copy(password = x) }
      opt[String]('q', "query") action { (x, c) => c.copy(jiraQuery = x) }
      opt[String]('a', "author") action { (x, c) => c.copy(author = Some(x)) }
      opt[String]('f', "fromDate") action { (x, c) => c.copy(fromDate =
        LocalDate.parse(x, DATE_FORMATTER))
      }
      opt[String]('t', "toDate") action { (x, c) => c.copy(toDate =
        LocalDate.parse(x, DATE_FORMATTER))
      }
      opt[String]('z', "timeZone") action { (x, c) => c.copy(timeZone = TimeZone.getTimeZone(x))}
      opt[String]('o', "outputFile") action { (x, c) => c.copy(outputFile = Some(new File(x))) }
    }
    parser.parse(args, AppParams()) match {
      case None => return
      case Some(params) => {
        val connConfig = ConnectionConfig(
              params.baseUrl,
              params.username,
              if (params.password != null) params.password else promptForPassword
        )
        val filter: JiraWorklogFilter = JiraWorklogFilter(
          params.author, params.fromDate, params.toDate, params.timeZone, params.jiraQuery)

        for (reporter <- managed(new JiraWorklogReporter(connConfig, filter)(global))) {
          reporter.printWorklogsAsCsv(params.outputFile)
          sys.exit(0)
        }
      }
    }
  }

  def promptForPassword: String = {
    String valueOf System.console.readPassword("Please enter you password: ")
  }
}

case class AppParams(
                      baseUrl: URI = new URI("https://jira.atlassian.com"),
                      // baseUrl: URI = new URI("https://jira02.jirahosting.de/jira"),
                      username: String = null,
                      password: String = null,
                      jiraQuery: String =
                      "project = CLOUD",
                      //"project = BICM AND labels = 2015 AND labels IN ('#8', '#9') AND summary ~ 'Project Management'",
                      author: Option[String] = None,
                      // fromDate: DateTime = dateFormatter parseDateTime "2015-08-16" ,
                      // toDate: DateTime = dateFormatter parseDateTime "2015-09-22",
                      fromDate: LocalDate = LocalDate.now minusWeeks 1,
                      toDate: LocalDate = LocalDate.now plusDays 1,
                      timeZone: TimeZone = TimeZone.getDefault,
                      outputFile: Option[File] = None )
