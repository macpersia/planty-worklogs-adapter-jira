package com.github.macpersia.planty_jira_view

import java.io.{File, FileNotFoundException}
import java.net._
import com.typesafe.scalalogging.LazyLogging
import org.joda.time.{DateTime, LocalDate}
import scopt.OptionParser
import com.github.macpersia.planty_jira_view.ReportGenerator._

case class Config(baseUrl: String = "https://jira02.jirahosting.de/jira",
                  username: String = null,
                  password: String = null,
                  jiraQuery: String =
                  "project = BICM AND labels = 2015 AND labels IN ('#7', '#8') AND summary ~ 'Project Management'",
                  // fromDate: DateTime = dateFormatter parseDateTime "2015-08-16" ,
                  // toDate: DateTime = dateFormatter parseDateTime "2015-09-22",
                  fromDate: LocalDate = (new DateTime minusWeeks 1 toLocalDate),
                  toDate: LocalDate = (new DateTime plusDays 1 toLocalDate),
                  outputFile: File = null)

object Main extends LazyLogging {

  @throws(classOf[URISyntaxException])
  @throws(classOf[FileNotFoundException])
  def main(args: Array[String]) {

    val parser = new OptionParser[Config]("scopt") {
      head("scopt", "3.x")
      opt[String]('b', "baseUrl") action { (x, c) => c.copy(baseUrl = x) }
      opt[String]('u', "username") action { (x, c) => c.copy(username = x) }
      opt[String]('p', "password") action { (x, c) => c.copy(password = x) }
      opt[String]('q', "query") action { (x, c) => c.copy(jiraQuery = x) }
      opt[String]('f', "fromDate") action { (x, c) => c.copy(fromDate =
        DATE_FORMATTER parseDateTime x toLocalDate)
      }
      opt[String]('t', "toDate") action { (x, c) => c.copy(toDate =
        DATE_FORMATTER parseDateTime x toLocalDate)
      }
      opt[String]('o', "outputFile") action { (x, c) => c.copy(outputFile = new File(x)) }
    }
    parser.parse(args, Config()) match {
      case None => return
      case Some(config) => {
        val reportGenerator =
          if (config.password != null)
            new ReportGenerator(config)
          else
            new ReportGenerator(config.copy(password = promptForPassword))

        reportGenerator.printAsCsv(reportGenerator.generateEntries())
      }
    }
  }

  def promptForPassword: String = {
    String valueOf System.console.readPassword("Please enter you password: ")
  }
}

