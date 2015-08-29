package com.github.macpersia.planty_jira_view

import java.io.PrintStream
import java.net.URI
import java.util.function.BiFunction
import java.util.{Comparator, HashMap, List, Map, SortedMap}

import com.atlassian.jira.rest.client.domain.{Issue, Worklog}
import com.atlassian.jira.rest.client.internal.jersey.JerseyJiraRestClientFactory
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat}

object Utils {

  // Constants

  val NEWLINE = System.lineSeparator

  val JIRA_BASE_URL = "https://jira02.jirahosting.de/jira"
  val JIRA_BASE_URI = new URI(JIRA_BASE_URL)

  val USERNAME = System.getProperty("jira.username")
  val PASSWORD = System.getProperty("jira.password")

  val FROM_DATE = new DateTime(2015, 8, 1, 0, 0, 0, 0)
  val TO_DATE = new DateTime(2015, 9, 1, 0, 0, 0, 0)

  val DATE_FORMATTER = ISODateTimeFormat.date

  val REVERSE_MAP: Map[Worklog, Issue] = new HashMap

  val FACTORY = new JerseyJiraRestClientFactory
  val JIRA_CLIENT = FACTORY.createWithBasicHttpAuthentication(JIRA_BASE_URI, USERNAME, PASSWORD)

  val JIRA_QUERY = "project = BICM AND labels = 2015 AND labels IN (\"#6\", \"#7\", \"#8\") AND summary ~ \"Project Management\""

  // Functions

  def isLoggedBy(username: String, w: Worklog): Boolean = {
    return w.getAuthor.getName.equalsIgnoreCase(username)
  }

  def isWithinPeriod(fromDate: DateTime, toDate: DateTime, w: Worklog): Boolean = {
    return w.getStartDate.isAfter(fromDate) && w.getStartDate.isBefore(toDate)
  }

  def createComparator(reverseMap: Map[Worklog, Issue]): Comparator[Worklog] = {
    object WorklogComparator extends Comparator[Worklog] {
      override def compare(w1: Worklog, w2: Worklog) = {
        val startDate1 = w1.getStartDate().toDateMidnight()
        val startDate2 = w2.getStartDate().toDateMidnight()
        val dateDelta = startDate1.compareTo(startDate2)
        if (dateDelta != 0)
          dateDelta
        else {
          val k1 = REVERSE_MAP.get(w1).getKey()
          val k2 = REVERSE_MAP.get(w2).getKey()
          k1.compareTo(k2)
        }
      }
    }
    WorklogComparator
  }

  def printToCsv(dateFormatter: DateTimeFormatter, csvPs: PrintStream, sortedReverseMap: SortedMap[Worklog, Issue], worklog: Worklog) {
    val issueKey: String = sortedReverseMap.get(worklog).getKey
    val startDate: String = dateFormatter.print(worklog.getStartDate)
    val minutesPerLog: Int = worklog.getMinutesSpent
    val hoursPerLog: Double = (minutesPerLog.toDouble) / 60
    csvPs.println(startDate + "," + issueKey + "," + hoursPerLog)
  }

  def toFuzzyDuration(totalMinutes: Int): String = {
    val hours: Int = totalMinutes / 60
    val minutes: Int = totalMinutes % 60
    val fuzzyDuration: String = hours + " h" + (if (minutes > 0) ", " + minutes + " m" else "")
    return fuzzyDuration
  }

  def printWorklogs(issue: Issue, myWorklogs: List[Worklog], sumAndLogFunc: BiFunction[List[Worklog], StringBuilder, Integer]) {
    val logBuilder: StringBuilder = new StringBuilder
    logBuilder.append("BICM: " + issue.getKey + " " + issue.getSummary).append(NEWLINE)
    val sumOfMinutes: Int = sumAndLogFunc.apply(myWorklogs, logBuilder)
    logBuilder.append("\t ---------------").append(NEWLINE)
    logBuilder.append("\t >>>   Subtotal: " + toFuzzyDuration(sumOfMinutes)).append(NEWLINE)
    logBuilder.append(NEWLINE)
    System.out.println(logBuilder.toString)
  }
}

