package com.github.macpersia.planty_jira_view

import java.io.{FileNotFoundException, PrintStream}
import java.net.{URI, URISyntaxException}
import java.util._

import com.atlassian.jira.rest.client.domain.{Issue, Worklog}
import com.atlassian.jira.rest.client.internal.jersey.JerseyJiraRestClientFactory
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

import scala.collection.JavaConversions._

/**
 * Created by hadi on 8/18/15.
 */
object Main {
  @throws(classOf[URISyntaxException])
  @throws(classOf[FileNotFoundException])
  def main(args: Array[String]) {

    val baseUrl = "https://jira02.jirahosting.de/jira"
    val username = System.getProperty("jira.username")
    val password = System.getProperty("jira.password")

    val fromDate = new DateTime(2015, 8, 1, 0, 0, 0, 0)
    val toDate = new DateTime(2015, 9, 1, 0, 0, 0, 0)

    val dateFormatter = ISODateTimeFormat.date
    val worklogsMap: Map[Worklog, Issue] = new HashMap

    val factory = new JerseyJiraRestClientFactory
    val restClient = factory.createWithBasicHttpAuthentication(new URI(baseUrl), username, password)

    // val jiraQuery = "type = Epic ORDER BY RANK ASC";
    val jiraQuery = "project = BICM AND labels = 2015 AND labels IN (\"#6\", \"#7\", \"#8\") AND summary ~ \"Project Management\""
    // val jiraQuery = "project = BICM AND labels = 2015 AND labels IN (\"#6\", \"#7\", \"#8\") AND NOT summary ~ \"Project Management\"";

    val searchResult = restClient.getSearchClient.searchJql(jiraQuery, null)
    val csvPs: PrintStream = new PrintStream("JIRA-worklogs.csv")
    try {
      for (basicIssue <- searchResult.getIssues) {
        val issue = restClient.getIssueClient.getIssue((basicIssue).getKey, null)
        val myWorklogs: List[Worklog] = new LinkedList
        for (worklog <- issue.getWorklogs) {
          //            if (worklog.getAuthor.getName.equalsIgnoreCase(username)
          //                && worklog.getStartDate.isAfter(fromDate)
          //                && worklog.getStartDate.isBefore(toDate)) {
          myWorklogs.add(worklog)
          worklogsMap.put(worklog, issue)
          //            }
        }
        if (!myWorklogs.isEmpty) {
          System.out.println("BICM: " + issue.getKey + " " + issue.getSummary)
          var accumulatedMinutes = 0
          for (worklog <- myWorklogs) {
            val minutesPerLog = worklog.getMinutesSpent
            val startDate = dateFormatter.print(worklog.getStartDate)
            System.out.println("\t >>> " + startDate + " : " + toFuzzyDuration(minutesPerLog))
            accumulatedMinutes += minutesPerLog
          }
          System.out.println("\t ---------------")
          System.out.println("\t >>>   Subtotal: " + toFuzzyDuration(accumulatedMinutes))
          System.out.println
        }
      }
      if (!worklogsMap.isEmpty) {
        object WorklogComparator extends Comparator[Worklog] {
          override def compare(w1: Worklog, w2: Worklog) = {
            val startDate1 = w1.getStartDate().toDateMidnight()
            val startDate2 = w2.getStartDate().toDateMidnight()
            val dateDelta = startDate1.compareTo(startDate2)
            if (dateDelta != 0)
              dateDelta
            else {
              val k1 = worklogsMap.get(w1).getKey()
              val k2 = worklogsMap.get(w2).getKey()
              k1.compareTo(k2)
            }
          }
        }
        val sortedWorklogsMap: SortedMap[Worklog, Issue] = new TreeMap(WorklogComparator)
        sortedWorklogsMap.putAll(worklogsMap)
        for (worklog <- sortedWorklogsMap.keySet) {
          val issueKey = sortedWorklogsMap.get(worklog).getKey
          val startDate = dateFormatter.print(worklog.getStartDate)
          val minutesPerLog = worklog.getMinutesSpent
          val hoursPerLog = (minutesPerLog.toDouble) / 60
          csvPs.println(startDate + "," + issueKey + "," + hoursPerLog)
        }
      }
    } finally {
      if (csvPs != null) csvPs.close()
    }
  }

  private def toFuzzyDuration(totalMinutes: Int): String = {
    val hours = totalMinutes / 60
    val minutes = totalMinutes % 60
    val fuzzyDuration = hours + " h" + (if (minutes > 0) ", " + minutes + " m" else "")
    return fuzzyDuration
  }
}
