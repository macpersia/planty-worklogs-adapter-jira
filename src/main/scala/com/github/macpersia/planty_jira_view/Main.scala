package com.github.macpersia.planty_jira_view

import java.io.{FileNotFoundException, PrintStream}
import java.net.URISyntaxException
import java.util._

import com.atlassian.jira.rest.client.domain.{Issue, Worklog}
import com.github.macpersia.planty_jira_view.Utils._

import scala.collection.JavaConversions._

object Main {
  @throws(classOf[URISyntaxException])
  @throws(classOf[FileNotFoundException])
  def main(args: Array[String]) {

    val searchResult = JIRA_CLIENT.getSearchClient.searchJql(JIRA_QUERY, null)
    val csvPs: PrintStream = new PrintStream("JIRA-worklogs.csv")
    try {
      for (basicIssue <- searchResult.getIssues) {
        val issue = JIRA_CLIENT.getIssueClient.getIssue((basicIssue).getKey, null)
        val myWorklogs: List[Worklog] = new LinkedList
        for (worklog <- issue.getWorklogs) {
          if (isLoggedBy(USERNAME, worklog) && isWithinPeriod(FROM_DATE, TO_DATE, worklog)) {
            myWorklogs.add(worklog)
            REVERSE_MAP.put(worklog, issue)
          }
        }
        if (!myWorklogs.isEmpty) {
          System.out.println("BICM: " + issue.getKey + " " + issue.getSummary)
          var accumulatedMinutes = 0
          for (worklog <- myWorklogs) {
            val minutesPerLog = worklog.getMinutesSpent
            val startDate = DATE_FORMATTER.print(worklog.getStartDate)
            System.out.println("\t >>> " + startDate + " : " + toFuzzyDuration(minutesPerLog))
            accumulatedMinutes += minutesPerLog
          }
          System.out.println("\t ---------------")
          System.out.println("\t >>>   Subtotal: " + toFuzzyDuration(accumulatedMinutes))
          System.out.println
        }
      }
      if (!REVERSE_MAP.isEmpty) {
        val sortedWorklogsMap: SortedMap[Worklog, Issue] = new TreeMap(createComparator(REVERSE_MAP))
        sortedWorklogsMap.putAll(REVERSE_MAP)
        for (worklog <- sortedWorklogsMap.keySet) {
          val issueKey = sortedWorklogsMap.get(worklog).getKey
          val startDate = DATE_FORMATTER.print(worklog.getStartDate)
          val minutesPerLog = worklog.getMinutesSpent
          val hoursPerLog = (minutesPerLog.toDouble) / 60
          csvPs.println(startDate + "," + issueKey + "," + hoursPerLog)
        }
      }
    } finally {
      if (csvPs != null) csvPs.close()
    }
  }
}

