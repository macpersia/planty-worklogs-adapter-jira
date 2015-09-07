package com.github.macpersia.planty_jira_view

import java.io.PrintStream
import java.net.URI
import java.util
import java.util.Collections._
import java.util._

import com.atlassian.jira.rest.client.domain.{Issue, Worklog}
import com.atlassian.jira.rest.client.internal.jersey.JerseyJiraRestClientFactory
import com.atlassian.jira.rest.client.internal.json.WorklogJsonParser
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.httpclient.auth.AuthScope
import org.apache.commons.httpclient.methods.GetMethod
import org.apache.commons.httpclient.{HttpClient, UsernamePasswordCredentials}
import org.codehaus.jettison.json.{JSONArray, JSONObject}
import org.joda.time.LocalDate
import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat}

import scala.collection.JavaConversions._
import scala.collection.parallel.immutable.ParSeq
import scala.io.Source

import ReportGenerator._


case class WorklogEntry(date: LocalDate, description: String, duration: Double)

object ReportGenerator {
  val DATE_FORMATTER = ISODateTimeFormat.date
}

class ReportGenerator(config: Config) extends LazyLogging {

  case class WorklogComparator(worklogsMap: Map[Worklog, Issue]) extends Comparator[Worklog] {
    def compare(w1: Worklog, w2: Worklog) = {
      val millis1 = w1.getStartDate.toDateMidnight.getMillis
      val millis2 = w2.getStartDate.toDateMidnight.getMillis
      val issueKey1 = worklogsMap.get(w1).getKey
      val issueKey2 = worklogsMap.get(w2).getKey
      Ordering[(Long, String)].compare((millis1, issueKey1), (millis2, issueKey2))
    }
  }

  def printAsCsv(entries: Seq[WorklogEntry]) {
    val csvPrintStream =
      if (config.outputFile != null) new PrintStream(config.outputFile)
      else Console.out
    try
      for (entry <- entries) printAsCsv(entry, csvPrintStream, DATE_FORMATTER)
    finally
      if (csvPrintStream != null && config.outputFile == null) csvPrintStream.close()
  }

  private def printAsCsv(entry: WorklogEntry, csvPs: PrintStream, formatter: DateTimeFormatter) {
    val date = formatter print entry.date
    csvPs.println(s"${date}, ${entry.description}, ${entry.duration}")
  }

  def generateEntries(): Seq[WorklogEntry] = {

    logger.debug(s"Connecting to ${config.baseUrl} as ${config.username}")

    val worklogsMap: Map[Worklog, Issue] = synchronizedMap(new HashMap)

    val factory = new JerseyJiraRestClientFactory
    val restClient = factory.createWithBasicHttpAuthentication(
      new URI(config.baseUrl), config.username, config.password)

    val searchResult = restClient.getSearchClient.searchJql(config.jiraQuery, null)
    for (basicIssue <- searchResult.getIssues.par) {
      val issue = restClient.getIssueClient.getIssue(basicIssue.getKey, null)

      val myWorklogs: util.List[Worklog] = synchronizedList(new util.LinkedList)
      for (worklog <- retrieveWorklogs(issue, config.username, config.password)) {
        if (isLoggedBy(config.username, worklog)
          && isWithinPeriod(config.fromDate, config.toDate, worklog)) {

          myWorklogs.add(worklog)
          worklogsMap.put(worklog, issue)
        }
      }
    }
    if (worklogsMap.isEmpty)
      return Seq.empty
    else {
      val sortedWorklogsMap: SortedMap[Worklog, Issue] = new TreeMap(new WorklogComparator(worklogsMap))
      sortedWorklogsMap.putAll(worklogsMap)
      val worklogEntries =
        for (worklog <- sortedWorklogsMap.keySet)
          yield toWorklogEntry(sortedWorklogsMap, worklog)

      return worklogEntries.toSeq
    }
  }

  private def retrieveWorklogs(issue: Issue, username: String, password: String): ParSeq[Worklog] = {

    var getMethod: GetMethod = null
    try {
      val httpClient = new HttpClient
      getMethod = createGetMethod(httpClient, issue.getWorklogUri, username, password)
      httpClient.executeMethod(getMethod)
      val worklogsJson = new JSONObject(
        Source.fromInputStream(getMethod.getResponseBodyAsStream)("UTF-8").mkString)

      val worklogsArray = worklogsJson.getJSONArray("worklogs")
      val worklogParser = new WorklogJsonParser
      val worklogs =
        for (i <- (0 to worklogsArray.length() - 1).par)
          yield worklogParser.parse(fixWorklogJsonObject(issue, worklogsArray, i))

      return worklogs

    } finally {
      if (getMethod != null) getMethod.releaseConnection()
    }
  }

  private def fixWorklogJsonObject(issue: Issue, worklogsArray: JSONArray, i: Int): JSONObject = {

    val worklogJson = worklogsArray.getJSONObject(i)
    worklogJson.put("issue", issue.getKey)
    worklogJson.put("minutesSpent", worklogJson.getInt("timeSpentSeconds") / 60)
    return worklogJson
  }

  private def createGetMethod(httpClient: HttpClient, uri: URI, username: String, password: String): GetMethod = {

    val defaultCreds = new UsernamePasswordCredentials(username, password)
    httpClient.getParams.setAuthenticationPreemptive(true)
    httpClient.getState.setCredentials(
      new AuthScope(uri.getHost, 443, AuthScope.ANY_REALM), defaultCreds)
    val getMethod = new GetMethod(uri.toString)
    getMethod.setDoAuthentication(true)
    return getMethod
  }

  def isLoggedBy(username: String, theWorklog: Worklog): Boolean = {
    theWorklog.getAuthor.getName.equalsIgnoreCase(username)
  }

  def isWithinPeriod(fromDate: LocalDate, toDate: LocalDate, worklog: Worklog): Boolean = {
    val startDate = worklog.getStartDate.toLocalDate
    startDate.isAfter(fromDate) && startDate.isBefore(toDate)
  }

  def toWorklogEntry(sortedReverseMap: SortedMap[Worklog, Issue], worklog: Worklog) = {
    val issueKey = sortedReverseMap.get(worklog).getKey
    val minutesPerLog = worklog.getMinutesSpent
    val hoursPerLog = (minutesPerLog.toDouble) / 60
    new WorklogEntry(date = worklog.getStartDate.toLocalDate, description = issueKey, duration = hoursPerLog)
  }

  def toFuzzyDuration(totalMinutes: Int): String = {
    val hours = totalMinutes / 60
    val minutes = totalMinutes % 60
    if (minutes == 0)
      s"$hours h"
    else
      s"$hours h, $minutes m"
  }
}
