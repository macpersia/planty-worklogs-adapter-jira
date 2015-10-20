package com.github.macpersia.planty_jira_view

import java.io.{File, PrintStream}
import java.net.URI
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}
import java.util
import java.util.Collections._
import java.util._
import java.util.concurrent.TimeUnit.MINUTES

import com.github.macpersia.planty_jira_view.model._
import com.typesafe.scalalogging.LazyLogging
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.libs.ws.WSAuthScheme.BASIC
import play.api.libs.ws.ning.NingWSClient
import resource.managed

import scala.collection.JavaConversions._
import scala.collection.immutable
import scala.collection.parallel.immutable.ParSeq
import scala.concurrent.Await
import scala.concurrent.duration.Duration

case class ConnectionConfig(
                             baseUri: URI,
                             username: String,
                             password: String)

case class WorklogFilter(
                          jiraQuery: String,
                          author: Option[String],
                          fromDate: LocalDate,
                          toDate: LocalDate,
                          timeZone: TimeZone)

case class WorklogEntry(
                         date: LocalDate,
                         description: String,
                         duration: Double)

object WorklogReporter extends LazyLogging {
  val DATE_FORMATTER = DateTimeFormatter.ISO_DATE

  private val jiraDTFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSZZ")

  implicit val readsLocalDate = Reads[LocalDate] ( js =>
    js.validate[String].map[LocalDate]      (dtString => LocalDate.parse(dtString, jiraDTFormatter))
  )
  implicit val readsZonedDateTime = Reads[ZonedDateTime] ( js =>
    js.validate[String].map[ZonedDateTime]  (dtString => ZonedDateTime.parse(dtString, jiraDTFormatter))
  )
  implicit val basicIssueReads = Json.reads[BasicIssue]
  implicit val searchResultReads = Json.reads[SearchResult]
  implicit val issueTypeReads = Json.reads[IssueType]
  implicit val statusReads = Json.reads[Status]
  implicit val userReads = Json.reads[User]
  implicit val priorityReads = Json.reads[Priority]
  implicit val issueFieldsReads = Json.reads[IssueFields]
  implicit val fullIssueReads = Json.reads[FullIssue]

  implicit val worklogReads = Json.reads[Worklog]
  implicit val issueWorklogsReads = Json.reads[IssueWorklogs]

  implicit val basicIssueWrites = Json.writes[BasicIssue]
  implicit val searchResultWrites = Json.writes[SearchResult]
  implicit val issueTypeWrites = Json.writes[IssueType]
  implicit val statusWrites = Json.writes[Status]
  implicit val userWrites = Json.writes[User]
  implicit val priorityWrites = Json.writes[Priority]
  implicit val issueFieldsWrites = Json.writes[IssueFields]
  implicit val fullIssueWrites = Json.writes[FullIssue]
  implicit val worklogWrites = Json.writes[Worklog]
  implicit val issueWorklogsWrites = Json.writes[IssueWorklogs]
}

import com.github.macpersia.planty_jira_view.WorklogReporter._

class WorklogReporter(connConfig: ConnectionConfig, filter: WorklogFilter)
  extends LazyLogging with AutoCloseable {

  // val dateTZ = DateTimeZone.forTimeZone(filter.timeZone)
  val zoneId = filter.timeZone.toZoneId

  implicit val sslClient = NingWSClient()

  override def close(): Unit = {
    if (sslClient != null) sslClient.close()
  }

  case class WorklogComparator(worklogsMap: util.Map[Worklog, BasicIssue]) extends Comparator[Worklog] {
    def compare(w1: Worklog, w2: Worklog) = {
      Ordering[(Long, String)].compare(
        (w1.started.toEpochDay, worklogsMap.get(w1).key),
        (w2.started.toEpochDay, worklogsMap.get(w2).key)
      )
    }
  }

  def printWorklogsAsCsv(outputFile: Option[File]) {
    for (csvPrintStream <- managed(
         if (outputFile.isDefined) new PrintStream(outputFile.get)
         else Console.out )) {
      for (entry <- retrieveWorklogs())
        printWorklogAsCsv(entry, csvPrintStream, WorklogReporter.DATE_FORMATTER)
    }
  }

  private def printWorklogAsCsv(entry: WorklogEntry, csvPs: PrintStream, formatter: DateTimeFormatter) {
    val date = formatter format entry.date
    csvPs.println(s"$date, ${entry.description}, ${entry.duration}")
  }

  def retrieveWorklogs(): Seq[WorklogEntry] = {

    logger.debug(s"Searching the JIRA at ${connConfig.baseUri} as ${connConfig.username}")

    val reqTimeout = Duration(1, MINUTES)
    val worklogsMap: util.Map[Worklog, model.BasicIssue] = synchronizedMap(new util.HashMap)

    val searchUrl = connConfig.baseUri.toString + "/rest/api/2/search"
    val searchReq = WS.clientUrl(searchUrl)
                    .withAuth(connConfig.username, connConfig.password, BASIC)
                    .withHeaders("Content-Type" -> "application/json")
                    .withQueryString(
                      ("jql", filter.jiraQuery),
                      ("fields", "id,key")
                    )
    val respFuture = searchReq.get()
    val resp = Await.result(respFuture, reqTimeout)
    val searchResult = resp.json.validate[SearchResult].get

    for (basicIssue <- searchResult.issues) {
      val issueUrl = connConfig.baseUri.toString + s"/rest/api/2/issue/${basicIssue.key}"
      logger.debug(s"Retrieving issue ${basicIssue.key} at $issueUrl")

      val issueReq = WS.clientUrl(issueUrl)
        .withAuth(connConfig.username, connConfig.password, BASIC)
        .withHeaders("Content-Type" -> "application/json")
      val respFuture = issueReq.get()
      val resp = Await.result(respFuture, reqTimeout)

      val issueResult = resp.json.validate[FullIssue].get

      val myWorklogs: util.List[model.Worklog] = synchronizedList(new util.LinkedList)
      for (worklog <- retrieveWorklogs(basicIssue.key, connConfig.username, connConfig.password)) {
        val author = filter.author match {
          case None => connConfig.username
          case Some(username) => if (!username.trim.isEmpty) username else connConfig.username
        }
        if (isLoggedBy(author, worklog)
          && isWithinPeriod(filter.fromDate, filter.toDate, worklog)) {

          myWorklogs.add(worklog)
          worklogsMap.put(worklog, basicIssue)
        }
      }
    }

    if (worklogsMap.isEmpty)
      return Seq.empty
    else {
      val sortedWorklogsMap: util.SortedMap[Worklog, BasicIssue] = new util.TreeMap(new WorklogComparator(worklogsMap))
      sortedWorklogsMap.putAll(worklogsMap)
      val worklogEntries =
        for (worklog <- sortedWorklogsMap.keySet.iterator)
          yield toWorklogEntry(sortedWorklogsMap, worklog)

      return worklogEntries.toSeq
    }
  }

  private def retrieveWorklogs(issueKey: String, username: String, password: String): ParSeq[Worklog] = {

    val worklogsUrl = s"${connConfig.baseUri}/rest/api/2/issue/$issueKey/worklog"
    val reqTimeout = Duration(1, MINUTES)
    val worklogsReq = WS.clientUrl(worklogsUrl)
                    .withAuth(connConfig.username, connConfig.password, BASIC)
                    .withHeaders("Content-Type" -> "application/json")
    val respFuture = worklogsReq.get()
    val resp = Await.result(respFuture, reqTimeout)

    val issueWorklogsResult = resp.json.validate[IssueWorklogs].get
    return (issueWorklogsResult.worklogs getOrElse immutable.Seq.empty).par
  }

  def isLoggedBy(username: String, worklog: Worklog): Boolean = {
    worklog.author.name.equalsIgnoreCase(username)
  }

  def isWithinPeriod(fromDate: LocalDate, toDate: LocalDate, worklog: Worklog): Boolean = {
    val startDate = worklog.started.atStartOfDay(zoneId).toLocalDate
    startDate.isEqual(fromDate) || startDate.isAfter(fromDate) && startDate.isBefore(toDate)
  }

  def toWorklogEntry(sortedReverseMap: util.SortedMap[Worklog, BasicIssue], worklog: Worklog) = {
    val issueKey = sortedReverseMap.get(worklog).key
    val secondsSpent = worklog.timeSpentSeconds.toDouble
    val hoursPerLog = secondsSpent / 60 / 60
    new WorklogEntry(
      date = worklog.started.atStartOfDay(zoneId).toLocalDate,
      description = issueKey,
      duration = hoursPerLog)
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
