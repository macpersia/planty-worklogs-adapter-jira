package com.github.macpersia.planty_jira_view

import java.io.{File, PrintStream}
import java.net.URI
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatter.ofPattern
import java.time.{LocalDate, ZoneId}
import java.util
import java.util.Collections._
import java.util._
import java.util.concurrent.TimeUnit.MINUTES

import com.typesafe.scalalogging.LazyLogging
import play.api.libs.json.{JsError, JsSuccess}
import play.api.libs.ws.WS
import play.api.libs.ws.WSAuthScheme.BASIC
import play.api.libs.ws.ning.NingWSClient
import resource.managed

import scala.collection.JavaConversions._
import scala.collection.immutable
import scala.collection.parallel.immutable.ParSeq
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, ExecutionContext}

case class ConnectionConfig( baseUri: URI,
                             username: String,
                             password: String)

case class WorklogFilter( jiraQuery: String,
                          author: Option[String],
                          fromDate: LocalDate,
                          toDate: LocalDate,
                          timeZone: TimeZone)

case class WorklogEntry( date: LocalDate,
                         description: String,
                         duration: Double)

object WorklogReporter extends LazyLogging {
  val DATE_FORMATTER = DateTimeFormatter.ISO_DATE
}

import com.github.macpersia.planty_jira_view.model._

class WorklogReporter(connConfig: ConnectionConfig, filter: WorklogFilter)
                     (implicit execContext: ExecutionContext)
  extends LazyLogging with AutoCloseable {

  val zoneId = filter.timeZone.toZoneId
  val cacheManager = new CacheManager()

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

    val latestIssueTs = Await.result(
      cacheManager.latestIssueTimestamp(),
      Duration(10, SECONDS))
    logger.debug(s"Previous timestamp for updates: ${latestIssueTs}")

    logger.debug(s"Searching the JIRA at ${connConfig.baseUri} as ${connConfig.username}")

    val reqTimeout = Duration(1, MINUTES)

    val userUrl = connConfig.baseUri.toString + s"/rest/api/2/user?username=${connConfig.username}"
    val userReq = WS.clientUrl(userUrl)
                    .withAuth(connConfig.username, connConfig.password, BASIC)
                    .withHeaders("Content-Type" -> "application/json")
    val userFuture = userReq.get()
    val userResp = Await.result(userFuture, reqTimeout)
    val userResult = userResp.json.validate[User].get
    logger.debug("Current user's time zone: " + ZoneId.of(userResult.timeZone.get))

    val dateTimeFormatter: DateTimeFormatter = ofPattern("yyyy-MM-dd")
    val fromDateFormatted: String = dateTimeFormatter.format(filter.fromDate)
    val toDateFormatted: String = dateTimeFormatter.format(filter.toDate)

    val searchUrl = connConfig.baseUri.toString + "/rest/api/2/search"
    val jql: String = Seq(filter.jiraQuery, s"updated>=${fromDateFormatted} AND created<=${toDateFormatted}")
      .mkString(" AND ")
    val searchReq = WS.clientUrl(searchUrl)
                    .withAuth(connConfig.username, connConfig.password, BASIC)
                    .withHeaders("Content-Type" -> "application/json")
                    .withQueryString(
                      "jql" -> jql,
                      "maxResults" -> "1000",
                      "fields" -> "updated,created"
                    )
    val searchFuture = searchReq.get()
    val searchResp = Await.result(searchFuture, reqTimeout)
    searchResp.json.validate[SearchResult] match {
      case JsSuccess(searchResult, path) => {
        val worklogsMap: util.Map[Worklog, BasicIssue] = exxtractWorklogs(searchResult)
        return toWorklogEntries(worklogsMap)
      }
      case JsError(errors) => {
        for (e <- errors) logger.error(e.toString())
        logger.debug("The body of search response: \n" + searchResp.body)
        throw new RuntimeException("Search Failed!")
      }
    }
  }

  def toWorklogEntries(worklogsMap: util.Map[Worklog, BasicIssue]): Seq[WorklogEntry] = {
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

  def exxtractWorklogs(searchResult: SearchResult) : util.Map[Worklog, BasicIssue] = {

    val worklogsMap: util.Map[Worklog, BasicIssue] = synchronizedMap(new util.HashMap)
    val myWorklogs: util.List[Worklog] = synchronizedList(new util.LinkedList)

    for (issue <- searchResult.issues.par) {
      val prevSyncTs = Await.result(cacheManager.latestIssueTimestamp(), Duration(30, SECONDS))
      val cachedIssue = Await.result(cacheManager.getIssueById(issue.id), Duration(30, SECONDS))
      val allWorklogs =
        if (prevSyncTs.isEmpty || cachedIssue.isEmpty || (issue.updated isAfter cachedIssue.get.updated)) {
          //val issueUrl = connConfig.baseUri.toString + s"/rest/api/2/issue/${issue.key}"
          //logger.debug(s"Retrieving issue ${issue.key} at $issueUrl")
          //
          //val issueReq = WS.clientUrl(issueUrl)
          //  .withAuth(connConfig.username, connConfig.password, BASIC)
          //  .withHeaders("Content-Type" -> "application/json")
          // val searchFuture = issueReq.get()
          // val searchResp = Await.result(searchFuture, reqTimeout)
          // val issueResult = searchResp.json.validate[FullIssue].get
          retrieveWorklogsFromRestAPI(issue, connConfig.username, connConfig.password)
        } else
          Await.result(cacheManager.listWorklogs(issue.key), Duration(30, SECONDS))

      for (worklog <- allWorklogs) {
        val author = filter.author match {
          case None => connConfig.username
          case Some(username) => if (!username.trim.isEmpty) username else connConfig.username
        }
        if (isLoggedBy(author, worklog)
          && isWithinPeriod(filter.fromDate, filter.toDate, worklog)) {

          myWorklogs.add(worklog)
          worklogsMap.put(worklog, issue)
        }
      }
    }
    return worklogsMap
  }

  private def retrieveWorklogsFromRestAPI(issue: BasicIssue, username: String, password: String): ParSeq[Worklog] = {

    val worklogsUrl = s"${connConfig.baseUri}/rest/api/2/issue/${issue.key}/worklog"
    val reqTimeout = Duration(1, MINUTES)
    val worklogsReq = WS.clientUrl(worklogsUrl)
                    .withAuth(connConfig.username, connConfig.password, BASIC)
                    .withHeaders("Content-Type" -> "application/json")
                    .withQueryString("maxResults" -> "1000")
    val respFuture = worklogsReq.get()
    val resp = Await.result(respFuture, reqTimeout)

    resp.json.validate[IssueWorklogs] match {
      case JsSuccess(issueWorklogs, path) => {
        val enhancedWorklogs = issueWorklogs.worklogs.map(_.map(w => w.copy(issueKey = Option(issue.key))))
        val enhancedIssueWorklogs = issueWorklogs.copy(issueKey = Some(issue.key), worklogs = enhancedWorklogs)
        cacheManager.updateIssueWorklogs(enhancedIssueWorklogs)
        cacheManager.updateIssue(issue)
        return (enhancedIssueWorklogs.worklogs getOrElse immutable.Seq.empty).par
      }
      case JsError(errors) => {
        for (e <- errors) logger.error(e.toString())
        logger.debug("The body of search response: \n" + resp.body)
        throw new RuntimeException("Retrieving Worklogs Failed!")
      }
    }
  }

  def isLoggedBy(username: String, worklog: Worklog): Boolean = {
    worklog.author.name.equalsIgnoreCase(username)
  }

  def isWithinPeriod(fromDate: LocalDate, toDate: LocalDate, worklog: Worklog): Boolean = {
    val startDate = worklog.started.atStartOfDay(zoneId).toLocalDate
    startDate.isEqual(fromDate) || startDate.isEqual(toDate) ||
      (startDate.isAfter(fromDate) && startDate.isBefore(toDate))
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
