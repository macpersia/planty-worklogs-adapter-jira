package com.github.macpersia.planty.views.jira

import java.io.{File, PrintStream}
import java.net.URI
import java.time.LocalTime.NOON
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatter.ofPattern
import java.time._
import java.util
import java.util.Collections._
import java.util._
import java.util.concurrent.TimeUnit.MINUTES

import com.github.macpersia.planty.views.jira.model._
import com.github.macpersia.planty.worklogs.WorklogReporting
import com.github.macpersia.planty.worklogs.model.{WorklogEntry, WorklogFilter}
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
import scala.util.{Failure, Success, Try}

case class ConnectionConfig(baseUri: URI,
                            username: String,
                            password: String
                           ) {
  val baseUriWithSlash = {
    val baseUriStr = baseUri.toString
    if (baseUriStr.endsWith("/")) baseUriStr
    else s"$baseUriStr/"
  }
}

object JiraWorklogReporter extends LazyLogging {
  val DATE_FORMATTER = DateTimeFormatter.ISO_DATE
}

class JiraWorklogReporter(connConfig: ConnectionConfig, filter: JiraWorklogFilter)
                         (implicit execContext: ExecutionContext)
  extends LazyLogging with WorklogReporting {

  val zoneId = filter.timeZone.toZoneId
  lazy val cacheManager = CacheManager.instance
  implicit val sslClient = NingWSClient()

  override def close(): Unit = {
    if (sslClient != null) sslClient.close()
  }

  class WorklogComparator(worklogsMap: util.Map[Worklog, BasicIssue])
    extends Comparator[Worklog] {

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
      else Console.out)) {
      for (entry <- retrieveWorklogs())
        printWorklogAsCsv(entry, csvPrintStream, JiraWorklogReporter.DATE_FORMATTER)
    }
  }

  private def printWorklogAsCsv(entry: WorklogEntry, csvPs: PrintStream, formatter: DateTimeFormatter) {
    val date = formatter format entry.date
    csvPs.println(s"$date, ${entry.description}, ${entry.duration}")
  }

  override def retrieveWorklogs(): Seq[WorklogEntry] = {

    val latestIssueTs = Await.result(
      cacheManager.latestIssueTimestamp(connConfig.baseUriWithSlash),
      Duration(10, SECONDS))
    logger.debug(s"Previous timestamp for updates: $latestIssueTs")

    logger.debug(s"Searching the JIRA at ${connConfig.baseUriWithSlash} as ${connConfig.username}")

    val reqTimeout = Duration(1, MINUTES)

    val userUrl = connConfig.baseUriWithSlash + s"rest/api/2/user?username=${connConfig.username}"
//    val userReq = WS.clientUrl(userUrl)
//      .withAuth(connConfig.username, connConfig.password, BASIC)
//      .withHeaders("Content-Type" -> "application/json")
//    val userFuture = userReq.get()
//    val userResp = Await.result(userFuture, reqTimeout)
//    val userResult = userResp.json.validate[User].get
//    logger.debug("Current user's time zone: " + ZoneId.of(userResult.timeZone.get))
//
//    val dateTimeFormatter: DateTimeFormatter = ofPattern("yyyy-MM-dd")
//    val fromDateFormatted: String = dateTimeFormatter.format(filter.fromDate)
//    val toDateFormatted: String = dateTimeFormatter.format(filter.toDate)
//
//    val searchUrl = connConfig.baseUriWithSlash + "rest/api/2/search"
//    val jql: String = Seq(filter.jiraQuery, s"updated>=$fromDateFormatted AND created<=$toDateFormatted")
//      .mkString(" AND ")
//    val maxResults = 1000
//    val searchReq = WS.clientUrl(searchUrl)
//      .withAuth(connConfig.username, connConfig.password, BASIC)
//      .withHeaders("Content-Type" -> "application/json")
//      .withQueryString(
//        "jql" -> jql,
//        "maxResults" -> s"$maxResults",
//        "fields" -> "updated,created"
//      )
//    def fetchMatchingIssues(startAt: Int, acc: Seq[BasicIssue]): Try[Seq[BasicIssue]] = {
//      val searchFuture = searchReq.withQueryString("startAt" -> s"$startAt").get()
//      val searchResp = Await.result(searchFuture, reqTimeout)
//      searchResp.json.validate[SearchResult] match {
//        case JsSuccess(result, path) =>
//          val issues: Seq[BasicIssue] = acc ++ result.issues
//          if (issues.size < result.total)
//            return fetchMatchingIssues(startAt = issues.size, acc = issues)
//          else
//            return Success(issues)
//
//        case JsError(errors) =>
//          for (e <- errors) logger.error(e.toString())
//          logger.debug("The body of search response: \n" + searchResp.body)
//          return Failure(new RuntimeException("Search Failed!"))
//      }
//    }
//    val issues = fetchMatchingIssues(startAt = 0, acc = Nil).get

    val issues = Nil
    val worklogsMap: util.Map[Worklog, BasicIssue] = extractWorklogs(issues, latestIssueTs)

    return toWorklogEntries(worklogsMap)
  }

  def updateWorklogHours(issueKey: String, worklogDate: LocalDate, hoursSpent: Double): Int = {
    val worklog = Await.result(
      cacheManager.listWorklogs(connConfig.baseUriWithSlash, issueKey), Duration(30, SECONDS)
    ).find(w => w.started == worklogDate)
    updateWorklogHours(issueKey, worklog.get.id, hoursSpent)
  }

  def updateWorklogHours(issueKey: String, worklogId: String, hoursSpent: Double): Int = {

    val reqTimeout = Duration(1, MINUTES)

    val updateUrl = connConfig.baseUriWithSlash + s"rest/api/2/issue/$issueKey/worklog/$worklogId"
    val updateReq = WS.clientUrl(updateUrl)
      .withAuth(connConfig.username, connConfig.password, BASIC)
      .withHeaders(
        "Content-Type" -> "application/json"
        //      ).withQueryString(
        //      "_" -> s"${nonce.toEpochSecond}"
      )

    /*
        "timeSpent": "5m",
        "timeSpentSeconds": "300",
        "author": {
            "self": "${connConfig.baseUriWithSlash}api/2/user?username=${connConfig.username}"
        },
        "updateAuthor": {
            "self": "${connConfig.baseUriWithSlash}api/2/user?username=${connConfig.username}"
        },
        "visibility": {
            "type": "group",
            "value": "jira-developers"
        },
        "comment": "Testing JIRA...",
        "id" : "${issueId}"
     */

    val secondsSpent = Math.round(hoursSpent * 60 * 60).toInt
    val updateFuture = updateReq.put(
      s"""
         |  {
         |    "timeSpentSeconds": ${secondsSpent}
         |  }
      """.stripMargin)
    val updateResp = Await.result(updateFuture, reqTimeout)
    logger.debug("The update response JSON: " + updateResp.body)
    //    logger.debug("The update response JSON: " + updateResp.json)
    //    updateResp.json.validate[CatsSearchResult] match {
    //      case JsSuccess(searchResult, path) =>
    //        val worklogsMap: util.Map[CatsWorklog, BasicIssue] = extractWorklogs(searchResult)
    //        return toWorklogEntries(worklogsMap)
    //      case JsError(errors) =>
    //        for (e <- errors) logger.error(e.toString())
    //        logger.debug("The body of search response: \n" + updateResp.body)
    //        throw new RuntimeException("Search Failed!")
    //    }
    updateResp.status
  }

  def createWorklog(issueKey: String, worklogDate: LocalDate, zone: ZoneId, hoursSpent: Double, comment: String): Int = {
    val reqTimeout = Duration(1, MINUTES)

    val createUrl = connConfig.baseUriWithSlash + s"rest/api/2/issue/$issueKey/worklog"
    val createReq = WS.clientUrl(createUrl)
      .withAuth(connConfig.username, connConfig.password, BASIC)
      .withHeaders(
        "Content-Type" -> "application/json"
      )
//    {
//        "comment": "I did some work here.",
//        "visibility": {
//            "type": "group",
//            "value": "jira-developers"
//        },
//        "started": "2016-05-18T12:19:04.126+0000",
//        "timeSpentSeconds": 12000
//    }
    val secondsSpent = Math.round(hoursSpent * 60 * 60).toInt
    val formattedDate: String = jiraDTFormatter.format(worklogDate.atStartOfDay(zone).plusHours(12))
    val createFuture = createReq.post(
      s"""
         |  {
         |    "comment": "${comment}",
         |    "started": "${formattedDate}",
         |    "timeSpentSeconds": ${secondsSpent}
         |  }
      """.stripMargin)
    val createResp = Await.result(createFuture, reqTimeout)
    logger.debug("The create response JSON: " + createResp.body)
    createResp.status
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

  def toWorklogEntry(sortedReverseMap: util.SortedMap[Worklog, BasicIssue], worklog: Worklog) = {
    val issueKey = sortedReverseMap.get(worklog).key
    val secondsSpent = worklog.timeSpentSeconds.toDouble
    val hoursPerLog = secondsSpent / 60 / 60
    new WorklogEntry(
      date = worklog.started.atStartOfDay(zoneId).toLocalDate,
      description = issueKey,
      duration = hoursPerLog)
  }

  def extractWorklogs(issues: Seq[BasicIssue], prevSyncTimestamp: Option[ZonedDateTime])
  : util.Map[Worklog, BasicIssue] = {

    val worklogsMap: util.Map[Worklog, BasicIssue] = synchronizedMap(new util.HashMap)
    val myWorklogs: util.List[Worklog] = synchronizedList(new util.LinkedList)

    val baseUrlOption = Option(connConfig.baseUriWithSlash)
    for (issue <- issues.map(_.copy(baseUrl = baseUrlOption)).par) {
      val cachedIssue = Await.result(
        cacheManager.getIssueByBaseUrlAndId(issue.baseUrl.get, issue.id),
        Duration(30, SECONDS)
      )
      val allWorklogs =
        if (prevSyncTimestamp.isEmpty || cachedIssue.isEmpty || (issue.updated isAfter cachedIssue.get.updated)) {
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
          Await.result(cacheManager.listWorklogs(issue.baseUrl.get, issue.key), Duration(30, SECONDS))

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

    val worklogsUrl = s"${connConfig.baseUriWithSlash}rest/api/2/issue/${issue.key}/worklog"
    val reqTimeout = Duration(2, MINUTES)
    val worklogsReq = WS.clientUrl(worklogsUrl)
      .withAuth(connConfig.username, connConfig.password, BASIC)
      .withHeaders("Content-Type" -> "application/json")
      .withQueryString("maxResults" -> "1000")
    val respFuture = worklogsReq.get()
    val resp = Await.result(respFuture, reqTimeout)

    resp.json.validate[IssueWorklogs] match {
      case JsSuccess(issueWorklogs, path) =>
        val baseUrl = connConfig.baseUriWithSlash
        val enhancedWorklogs = issueWorklogs.worklogs.map(_.map(w => w.copy(
          issueKey = Option(issue.key), baseUrl = Option(baseUrl)
        )))
        val enhancedIssueWorklogs = issueWorklogs.copy(
          baseUrl = Option(baseUrl), issueKey = Option(issue.key), worklogs = enhancedWorklogs
        )
        cacheManager.updateIssueWorklogs(enhancedIssueWorklogs) onSuccess {
          case lastError => if (lastError.ok)
            cacheManager.updateIssue(issue)
        }
        return (enhancedIssueWorklogs.worklogs getOrElse immutable.Seq.empty).par
      case JsError(errors) =>
        for (e <- errors) logger.error(e.toString())
        logger.debug("The body of search response: \n" + resp.body)
        throw new RuntimeException("Retrieving Worklogs Failed!")
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

  def toFuzzyDuration(totalMinutes: Int): String = {
    val hours = totalMinutes / 60
    val minutes = totalMinutes % 60
    if (minutes == 0)
      s"$hours h"
    else
      s"$hours h, $minutes m"
  }
}
