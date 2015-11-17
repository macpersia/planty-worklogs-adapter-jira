package com.github.macpersia.planty_jira_view.model

import java.time.{Instant, ZoneId, ZonedDateTime}

import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson._
import reactivemongo.core.commands._
import reactivemongo.core.nodeset.Authenticate

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.collection.parallel.immutable.ParSeq
import scala.concurrent.{ExecutionContext, Future}

object CacheManager {
  private val ctxToCacheMap = synchronized(mutable.Map[ExecutionContext, CacheManager]())
  def instance(implicit execContext: ExecutionContext): CacheManager =
    ctxToCacheMap.getOrElse(execContext, {
      val newCache = new CacheManager()(execContext)
      ctxToCacheMap.put(execContext, newCache)
      newCache
    })
}

class CacheManager private (implicit execContext: ExecutionContext) {

  private val driver = new MongoDriver

  private val mongoDbHost = sys.props.get("mongodb.host").getOrElse("localhost")
  private val mongoDbPort = sys.props.get("mongodb.port").getOrElse(27017)
  private val servers = Seq(s"$mongoDbHost:$mongoDbPort")

  private val mongoDbName = sys.props.get("mongodb.name").getOrElse("diy")
  private val mongoDbUsername = sys.props.get("mongodb.username")
  private val mongoDbPassword = sys.props.get("mongodb.password")
  private val credentials = Seq(Authenticate(mongoDbName, mongoDbUsername.orNull, mongoDbPassword.orNull))

  private val connection =
    if (mongoDbUsername.isDefined)
      driver.connection(servers, authentications = credentials)
    else
      driver.connection(servers)

  private val db = connection(mongoDbName)(execContext)
  private val issuesColl: BSONCollection = db("jira.issues")
  private val issueWorklogsColl: BSONCollection = db("jira.issueWorklogs")

  val utcZoneId = ZoneId.of("UTC")

  implicit object InstantHandler extends BSONHandler[BSONDateTime, Instant] {
    def read(bson: BSONDateTime): Instant = Instant.ofEpochMilli(bson.value)

    def write(t: Instant) = BSONDateTime(t.toEpochMilli)
  }

  implicit object BasicIssueWriter extends BSONDocumentWriter[BasicIssue] {
    def write(issue: BasicIssue) = BSONDocument(
      "baseUrl" -> issue.baseUrl,
      "id" -> issue.id,
      "key" -> issue.key,
      "created" -> issue.created.toInstant,
      "updated" -> issue.updated.toInstant
    )
  }

  implicit object UserWriter extends BSONDocumentWriter[User] {
    def write(user: User) = BSONDocument(
      "baseUrl" -> user.baseUrl,
      "name" -> user.name,
      "displayName" -> user.displayName,
      "emailAddress" -> user.emailAddress,
      "timeZone" -> user.timeZone
    )
  }

  implicit object WorklogWriter extends BSONDocumentWriter[Worklog] {
    def write(worklog: Worklog) = BSONDocument(
      "id" -> worklog.id,
      "started" -> worklog.started.atStartOfDay(utcZoneId).toInstant,
      "timeSpentSeconds" -> worklog.timeSpentSeconds,
      "author" -> worklog.author,
      "updateAuthor" -> worklog.updateAuthor,
      "comment" -> worklog.comment,
      "issueKey" -> worklog.issueKey,
      "baseUrl" -> worklog.baseUrl
    )
  }

  implicit object IssueWorklogsWriter extends BSONDocumentWriter[IssueWorklogs] {
    def write(iw: IssueWorklogs) = BSONDocument(
      "baseUrl" -> iw.baseUrl,
      "issueKey" -> iw.issueKey,
      "worklogs" -> iw.worklogs
    )
  }

  implicit object BasicIssueReader extends BSONDocumentReader[BasicIssue] {
    def read(doc: BSONDocument) = BasicIssue(
      doc.getAs[String]("baseUrl"),
      doc.getAs[String]("id").get,
      doc.getAs[String]("key").get,
      doc.getAs[Instant]("created").get.atZone(utcZoneId),
      doc.getAs[Instant]("updated").get.atZone(utcZoneId)
    )
  }

  implicit object UserReader extends BSONDocumentReader[User] {
    def read(doc: BSONDocument) = User(
      doc.getAs[String]("baseUrl"),
      doc.getAs[String]("name").get,
      doc.getAs[String]("displayName").get,
      doc.getAs[String]("emailAddress"),
      doc.getAs[String]("timeZone")
    )
  }

  implicit object WorklogReader extends BSONDocumentReader[Worklog] {
    def read(doc: BSONDocument) = Worklog(
      doc.getAs[String]("id").get,
      doc.getAs[Instant]("started").get.atZone(utcZoneId).toLocalDate,
      doc.getAs[Int]("timeSpentSeconds").get,
      doc.getAs[User]("author").get,
      doc.getAs[User]("updateAuthor"),
      doc.getAs[String]("comment"),
      doc.getAs[String]("issueKey"),
      doc.getAs[String]("baseUrl")
    )
  }

  implicit object IssueWorklogsReader extends BSONDocumentReader[IssueWorklogs] {
    def read(doc: BSONDocument) = IssueWorklogs(
      doc.getAs[String]("baseUrl"),
      doc.getAs[String]("issueKey"),
      null, null, null,
      doc.getAs[Seq[Worklog]]("worklogs")
    )
  }

  def updateIssue(issue: BasicIssue): Unit = {
    issuesColl.update(issueSelector(issue), issue, upsert = true)
  }

  def updateIssues(issues: Seq[BasicIssue]): Unit = {
    for (issue <- issues.par) updateIssue(issue)
  }

  def updateIssueWorklogs(iw: IssueWorklogs) = {
    issueWorklogsColl.update(issueWorklogsSelector(iw), iw, upsert = true)
  }

//  def listIssues(): Future[Seq[BasicIssue]] = {
//    issuesColl.find(BSONDocument()).cursor[BasicIssue].collect[Seq]()
//  }

  def getIssueByBaseUrlAndId(baseUrl: String, id: String) = {
    // issuesColl.find(BSONDocument("id" -> BSONDocument("$eq" -> id))).one[BasicIssue]
    issuesColl.find(issueSelector(baseUrl, id)).one[BasicIssue]
  }

  def issueSelector(issue: BasicIssue): BSONDocument =
    issueSelector(issue.baseUrl.get, issue.id)

  def issueSelector(baseUrl: String, id: String): BSONDocument = {
    BSONDocument("$and" -> BSONArray(
      BSONDocument("baseUrl" -> BSONDocument("$in" -> BSONArray(baseUrl))),
      BSONDocument("id" -> BSONDocument("$in" -> BSONArray(id)))
    ))
  }

  def issueWorklogsSelector(iw: IssueWorklogs): BSONDocument =
    issueWorklogsSelector(iw.baseUrl.get, iw.issueKey.get)

  def issueWorklogsSelector(baseUrl: String, issueKey: String): BSONDocument = {
    BSONDocument("$and" -> BSONArray(
      BSONDocument("baseUrl" -> BSONDocument("$in" -> BSONArray(baseUrl))),
      BSONDocument("issueKey" -> BSONDocument("$in" -> BSONArray(issueKey)))
    ))
  }

  def listWorklogs(baseUrl: String, issueKey: String): Future[ParSeq[Worklog]] = {
    val futureRes = issueWorklogsColl
      .find(issueWorklogsSelector(baseUrl, issueKey))
      .one[IssueWorklogs]
    val worklogs = futureRes.map {
      case Some(issueWorklogs) => issueWorklogs.worklogs match {
        case Some(seq) => seq.par
        case None => ParSeq.empty
      }
      case None => ParSeq.empty
    }
    worklogs
  }

  def latestIssueTimestamp(baseUrl: String): Future[Option[ZonedDateTime]] = {

    val latestTsAlias = "latestTimestamp"

    //    import issuesColl.BatchCommands.AggregationFramework._
    //    // issuesColl.aggregate(Group(BSONString("$state"))(
    //    val futureRes = issuesColl.aggregate(Group(BSONNull)(
    //      fieldAlias -> Max("updated")
    //    ))

    val maxCommand = Aggregate(issuesColl.name, Seq(
     Match(BSONDocument("baseUrl" -> baseUrl)),
     Group(BSONNull)(latestTsAlias -> Max("updated"))
    ))
    val futureRes = issuesColl.db.command(maxCommand)

    futureRes
      .map(_.map(doc => doc.getAs[Instant](latestTsAlias).map(_.atZone(utcZoneId))))
      .map(s => if(s.isEmpty) None else s.head)
  }
}
