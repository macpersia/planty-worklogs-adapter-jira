package com.github.macpersia.planty_jira_view.model

import java.time.{Instant, ZoneId, ZonedDateTime}

import play.api.libs.json.Json
import play.modules.reactivemongo.json.BSONFormats
import reactivemongo.api.{MongoConnection, MongoDriver}
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson._
import reactivemongo.core.commands._
import reactivemongo.core.nodeset.{Connection, Authenticate}

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
      "id" -> issue.id,
      "key" -> issue.key,
      "created" -> issue.created.toInstant,
      "updated" -> issue.updated.toInstant
    )
  }

  implicit object UserWriter extends BSONDocumentWriter[User] {
    def write(user: User) = BSONDocument(
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
      "issueKey" -> worklog.issueKey
    )
  }

  implicit object IssueWorklogsWriter extends BSONDocumentWriter[IssueWorklogs] {
    def write(issueWorklogs: IssueWorklogs) = BSONDocument(
      "issueKey" -> issueWorklogs.issueKey,
      "worklogs" -> issueWorklogs.worklogs
    )
  }

  implicit object BasicIssueReader extends BSONDocumentReader[BasicIssue] {
    def read(doc: BSONDocument) = BasicIssue(
      doc.getAs[String]("id").get,
      doc.getAs[String]("key").get,
      doc.getAs[Instant]("created").get.atZone(utcZoneId),
      doc.getAs[Instant]("updated").get.atZone(utcZoneId)
    )
  }

  implicit object UserReader extends BSONDocumentReader[User] {
    def read(doc: BSONDocument) = User(
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
      doc.getAs[String]("issueKey")
    )
  }

  implicit object IssueWorklogsReader extends BSONDocumentReader[IssueWorklogs] {
    def read(doc: BSONDocument) = IssueWorklogs(
      doc.getAs[String]("issueKey"),
      null, null, null,
      doc.getAs[Seq[Worklog]]("worklogs")
    )
  }

  def updateIssue(issue: BasicIssue): Unit = {
    issuesColl.update(BSONDocument("id" -> issue.id), issue, upsert = true)
  }

  def updateIssues(issues: Seq[BasicIssue]): Unit = {
    for (issue <- issues.par) updateIssue(issue)
  }

  def updateIssueWorklogs(issueWorklogs: IssueWorklogs) = {
    issueWorklogsColl.update(BSONDocument("id" -> issueWorklogs.issueKey), issueWorklogs, upsert = true)
  }

  def listIssues(): Future[Seq[BasicIssue]] = {
    issuesColl.find(BSONDocument()).cursor[BasicIssue].collect[Seq]()
  }

  def getIssueById(id: String) = {
    // issuesColl.find(BSONDocument("id" -> BSONDocument("$eq" -> id))).one[BasicIssue]
    issuesColl.find(BSONDocument("id" -> BSONDocument("$in" -> BSONArray(id)))).one[BasicIssue]
  }

  def listWorklogs(issueKey: String): Future[ParSeq[Worklog]] = {
    val futureRes = issueWorklogsColl
      // .find(BSONDocument("issueKey" -> BSONDocument("$eq" -> issueKey)))
      .find(BSONDocument("issueKey" -> BSONDocument("$in" -> BSONArray(issueKey))))
      .one[IssueWorklogs]
    val worklogs = futureRes.map(_ match {
      case Some(issueWorklogs) => issueWorklogs.worklogs match {
        case Some(seq) => seq.par
        case None => ParSeq.empty
      }
      case None => ParSeq.empty
    })
    worklogs
  }

  def latestIssueTimestamp(): Future[Option[ZonedDateTime]] = {

    val fieldAlias = "latestTimestamp"

    //    import issuesColl.BatchCommands.AggregationFramework._
    //    // issuesColl.aggregate(Group(BSONString("$state"))(
    //    val futureRes = issuesColl.aggregate(Group(BSONNull)(
    //      fieldAlias -> Max("updated")
    //    ))
    //    val futureDocs = futureRes.map(_.documents)

    import play.modules.reactivemongo.json.BSONFormats._
    val maxCommand = Aggregate(issuesColl.name, Seq(
     Group(BSONNull)(fieldAlias -> Max("updated"))
    ))
    val futureRes = issuesColl.db.command(maxCommand)
    val futureDocs = futureRes.map(_.toSeq)


    futureDocs.map(_ match {
      case x :: xs => x.getAs[Instant](fieldAlias).map(_.atZone(utcZoneId))
      case _ => None
    })
  }
}
