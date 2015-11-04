package com.github.macpersia.planty_jira_view.model

import java.time.{Instant, ZoneId, ZonedDateTime}

import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson._

import scala.collection.immutable.Seq
import scala.collection.parallel.immutable.ParSeq
import scala.concurrent.{ExecutionContext, Future}

class CacheManager(implicit execContext: ExecutionContext) {

  private val driver = new MongoDriver
  private val connection = driver.connection(List("localhost"))
  private val db = connection("diy")(execContext)
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
      "timeSpentSeconds" ->  worklog.timeSpentSeconds,
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
    issuesColl.find(BSONDocument()).cursor[BasicIssue]().collect[Seq]()
  }

  def getIssueById(id: String) = {
    issuesColl.find(BSONDocument("id" -> BSONDocument("$eq" -> id))).one[BasicIssue]
  }

  def listWorklogs(issueKey: String): Future[ParSeq[Worklog]] = {
    val futureRes = issueWorklogsColl.find(BSONDocument("issueKey" -> BSONDocument("$eq" -> issueKey))).one[IssueWorklogs]
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
    import issuesColl.BatchCommands.AggregationFramework._
    // issuesColl.aggregate(Group(BSONString("$state"))(
    val fieldAlias = "latestTimestamp"
    val futureRes = issuesColl.aggregate(Group(BSONNull)(
      fieldAlias -> Max("updated")
    ))
    futureRes.map(_.documents).map(_ match {
      case x :: xs => x.getAs[Instant](fieldAlias).map(_.atZone(utcZoneId))
      case Nil => None
    })
  }
}
