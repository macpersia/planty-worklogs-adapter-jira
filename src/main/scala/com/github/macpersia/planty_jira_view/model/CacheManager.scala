package com.github.macpersia.planty_jira_view.model

import java.time.{ZonedDateTime, Instant, ZoneId}

import reactivemongo.api.MongoDriver
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson._

import scala.concurrent.{ExecutionContext, Future}

class CacheManager(implicit execContext: ExecutionContext) {

  private val driver = new MongoDriver
  private val connection = driver.connection(List("localhost"))
  private val db = connection("diy")(execContext)
  private val issuesColl: BSONCollection = db("jira.issues")

  implicit object InstantHandler extends BSONHandler[BSONDateTime, Instant] {
    def read(bson: BSONDateTime): Instant = Instant.ofEpochMilli(bson.value)
    def write(t: Instant): BSONDateTime = BSONDateTime(t.toEpochMilli)
  }

  implicit object BasicIssueWriter extends BSONDocumentWriter[BasicIssue] {
    def write(issue: BasicIssue): BSONDocument = BSONDocument(
      "id" -> issue.id,
      "key" -> issue.key,
      "created" -> issue.created.toInstant,
      "updated" -> issue.updated.toInstant
    )
  }

  implicit object BasicIssueReader extends BSONDocumentReader[BasicIssue] {
    val zoneId = ZoneId.systemDefault()
    def read(doc: BSONDocument): BasicIssue = BasicIssue(
      doc.getAs[String]("id").get,
      doc.getAs[String]("key").get,
      doc.getAs[Instant]("created").get.atZone(zoneId),
      doc.getAs[Instant]("updated").get.atZone(zoneId)
    )
  }

  def updateIssues(issues: Seq[BasicIssue]): Unit = {
      for (issue <- issues.par)
        issuesColl.update(BSONDocument("id" -> issue.id), issue, upsert = true)
  }

  def listIssues(): Future[Seq[BasicIssue]] = {
    issuesColl.find(BSONDocument()).cursor[BasicIssue]().collect[Seq]()
  }

  def latestIssueTimestamp(): Future[ZonedDateTime] = {
    issuesColl.find(BSONDocument("$max" -> "updated")).one(readsZonedDateTime, execContext).map(x => x.get)
  }
}
