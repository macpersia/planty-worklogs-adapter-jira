package com.github.macpersia.planty.views.jira

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}
import java.util.TimeZone

import com.github.macpersia.planty.worklogs.model.WorklogFilter
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.collection.immutable.Seq

package object model {

  case class JiraWorklogFilter(
                                override val author: Option[String],
                                override val fromDate: LocalDate,
                                override val toDate: LocalDate,
                                override val timeZone: TimeZone,
                                jiraQuery: String)
    extends WorklogFilter(
      author, fromDate, toDate, timeZone)

  case class SearchResult(startAt: Int,
                          maxResults: Int,
                          total: Int,
                          issues: Seq[BasicIssue])

  case class BasicIssue(baseUrl: Option[String],
                        id: String,
                        key: String,
                        created: ZonedDateTime,
                        updated: ZonedDateTime)

  /*
  case class FullIssue( id: String, key: String, fields: IssueFields )

  case class IssueFields( summary: String,
                          issueType: Option[IssueType],
                          status: Status,
                          //  labels: ???,
                          assignee: Option[User],
                          reporter: User,
                          updated: ZonedDateTime,
                          created: ZonedDateTime,
                          project: Project,
                          priority: Option[Priority] )

  case class IssueType( id: String, name: String, description: Option[String] )

  case class Status( id: String, name: String, description: Option[String] )

  case class Project( id: String, key: String, name: Option[String] )

  case class Priority( id: String, name: Option[String] )
  */

  case class User(
                   baseUrl: Option[String],
                   name: String,
                   displayName: String,
                   emailAddress: Option[String],
                   timeZone: Option[String])

  case class IssueWorklogs(baseUrl: Option[String], issueKey: Option[String],
                           startAt: Option[Int], maxResults: Option[Int], total: Option[Int],
                           worklogs: Option[Seq[Worklog]])

  case class Worklog(id: String,
                     started: LocalDate,
                     timeSpentSeconds: Int,
                     author: User,
                     updateAuthor: Option[User],
                     comment: Option[String],
                     issueKey: Option[String],
                     baseUrl: Option[String])

  val jiraDTFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSZZ")

  implicit val readsLocalDate = Reads[LocalDate](js =>
    js.validate[String].map[LocalDate]
      (dtString => LocalDate.parse(dtString, jiraDTFormatter))
  )
  implicit val writesLocalDate = Writes[LocalDate](date =>
    JsString(jiraDTFormatter.format(date))
  )
  implicit val readsZonedDateTime = Reads[ZonedDateTime](js =>
    js.validate[String].map[ZonedDateTime]
      (dtString => ZonedDateTime.parse(dtString, jiraDTFormatter))
  )

  implicit val writesZonedDateTime = Writes[ZonedDateTime](timestamp =>
    JsString(jiraDTFormatter.format(timestamp))
  )

  //  implicit val basicIssueReads = Json.reads[BasicIssue]
  implicit val basicIssueReads: Reads[BasicIssue] = (
    (JsPath \ "baseUrl").readNullable[String] and
      (JsPath \ "id").read[String] and
      (JsPath \ "key").read[String] and
      (JsPath \ "fields" \ "created").read(readsZonedDateTime) and
      (JsPath \ "fields" \ "updated").read(readsZonedDateTime)
    ) (BasicIssue.apply _)

  //  implicit val basicIssueWrites = Json.writes[BasicIssue]
  implicit val basicIssueWrites: Writes[BasicIssue] = (
    (JsPath \ "baseUrl").writeNullable[String] and
      (JsPath \ "id").write[String] and
      (JsPath \ "key").write[String] and
      (JsPath \ "fields" \ "created").write(writesZonedDateTime) and
      (JsPath \ "fields" \ "updated").write(writesZonedDateTime)
    ) (unlift(BasicIssue.unapply))

  implicit val searchResultReads = Json.reads[SearchResult]
  implicit val searchResultWrites = Json.writes[SearchResult]

  //  implicit val issueTypeReads = Json.reads[IssueType]
  //  implicit val issueTypeWrites = Json.writes[IssueType]

  //  implicit val statusReads = Json.reads[Status]
  //  implicit val statusWrites = Json.writes[Status]

  implicit val userReads = Json.reads[User]
  implicit val userWrites = Json.writes[User]

  //  implicit val priorityReads = Json.reads[Priority]
  //  implicit val priorityWrites = Json.writes[Priority]

  //  implicit val issueFieldsReads = Json.reads[IssueFields]
  //  implicit val issueFieldsWrites = Json.writes[IssueFields]

  //  implicit val fullIssueReads = Json.reads[FullIssue]
  //  implicit val fullIssueWrites = Json.writes[FullIssue]

  //  implicit val worklogReads: Reads[Worklog] = (
  //    (JsPath \ "id").read[String] and
  //    (JsPath \ "started").read(readsLocalDate) and
  //    (JsPath \ "timeSpentSeconds").read[Int] and
  //    (JsPath \ "author").read[User] and
  //    (JsPath \ "updateAuthor").readNullable[User] and
  //    (JsPath \ "comment").readNullable[String] and
  //    (JsPath \ "issueKey").readNullable[String] and
  //    (JsPath \ "baseUrl").readNullable[String]
  //  )(Worklog.apply _)
  implicit val worklogReads = Json.reads[Worklog]

  //  implicit val worklogWrites: Writes[Worklog] = (
  //    (JsPath \ "id").write[String] and
  //    (JsPath \ "started").write(writesLocalDate) and
  //    (JsPath \ "timeSpentSeconds").write[Int] and
  //    (JsPath \ "author").write[User] and
  //    (JsPath \ "updateAuthor").writeNullable[User] and
  //    (JsPath \ "comment").writeNullable[String] and
  //    (JsPath \ "issueKey").writeNullable[String] and
  //    (JsPath \ "baseUrl").writeNullable[String]
  //  )(unlift(Worklog.unapply))
  implicit val worklogWrites = Json.writes[Worklog]

  implicit val issueWorklogsReads = Json.reads[IssueWorklogs]
  implicit val issueWorklogsWrites = Json.writes[IssueWorklogs]
}
