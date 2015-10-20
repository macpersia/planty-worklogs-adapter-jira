package com.github.macpersia.planty_jira_view

import java.time.{LocalDate, ZonedDateTime}

import scala.collection.immutable._

package object model {

  case class SearchResult( startAt: Int,
                           maxResults: Int,
                           total: Int,
                           issues: Seq[BasicIssue])

  case class BasicIssue( id: String, key: String)

  case class FullIssue( id: String, key: String, fields: IssueFields)

  case class IssueFields( summary: String,
                          issueType: Option[IssueType],
                          status: Status,
                          //  labels: ???,
                          assignee: Option[User],
                          reporter: User,
                          //  project: Project,
                          updated: ZonedDateTime,
                          created: ZonedDateTime,
                          priority: Option[Priority])

  case class IssueType( id: String, name: String, description: Option[String] )

  case class Status( id: String, name: String, description: Option[String] )

  case class User( name: String, displayName: String, emailAddress: Option[String] )

  case class Priority( id: String, name: Option[String] )


  case class IssueWorklogs( startAt: Int, maxResults: Int, total: Int, worklogs: Option[Seq[Worklog]] )

  case class Worklog( id: String,
                      started: LocalDate,
                      timeSpentSeconds: Int,
                      author: User,
                      updateAuthor: Option[User],
                      comment: Option[String] )
}
