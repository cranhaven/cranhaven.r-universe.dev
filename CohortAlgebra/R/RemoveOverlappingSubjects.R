# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of CohortAlgebra
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Remove subjects in cohort that overlap with another cohort
#'
#' @description
#' Remove subjects in cohort that overlap with another cohort. Given a Cohort A, check if the records of
#' subjects in cohort A overlaps with records for the same subject in cohort B. If there is overlap
#' then remove all records of that subject from Cohort A.
#' Overlap is defined as b.cohort_end_date >= a.cohort_start_date AND b.cohort_start_date <= a.cohort_end_date.
#' The overlap logic maybe offset by using a startDayOffSet (applied on cohort A's cohort_start_date)
#' and endDayOffSet (applied on Cohort A's cohort_end_date). If while applying offset, the window becomes
#' such that (a.cohort_start_date + startDayOffSet) > (a.cohort_end_date + endDayOffset) that record is ignored
#' and thus deleted.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @template ConnectionDetails
#'
#' @template Connection
#'
#' @template CohortTable
#'
#' @param cohortId The cohort id of the cohort whose subjects will be removed.
#'
#' @param cohortsWithSubjectsToRemove An array of one or more cohorts with subjects to remove from given cohorts.
#'
#' @param offsetCohortStartDate (Default = 0) If you want to offset cohort start date, please provide a integer number.
#'
#' @param offsetCohortEndDate (Default = 0) If you want to offset cohort start date, please provide a integer number.
#'
#' @param restrictSecondCohortStartBeforeFirstCohortStart  (Default = FALSE) If TRUE, then the secondCohort's cohort_start_date
#'                                                          should be < firstCohort's cohort_start_date.
#'
#' @param restrictSecondCohortStartAfterFirstCohortStart  (Default = FALSE) If TRUE, then the secondCohort's cohort_start_date
#'                                                          should be > firstCohort's cohort_start_date.
#'
#' @template NewCohortId
#'
#' @template CohortDatabaseSchema
#'
#' @template PurgeConflicts
#'
#' @template TempEmulationSchema
#'
#' @return   Nothing is returned
#'
#' @export
removeOverlappingSubjects <- function(connectionDetails = NULL,
                                      connection = NULL,
                                      cohortDatabaseSchema,
                                      cohortId,
                                      newCohortId,
                                      cohortsWithSubjectsToRemove,
                                      offsetCohortStartDate = -99999,
                                      offsetCohortEndDate = 99999,
                                      restrictSecondCohortStartBeforeFirstCohortStart = FALSE,
                                      restrictSecondCohortStartAfterFirstCohortStart = FALSE,
                                      cohortTable = "cohort",
                                      purgeConflicts = FALSE,
                                      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDouble(
    x = offsetCohortStartDate,
    len = 1,
    null.ok = FALSE,
    add = errorMessages
  )
  checkmate::assertDouble(
    x = offsetCohortEndDate,
    len = 1,
    null.ok = FALSE,
    add = errorMessages
  )
  checkmate::assertDouble(
    x = cohortId,
    len = 1,
    null.ok = FALSE,
    add = errorMessages
  )
  checkmate::assertDouble(
    x = newCohortId,
    len = 1,
    null.ok = FALSE,
    add = errorMessages
  )
  checkmate::assertDouble(
    x = cohortsWithSubjectsToRemove,
    min.len = 1,
    null.ok = FALSE,
    add = errorMessages
  )
  checkmate::assertCharacter(
    x = cohortDatabaseSchema,
    min.chars = 1,
    len = 1,
    null.ok = TRUE,
    add = errorMessages
  )
  checkmate::assertCharacter(
    x = cohortTable,
    min.chars = 1,
    len = 1,
    null.ok = FALSE,
    add = errorMessages
  )
  checkmate::assertLogical(
    x = restrictSecondCohortStartBeforeFirstCohortStart,
    any.missing = FALSE,
    min.len = 1,
    add = errorMessages
  )
  checkmate::assertLogical(
    x = restrictSecondCohortStartAfterFirstCohortStart,
    any.missing = FALSE,
    min.len = 1,
    add = errorMessages
  )
  checkmate::reportAssertions(collection = errorMessages)

  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  if (!purgeConflicts) {
    cohortIdsInCohortTable <-
      getCohortIdsInCohortTable(
        connection = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        tempEmulationSchema = tempEmulationSchema
      )
    conflicitingCohortIdsInTargetCohortTable <-
      intersect(
        x = newCohortId,
        y = cohortIdsInCohortTable |> unique()
      )

    if (length(conflicitingCohortIdsInTargetCohortTable) > 0) {
      stop(
        paste0(
          "The following cohortIds already exist in the target cohort table, causing conflicts :",
          paste0(newCohortId,
            collapse = ","
          )
        )
      )
    }
  }

  tempTableName <- generateRandomString()
  tempTable1 <- paste0("#", tempTableName, "1") #
  tempTable2 <- paste0("#", tempTableName, "2")

  # subject to remove
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = " DROP TABLE IF EXISTS @temp_table_1;
            SELECT DISTINCT c1.subject_id
            INTO @temp_table_1
          	FROM {@cohort_database_schema != ''} ? {@cohort_database_schema.@cohort_table} : {@cohort_table} c1
          	INNER JOIN {@cohort_database_schema != ''} ? {@cohort_database_schema.@cohort_table} : {@cohort_table} c2 ON c1.subject_id = c2.subject_id
          		AND DATEADD(DAY, @first_offset, c1.cohort_start_date) <= c2.cohort_end_date
          		AND DATEADD(DAY, @second_offset, c1.cohort_end_date) >= c2.cohort_start_date
          	WHERE c1.cohort_definition_id IN (@first_cohort_id)
          		AND c2.cohort_definition_id IN (@remove_cohort_ids)
        		{@second_cohort_start_before_first_cohort_start} ? {
        		    AND c2.cohort_start_date < c1.cohort_start_date}
        		{@second_cohort_start_after_first_cohort_start} ? {
        		    AND c2.cohort_start_date > c1.cohort_start_date};",
    profile = FALSE,
    progressBar = FALSE,
    reportOverallTime = FALSE,
    cohort_database_schema = cohortDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    cohort_table = cohortTable,
    first_cohort_id = cohortId,
    remove_cohort_ids = cohortsWithSubjectsToRemove,
    temp_table_1 = tempTable1,
    first_offset = offsetCohortStartDate,
    second_offset = offsetCohortEndDate,
    second_cohort_start_before_first_cohort_start = restrictSecondCohortStartBeforeFirstCohortStart,
    second_cohort_start_after_first_cohort_start = restrictSecondCohortStartAfterFirstCohortStart
  )

  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = " DROP TABLE IF EXISTS @temp_table_2;
            SELECT CAST(@new_cohort_id AS BIGINT) cohort_definition_id,
               CAST(c.subject_id AS BIGINT) subject_id,
               CAST(c.cohort_start_date AS DATE) cohort_start_date,
               CAST(c.cohort_end_date AS DATE) cohort_end_date
            INTO @temp_table_2
            FROM (
                    SELECT *
                    FROM {@cohort_database_schema != ''} ? {@cohort_database_schema.@cohort_table} : {@cohort_table}
                    WHERE cohort_definition_id = @given_cohort_id
                  ) c
            LEFT JOIN
                @temp_table_1 r
            ON c.subject_id = r.subject_id
            WHERE r.subject_id IS NULL;",
    profile = FALSE,
    progressBar = FALSE,
    reportOverallTime = FALSE,
    cohort_database_schema = cohortDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    cohort_table = cohortTable,
    given_cohort_id = cohortId,
    new_cohort_id = newCohortId,
    temp_table_1 = tempTable1,
    temp_table_2 = tempTable2
  )

  if (purgeConflicts) {
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = " DELETE FROM {@cohort_database_schema != ''} ? {@cohort_database_schema.@cohort_table} : {@cohort_table}
              WHERE cohort_definition_id IN (@new_cohort_id);",
      profile = FALSE,
      progressBar = FALSE,
      reportOverallTime = FALSE,
      cohort_database_schema = cohortDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema,
      cohort_table = cohortTable,
      new_cohort_id = newCohortId
    )
  }
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = " INSERT INTO {@cohort_database_schema != ''} ? {@cohort_database_schema.@cohort_table} : {@cohort_table}
            SELECT *
            FROM @temp_table_2;
            UPDATE STATISTICS  {@cohort_database_schema != ''} ? {@cohort_database_schema.@cohort_table} : {@cohort_table};

            DROP TABLE IF EXISTS @temp_table_1;
            DROP TABLE IF EXISTS @temp_table_2;",
    profile = FALSE,
    progressBar = FALSE,
    reportOverallTime = FALSE,
    cohort_database_schema = cohortDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    cohort_table = cohortTable,
    temp_table_1 = tempTable1,
    temp_table_2 = tempTable2
  )
}
