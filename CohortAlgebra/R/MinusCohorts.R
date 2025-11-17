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

#' Minus cohort(s)
#'
#' @description
#' Given two cohorts, substract (minus) the dates from the first cohort, the
#' dates the subject also had on the second cohort.
#'
#' `r lifecycle::badge("stable")`
#'
#' @template ConnectionDetails
#'
#' @template Connection
#'
#' @template SourceCohortDatabaseSchema
#'
#' @template SourceCohortTable
#'
#' @template TargetCohortDatabaseSchema
#'
#' @template TargetCohortTable
#'
#' @param firstCohortId The cohort id of the cohort from which to subtract.
#'
#' @param secondCohortId The cohort id of the cohort that is used to subtract.
#'
#' @template NewCohortId
#'
#' @template PurgeConflicts
#'
#' @template TempEmulationSchema
#'
#' @return   Nothing is returned
#'
#' @export
minusCohorts <- function(connectionDetails = NULL,
                         connection = NULL,
                         sourceCohortDatabaseSchema = NULL,
                         sourceCohortTable = "cohort",
                         targetCohortDatabaseSchema = sourceCohortDatabaseSchema,
                         targetCohortTable = sourceCohortTable,
                         firstCohortId,
                         secondCohortId,
                         newCohortId,
                         purgeConflicts = FALSE,
                         tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(
    x = firstCohortId,
    len = 1,
    null.ok = FALSE,
    add = errorMessages
  )
  checkmate::assertIntegerish(
    x = secondCohortId,
    len = 1,
    null.ok = FALSE,
    add = errorMessages
  )
  checkmate::assertIntegerish(
    x = newCohortId,
    len = 1,
    null.ok = FALSE,
    add = errorMessages
  )
  checkmate::assertCharacter(
    x = sourceCohortDatabaseSchema,
    min.chars = 1,
    len = 1,
    null.ok = TRUE,
    add = errorMessages
  )
  checkmate::assertCharacter(
    x = sourceCohortTable,
    min.chars = 1,
    len = 1,
    null.ok = FALSE,
    add = errorMessages
  )
  checkmate::assertCharacter(
    x = targetCohortDatabaseSchema,
    min.chars = 1,
    len = 1,
    null.ok = TRUE,
    add = errorMessages
  )
  checkmate::assertCharacter(
    x = targetCohortTable,
    min.chars = 1,
    len = 1,
    null.ok = FALSE,
    add = errorMessages
  )
  checkmate::assertLogical(
    x = purgeConflicts,
    any.missing = FALSE,
    min.len = 1,
    add = errorMessages
  )
  checkmate::reportAssertions(collection = errorMessages)

  if (firstCohortId == secondCohortId) {
    warning(
      "During minus operation, both first and second cohorts have the same cohort id. The result will be a NULL cohort."
    )
  }

  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  if (!purgeConflicts) {
    cohortIdsInCohortTable <-
      getCohortIdsInCohortTable(
        connection = connection,
        cohortDatabaseSchema = targetCohortDatabaseSchema,
        cohortTable = targetCohortTable,
        tempEmulationSchema = tempEmulationSchema
      )

    conflicitingCohortIdsInTargetCohortTable <-
      intersect(
        x = newCohortId,
        y = cohortIdsInCohortTable |> unique()
      )
    if (length(conflicitingCohortIdsInTargetCohortTable) > 0) {
      stop("Target cohort id already in use in target cohort table")
    }
  }

  tempTableName <- generateRandomString()
  tempTable1 <- paste0("#", tempTableName, "1")
  tempTable2 <- paste0("#", tempTableName, "2")

  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = "
      DROP TABLE IF EXISTS @target_cohort_table;
      CREATE TABLE @target_cohort_table (
                    	cohort_definition_id BIGINT,
                    	subject_id BIGINT,
                    	cohort_start_date DATE,
                    	cohort_end_date DATE
  );",
    target_cohort_table = tempTable1
  )

  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = "
      DROP TABLE IF EXISTS @target_cohort_table;
      CREATE TABLE @target_cohort_table (
                    	cohort_definition_id BIGINT,
                    	subject_id BIGINT,
                    	cohort_start_date DATE,
                    	cohort_end_date DATE
  );",
    target_cohort_table = tempTable2
  )

  message("Performing minus operation.")
  intersectCohorts(
    connection = connection,
    sourceCohortDatabaseSchema = sourceCohortDatabaseSchema,
    sourceCohortTable = sourceCohortTable,
    cohortIds = c(firstCohortId, secondCohortId),
    targetCohortDatabaseSchema = NULL,
    targetCohortTable = tempTable1,
    newCohortId = -999,
    purgeConflicts = FALSE,
    tempEmulationSchema = tempEmulationSchema
  )

  sql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = "MinusCohorts.sql",
    packageName = utils::packageName(),
    dbms = connection@dbms,
    first_cohort_id = firstCohortId,
    temp_table_1 = tempTable1,
    temp_table_2 = tempTable2,
    tempEmulationSchema = tempEmulationSchema,
    source_cohort_database_schema = sourceCohortDatabaseSchema,
    source_cohort_table = sourceCohortTable,
    target_cohort_database_schema = targetCohortDatabaseSchema,
    target_cohort_table = targetCohortTable,
    new_cohort_id = newCohortId
  )
  DatabaseConnector::executeSql(
    connection = connection,
    sql = sql,
    profile = FALSE,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
}
