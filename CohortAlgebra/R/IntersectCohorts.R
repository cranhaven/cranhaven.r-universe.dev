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


#' Intersect cohort(s)
#'
#' @description
#' Find the common cohort period for persons present in all the cohorts. Note: if
#' subject is not found in any of the cohorts, then they will not
#' be in the final cohort.
#'
#' `r lifecycle::badge("stable")`
#'
#' @template ConnectionDetails
#'
#' @template Connection
#'
#' @template sourceCohortTable
#'
#' @template sourceCohortDatabaseSchema
#'
#' @template targetCohortTable
#'
#' @template targetCohortDatabaseSchema
#'
#' @template CohortIds
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
intersectCohorts <- function(connectionDetails = NULL,
                             connection = NULL,
                             sourceCohortDatabaseSchema = NULL,
                             sourceCohortTable,
                             targetCohortDatabaseSchema = NULL,
                             targetCohortTable,
                             cohortIds,
                             newCohortId,
                             purgeConflicts = FALSE,
                             tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(
    x = cohortIds,
    min.len = 1,
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

  numberOfCohorts <- length(cohortIds |> unique())

  sql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = "IntersectCohorts.sql",
    packageName = utils::packageName(),
    dbms = connection@dbms,
    number_of_cohorts = numberOfCohorts,
    cohort_ids = cohortIds,
    new_cohort_id = newCohortId,
    tempEmulationSchema = tempEmulationSchema,
    source_cohort_database_schema = sourceCohortDatabaseSchema,
    source_cohort_table = sourceCohortTable,
    target_cohort_table = tempTable1,
    target_cohort_database_schema = NULL
  )

  message(" Intersecting cohorts.")
  DatabaseConnector::executeSql(
    connection = connection,
    sql = sql,
    profile = FALSE,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  message(" Generating eras and saving.")
  eraFyCohorts(
    connection = connection,
    sourceCohortDatabaseSchema = NULL,
    sourceCohortTable = tempTable1,
    targetCohortDatabaseSchema = targetCohortDatabaseSchema,
    targetCohortTable = targetCohortTable,
    oldCohortIds = newCohortId,
    newCohortId = newCohortId,
    purgeConflicts = purgeConflicts
  )
}
