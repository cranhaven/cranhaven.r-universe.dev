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

#' Copy cohorts from one table to another
#'
#' @description
#' Copy cohorts from one table to another table. If the new cohort table
#' has any cohort id that matches the cohort id being copied, an error will
#' be displayed.
#'
#' `r lifecycle::badge("stable")`
#'
#' @template ConnectionDetails
#'
#' @template Connection
#'
#' @template OldToNewCohortId
#'
#' @template PurgeConflicts
#'
#' @template TempEmulationSchema
#'
#' @template IsTempTable
#'
#' @param sourceCohortDatabaseSchema The database schema of the source cohort table.
#'
#' @param targetCohortDatabaseSchema The database schema of the source cohort table.
#'
#' @param sourceCohortTable         The name of the source cohort table.
#'
#' @param targetCohortTable         The name of the target cohort table.
#'
#' @returns   Nothing is returned
#'
#' @export
#'
#'
copyCohorts <- function(connectionDetails = NULL,
                        connection = NULL,
                        oldToNewCohortId,
                        sourceCohortDatabaseSchema = NULL,
                        targetCohortDatabaseSchema = sourceCohortDatabaseSchema,
                        sourceCohortTable,
                        targetCohortTable,
                        isTempTable = FALSE,
                        purgeConflicts = FALSE,
                        tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  if (isTempTable) {
    if (!all(
      is.null(targetCohortDatabaseSchema),
      tableNameIsCompatibleWithTempTableName(tableName = targetCohortTable),
      !is.null(connection)
    )) {
      stop("Cannot output temp table - check input specifications")
    }
  }

  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(
    x = oldToNewCohortId,
    min.rows = 1,
    add = errorMessages
  )
  checkmate::assertNames(
    x = colnames(oldToNewCohortId),
    must.include = c("oldCohortId", "newCohortId"),
    add = errorMessages
  )
  checkmate::assertIntegerish(
    x = oldToNewCohortId$oldCohortId,
    min.len = 1,
    null.ok = FALSE,
    add = errorMessages
  )
  checkmate::assertIntegerish(
    x = oldToNewCohortId$newCohortId,
    min.len = 1,
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
    x = targetCohortDatabaseSchema,
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

  if (all(
    (sourceCohortDatabaseSchema == targetCohortDatabaseSchema),
    (sourceCohortTable == targetCohortTable)
  )) {
    if (length(intersect(
      oldToNewCohortId$oldCohortId,
      oldToNewCohortId$newCohortId
    )) > 0) {
      stop(" Cannot copy same cohort to same table.")
    }
  }

  if (!isTempTable) {
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
          x = oldToNewCohortId$newCohortId |> unique() |> sort(),
          y = cohortIdsInCohortTable |> unique()
        )

      if (length(conflicitingCohortIdsInTargetCohortTable) > 0) {
        stop(
          paste0(
            "The following cohortIds already exist in the target cohort table, causing conflicts :",
            paste0(conflicitingCohortIdsInTargetCohortTable, collapse = ",")
          )
        )
      }
    }
  }

  DatabaseConnector::insertTable(
    connection = connection,
    tableName = "#old_to_new_cohort_id",
    createTable = TRUE,
    dropTableIfExists = TRUE,
    tempTable = TRUE,
    tempEmulationSchema = tempEmulationSchema,
    progressBar = FALSE,
    bulkLoad = (Sys.getenv("bulkLoad") == TRUE),
    camelCaseToSnakeCase = TRUE,
    data = oldToNewCohortId
  )

  sql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = "CopyCohorts.sql",
    packageName = utils::packageName(),
    dbms = connection@dbms,
    source_cohort_database_schema = sourceCohortDatabaseSchema,
    target_cohort_database_schema = targetCohortDatabaseSchema,
    source_cohort_table = sourceCohortTable,
    target_cohort_table = targetCohortTable,
    is_temp_table = isTempTable,
    tempEmulationSchema = tempEmulationSchema
  )

  DatabaseConnector::executeSql(
    connection = connection,
    sql = sql,
    profile = FALSE,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
}
