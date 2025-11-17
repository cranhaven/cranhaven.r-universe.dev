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

#' Union cohort(s)
#'
#' @description
#' Given a specified array of cohortIds in a cohort table, perform
#' cohort union operator to create new cohorts.
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
#' @template OldToNewCohortId
#'
#' @template PurgeConflicts
#'
#' @template IsTempTable
#'
#' @template TempEmulationSchema
#'
#' @return   Nothing is returned
#'
#' @export
unionCohorts <- function(connectionDetails = NULL,
                         connection = NULL,
                         sourceCohortDatabaseSchema = NULL,
                         sourceCohortTable,
                         targetCohortDatabaseSchema = NULL,
                         targetCohortTable,
                         oldToNewCohortId,
                         isTempTable = FALSE,
                         tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                         purgeConflicts = FALSE) {
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

  newCohortIds <- oldToNewCohortId$newCohortId |> unique()

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
          x = newCohortIds,
          y = cohortIdsInCohortTable |> unique()
        )
      if (length(conflicitingCohortIdsInTargetCohortTable) > 0) {
        stop("Target cohort id already in use in target cohort table")
      }
    }
  }

  tempTables <- c()

  for (i in (1:length(newCohortIds))) {
    tempTableName <- paste0("#", generateRandomString())
    tempTables <- c(tempTables, tempTableName)

    eraFyCohorts(
      connection = connection,
      sourceCohortDatabaseSchema = sourceCohortDatabaseSchema,
      sourceCohortTable = sourceCohortTable,
      targetCohortDatabaseSchema = NULL,
      targetCohortTable = tempTableName,
      oldCohortIds = oldToNewCohortId |>
        dplyr::filter(.data$newCohortId == newCohortIds[[i]]) |>
        dplyr::pull("oldCohortId") |>
        unique(),
      newCohortId = newCohortIds[[i]],
      eraconstructorpad = 0,
      cdmDatabaseSchema = NULL,
      isTempTable = TRUE,
      tempEmulationSchema = tempEmulationSchema,
      purgeConflicts = FALSE
    )
  }

  # remove cohort data in target cohort for newCohortIds.
  # if purgeConflicts is FALSE, and there was a conflict - there would be an error message
  if (!isTempTable) {
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = "
      DELETE FROM @cohort_database_schema.@cohort_table
      WHERE cohort_definition_id IN (@cohort_definition_id);",
      cohort_database_schema = targetCohortDatabaseSchema,
      cohort_table = targetCohortTable,
      cohort_definition_id = newCohortIds,
      profile = FALSE,
      progressBar = FALSE,
      reportOverallTime = FALSE
    )
  }

  appendCohortTables(
    connection = connection,
    sourceTables = dplyr::tibble(
      sourceCohortDatabaseSchema = NA,
      sourceCohortTableName = tempTables
    ),
    targetCohortDatabaseSchema = targetCohortDatabaseSchema,
    targetCohortTable = targetCohortTable,
    tempEmulationSchema = tempEmulationSchema,
    isTempTable = isTempTable
  )
}
