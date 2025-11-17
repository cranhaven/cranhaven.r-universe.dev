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

#' Reindex cohort(s) by relative days
#'
#' @description
#' reindexCohort changes the cohort_start_date and/or cohort_end_date of
#' one or more source cohorts based on a set of reindexing rules. The output
#' is a one or more valid target cohorts.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @template ConnectionDetails
#'
#' @template Connection
#'
#' @template sourceCohortTable
#'
#' @template sourceCohortDatabaseSchema
#'
#' @param    sourceCohortIds   An array of one or more cohortIds in the source cohort table.
#'
#' @template targetCohortTable
#'
#' @template targetCohortDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @template IsTempTable
#'
#' @template CdmDatabaseSchema
#'
#' @template PurgeConflicts
#'
#' @param bulkLoad        See 'insertTable' function in 'DatabaseConnector'.
#'
#' @param offsetStartAnchor Determines the anchor point for the start of the reindexing. It can be either
#'                          cohort_start_date or cohort_end_date of sourceCohort.
#'
#' @param offsetEndAnchor Determines the anchor point for the end of the reindexing. It can be either
#'                        cohort_start_date or cohort_end_date of targetCohort.
#'
#' @param reindexRules    A data frame specifying the reindexing rules. It should contain the following columns: 'offsetId'
#'                        a unique key for identifying the newly generated cohorts. Each offsetId corresponds to a
#'                        specific reindex rule and will be used to create new cohort id in targetCohort. 'offsetStartValue'
#'                        is an integer value indicating the number of days to 'offsetStartAnchor'. A positive
#'                        values will extend, while negative values will shorten the start date from
#'                        the 'offsetStartAnchor'. offsetEndValue' An integer value indicating the number of
#'                        days to offset the end date. Positive values will extend, while negative values will
#'                        shorten the end date from the 'offsetEndAnchor'.
#'
#' @returns   If output is temp table, then the name of the temp table is returned.
#'
reindexCohortsByDays <- function(connectionDetails = NULL,
                                 connection = NULL,
                                 sourceCohortDatabaseSchema = NULL,
                                 sourceCohortTable = "cohort",
                                 sourceCohortIds,
                                 targetCohortDatabaseSchema = NULL,
                                 targetCohortTable,
                                 offsetStartAnchor = "cohort_start_date",
                                 offsetEndAnchor = "cohort_end_date",
                                 reindexRules,
                                 cdmDatabaseSchema = NULL,
                                 purgeConflicts = FALSE,
                                 isTempTable = FALSE,
                                 bulkLoad = Sys.getenv("DATABASE_CONNECTOR_BULK_UPLOAD"),
                                 tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertLogical(
    x = as.logical(bulkLoad),
    any.missing = TRUE,
    null.ok = TRUE,
    len = 1
  )
  checkmate::assertIntegerish(
    x = sourceCohortIds,
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
  checkmate::assertCharacter(
    x = cdmDatabaseSchema,
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
  checkmate::assertLogical(
    x = isTempTable,
    len = 1,
    null.ok = FALSE,
    add = errorMessages
  )
  # Check if reindexRules is a data frame
  checkmate::assertDataFrame(
    reindexRules,
    min.rows = 1,
    min.cols = 3,
    .var.name = "reindexRules",
    add = errorMessages
  )

  # Check for the presence of required columns
  requiredColumns <-
    c(
      "offsetId",
      "offsetStartValue",
      "offsetEndValue"
    )
  checkmate::assertNames(
    x = colnames(reindexRules),
    subset.of = requiredColumns,
    .var.name = "reindexRules",
    add = errorMessages
  )

  # Check that offsetId is unique
  checkmate::assertTRUE(
    length(reindexRules$offsetId) == reindexRules$offsetId |>
      unique() |>
      length(),
    add = errorMessages
  )

  # Check that offsetStartAnchor and offsetEndAnchor are either 'cohort_start_date' or 'cohort_end_date'
  validAnchors <- c("cohort_start_date", "cohort_end_date")
  checkmate::assertChoice(
    offsetStartAnchor,
    choices = validAnchors,
    .var.name = "offsetStartAnchor",
    add = errorMessages
  )
  checkmate::assertChoice(
    offsetEndAnchor,
    choices = validAnchors,
    .var.name = "offsetEndAnchor",
    add = errorMessages
  )

  # Check that offsetStartValue and offsetEndValue are integers
  checkmate::assertIntegerish(reindexRules$offsetStartValue,
    .var.name = "offsetStartValue",
    add = errorMessages
  )
  checkmate::assertIntegerish(reindexRules$offsetEndValue,
    .var.name = "offsetEndValue",
    add = errorMessages
  )
  checkmate::reportAssertions(collection = errorMessages)

  if (isTempTable) {
    if (!all(
      is.null(targetCohortDatabaseSchema),
      tableNameIsCompatibleWithTempTableName(tableName = targetCohortTable), !is.null(connection)
    )) {
      stop("Cannot output temp table - check input specifications")
    }
  }

  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  if (!isTempTable) {
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
          x = (((
            sourceCohortIds
          ) * 1000) + reindexRules$offsetId),
          y = cohortIdsInCohortTable |> unique()
        )
      if (length(conflicitingCohortIdsInTargetCohortTable) > 0) {
        stop("Target cohort id already in use in target cohort table")
      }
    } else {
      targetCohortTableExists <- DatabaseConnector::existsTable(
        connection = connection,
        databaseSchema = targetCohortDatabaseSchema,
        tableName = targetCohortTable
      )

      if (!targetCohortTableExists) {
        stop(
          paste0(
            "Target cohort table ",
            targetCohortTable,
            " does not exists in",
            targetCohortDatabaseSchema
          )
        )
      }
    }
  }

  reindexTableName <- uploadTempTable(
    connection = connection,
    data = reindexRules,
    tempEmulationSchema = tempEmulationSchema,
    bulkLoad = bulkLoad,
    camelCaseToSnakeCase = TRUE
  )

  tempTableName <- paste0("#", getUniqueString())

  sql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = "ReindexCohorts.sql",
    packageName = utils::packageName(),
    dbms = connection@dbms,
    tempEmulationSchema = tempEmulationSchema,
    cdm_database_schema = cdmDatabaseSchema,
    source_cohort_database_schema = sourceCohortDatabaseSchema,
    source_cohort_table = sourceCohortTable,
    source_cohort_id = sourceCohortIds,
    reindex_rules_table = reindexTableName,
    offset_start_anchor = offsetStartAnchor,
    offset_end_anchor = offsetEndAnchor,
    temp_output_table = tempTableName
  )
  DatabaseConnector::executeSql(
    connection = connection,
    sql = sql,
    profile = FALSE,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )

  cohortIdsInCohortTable <-
    getCohortIdsInCohortTable(
      connection = connection,
      cohortTable = tempTableName,
      tempEmulationSchema = tempEmulationSchema
    ) |>
    unique() |>
    sort()

  oldToNewCohortId <-
    dplyr::tibble(
      oldCohortId = cohortIdsInCohortTable,
      newCohortId = cohortIdsInCohortTable
    )

  CohortAlgebra::unionCohorts(
    connection = connection,
    sourceCohortTable = tempTableName,
    targetCohortDatabaseSchema = targetCohortDatabaseSchema,
    targetCohortTable = if (isTempTable) {
      paste0("#", tempTableName)
    } else {
      targetCohortTable
    },
    isTempTable = isTempTable,
    tempEmulationSchema = tempEmulationSchema,
    purgeConflicts = purgeConflicts,
    oldToNewCohortId = oldToNewCohortId
  )

  if (isTempTable) {
    return(tempTableName)
  }
}
