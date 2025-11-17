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

#' Append cohort data from multiple cohort tables(s)
#'
#' @description
#' Append cohort data from multiple cohort tables.
#'
#' `r lifecycle::badge("stable")`
#'
#' @template ConnectionDetails
#'
#' @template Connection
#'
#' @param sourceTables    A data.frame object with the columns sourceCohortDatabaseSchema, sourceCohortTableName.
#'
#' @template targetCohortDatabaseSchema
#'
#' @template targetCohortTable
#'
#' @template IsTempTable
#'
#' @template TempEmulationSchema
#'
#' @return   Nothing is returned
#'
#'
#' @export
appendCohortTables <- function(connectionDetails = NULL,
                               connection = NULL,
                               sourceTables,
                               targetCohortDatabaseSchema = NULL,
                               targetCohortTable,
                               isTempTable = FALSE,
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
    x = sourceTables,
    min.rows = 1,
    add = errorMessages
  )
  checkmate::assertNames(
    x = colnames(sourceTables),
    must.include = c("sourceCohortDatabaseSchema", "sourceCohortTableName"),
    add = errorMessages
  )
  checkmate::assertCharacter(
    x = sourceTables$sourceCohortDatabaseSchema,
    min.len = 1,
    null.ok = TRUE,
    add = errorMessages
  )
  checkmate::assertCharacter(
    x = sourceTables$sourceCohortTableName,
    min.len = 1,
    null.ok = FALSE,
    add = errorMessages
  )
  checkmate::reportAssertions(collection = errorMessages)

  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  sqlNest <- c()
  for (i in (1:nrow(sourceTables))) {
    if (all(
      !is.na(sourceTables[i, ]$sourceCohortDatabaseSchema),
      nchar(sourceTables[i, ]$sourceCohortDatabaseSchema) > 1
    )) {
      tableName <- paste0(
        sourceTables[i, ]$sourceCohortDatabaseSchema,
        ".",
        sourceTables[i, ]$sourceCohortTableName
      )
    } else {
      tableName <- sourceTables[i, ]$sourceCohortTableName
    }

    sqlNest[[i]] <- paste0(
      "SELECT CAST(cohort_definition_id AS BIGINT) cohort_definition_id,
               CAST(subject_id AS BIGINT) subject_id,
               CAST(cohort_start_date AS DATE) cohort_start_date,
               CAST(cohort_end_date AS DATE) cohort_end_date
       FROM ",
      tableName,
      " d",
      i,
      " "
    )
  }

  if (isTempTable) {
    sql <- paste0(
      "SELECT CAST(cohort_definition_id AS BIGINT) cohort_definition_id,
               CAST(subject_id AS BIGINT) subject_id,
               CAST(cohort_start_date AS DATE) cohort_start_date,
               CAST(cohort_end_date AS DATE) cohort_end_date
          INTO @temp_table_name
      FROM (",
      paste0(paste0(sqlNest, collapse = " union all "), "
            "),
      ") f
      "
    )

    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql,
      progressBar = FALSE,
      reportOverallTime = FALSE,
      tempEmulationSchema = tempEmulationSchema,
      temp_table_name = targetCohortTable
    )
  } else {
    sql <- paste0(
      "  INSERT INTO
      {
        @target_cohort_database_schema != ''
      } ? {
        @target_cohort_database_schema.@target_cohort_table
      }:{
        @target_cohort_table
      } (cohort_definition_id,
          subject_id,
          cohort_start_date,
          cohort_end_date)
      SELECT  CAST(cohort_definition_id AS BIGINT) cohort_definition_id,
              CAST(subject_id AS BIGINT) subject_id,
              CAST(cohort_start_date AS DATE) cohort_start_date,
              CAST(cohort_end_date AS DATE) cohort_end_date
      FROM (",
      paste0(paste0(sqlNest, collapse = " UNION ALL ")),
      ") f
      "
    )
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql,
      progressBar = FALSE,
      reportOverallTime = FALSE,
      tempEmulationSchema = tempEmulationSchema,
      target_cohort_database_schema = targetCohortDatabaseSchema,
      target_cohort_table = targetCohortTable
    )
  }
}
