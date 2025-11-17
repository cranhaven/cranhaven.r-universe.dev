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

#' Delete cohort
#'
#' @description
#' Delete all records for a given set of cohorts from the cohort table. Edit privileges
#' to the cohort table is required.
#'
#' `r lifecycle::badge("stable")`
#'
#' @template ConnectionDetails
#'
#' @template Connection
#'
#' @template CohortTable
#'
#' @template CohortIds
#'
#' @template CohortDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @returns   Nothing is returned
#'
deleteCohort <- function(connectionDetails = NULL,
                         connection = NULL,
                         cohortDatabaseSchema,
                         cohortTable = "cohort",
                         tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                         cohortIds) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(
    x = cohortIds,
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
  checkmate::reportAssertions(collection = errorMessages)

  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = " DELETE
            FROM {@cohort_database_schema != ''} ? {@cohort_database_schema.@cohort_table} : {@cohort_table}
            WHERE cohort_definition_id IN (@cohort_ids);
            UPDATE STATISTICS  {@cohort_database_schema != ''} ? {@cohort_database_schema.@cohort_table} : {@cohort_table};",
    profile = FALSE,
    progressBar = FALSE,
    reportOverallTime = FALSE,
    cohort_database_schema = cohortDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    cohort_table = cohortTable,
    cohort_ids = cohortIds
  )
}
