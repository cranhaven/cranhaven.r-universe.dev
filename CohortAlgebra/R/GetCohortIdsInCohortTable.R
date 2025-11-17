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

#' Get cohort ids in table
#'
#' @description
#' Get cohort ids in table
#'
#' `r lifecycle::badge("stable")`
#'
#' @template Connection
#'
#' @template CohortTable
#'
#' @template CohortDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @returns  An array of integers called cohort id.
#'
getCohortIdsInCohortTable <- function(connection = NULL,
                                      cohortDatabaseSchema = NULL,
                                      cohortTable,
                                      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")) {
  cohortIdsInCohortTable <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = "SELECT DISTINCT cohort_definition_id cohort_definition_id
             FROM {@cohort_database_schema != ''} ? {@cohort_database_schema.@cohort_table} : {@cohort_table};",
      snakeCaseToCamelCase = TRUE,
      cohort_database_schema = cohortDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema,
      cohort_table = cohortTable
    ) |>
    dplyr::pull(.data$cohortDefinitionId) |>
    unique() |>
    sort()
  return(cohortIdsInCohortTable)
}
