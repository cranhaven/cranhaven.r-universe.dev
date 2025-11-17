# Copyright 2023 Observational Health Data Sciences and Informatics
#
# This file is part of CohortExplorer
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

#' Create Cohort explorer shiny app with person level data
#'
#' @description
#' Export person level data from OMOP CDM tables for eligible persons in the cohort. Creates a folder with files
#' that are part of the Cohort Explorer 'shiny' app. This app may then be run to review person level profiles.
#'
#' @template Connection
#'
#' @template CohortDatabaseSchema
#'
#' @template CdmDatabaseSchema
#'
#' @template VocabularyDatabaseSchema
#'
#' @template CohortTable
#'
#' @template TempEmulationSchema
#'
#' @param cohortDefinitionId          The cohort id to extract records.
#'
#' @param cohortName                  (optional) Cohort Name
#'
#' @param doNotExportCohortData       (Optional) Do you want to not export cohort data? If set to true, parameters
#'                                    cohortDefinitionId, cohort, cohortDatabaseSchema, cohortName will be ignored.
#'                                    The persons entire observation period would be considered the cohort. Cohort
#'                                    Name will be 'Observation Period', cohort id will be set to 0.
#' @param sampleSize                  (Optional, default = 20) The number of persons to randomly sample.
#'                                    Ignored, if personId is given.
#'
#' @param personIds                   (Optional) An array of personId's to look for in Cohort table and CDM.
#'
#' @param exportFolder                The folder where the output will be exported to. If this folder does
#'                                    not exist it will be created.
#' @param databaseId                  A short string for identifying the database (e.g. 'Synpuf'). This will
#'                                    be displayed in 'shiny' app to toggle between databases. Should not have
#'                                    space or underscore (_).
#' @param shiftDates                  (Default = FALSE) Do you want to shift dates? This will help further
#'                                    de-identify data. The shift is the process of re calibrating dates such
#'                                    that all persons mi (observation_period_start_date) is 2000-01-01.
#' @param assignNewId                 (Default = FALSE) Do you want to assign a newId for persons. This will
#'                                    replace the personId in the source with a randomly assigned newId.
#' @param featureCohortDatabaseSchema The CohortDatabaseSchema where the feature cohort table exits.
#' @param featureCohortTable          The Cohort table where feature cohorts are instantiated.
#' @param featureCohortDefinitionSet  The CohortDefinitionSet object corresponding to the cohorts to
#'                                    be used as features.
#' @returns                           Returns invisibly the full path of the export folder where the
#'                                    files were created. In this path are the files that are part of the 'shiny'
#'                                    app.
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(
#'   dbms = "postgresql",
#'   server = "ohdsi.com",
#'   port = 5432,
#'   user = "me",
#'   password = "secure"
#' )
#'
#' createCohortExplorerApp(
#'   connectionDetails = connectionDetails,
#'   cohortDefinitionId = 1234
#' )
#' }
#'
#' @export
createCohortExplorerApp <- function(connectionDetails = NULL,
                                    connection = NULL,
                                    cohortDatabaseSchema = NULL,
                                    cdmDatabaseSchema,
                                    vocabularyDatabaseSchema = cdmDatabaseSchema,
                                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                    cohortTable = "cohort",
                                    cohortDefinitionId,
                                    cohortName = NULL,
                                    doNotExportCohortData = FALSE,
                                    sampleSize = 25,
                                    personIds = NULL,
                                    featureCohortDatabaseSchema = NULL,
                                    featureCohortTable = NULL,
                                    featureCohortDefinitionSet = NULL,
                                    exportFolder,
                                    databaseId,
                                    shiftDates = FALSE,
                                    assignNewId = FALSE) {
  errorMessage <- checkmate::makeAssertCollection()

  checkmate::assertLogical(
    x = doNotExportCohortData,
    any.missing = FALSE,
    len = 1,
    min.len = 1,
    null.ok = FALSE,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  if (doNotExportCohortData) {
    cohortDatabaseSchema <- cdmDatabaseSchema
    cohortDefinitionId <- 0
    cohortName <- "Observation Period"
    cohortTable <- "observation_period"
  }

  checkmate::assertCharacter(
    x = cohortDatabaseSchema,
    min.len = 0,
    max.len = 1,
    null.ok = TRUE,
    add = errorMessage
  )

  checkmate::assertCharacter(
    x = cdmDatabaseSchema,
    min.len = 1,
    add = errorMessage
  )

  checkmate::assertCharacter(
    x = vocabularyDatabaseSchema,
    min.len = 1,
    add = errorMessage
  )

  checkmate::assertCharacter(
    x = cohortTable,
    min.len = 1,
    add = errorMessage
  )

  checkmate::assertCharacter(
    x = databaseId,
    min.len = 1,
    max.len = 1,
    add = errorMessage
  )

  checkmate::assertCharacter(
    x = tempEmulationSchema,
    min.len = 1,
    null.ok = TRUE,
    add = errorMessage
  )

  checkmate::assertIntegerish(
    x = cohortDefinitionId,
    lower = 0,
    len = 1,
    add = errorMessage
  )

  checkmate::assertIntegerish(
    x = sampleSize,
    lower = 0,
    len = 1,
    null.ok = TRUE,
    add = errorMessage
  )

  if (is.null(personIds)) {
    checkmate::assertIntegerish(
      x = sampleSize,
      lower = 0,
      len = 1,
      null.ok = TRUE,
      add = errorMessage
    )
  } else {
    checkmate::assertIntegerish(
      x = personIds,
      lower = 0,
      min.len = 1,
      null.ok = TRUE
    )
  }

  exportFolder <- normalizePath(exportFolder, mustWork = FALSE)

  dir.create(
    path = exportFolder,
    showWarnings = FALSE,
    recursive = TRUE
  )

  checkmate::assertDirectory(
    x = exportFolder,
    access = "x",
    add = errorMessage
  )

  useCohortDomain <- FALSE
  if (any(
    !is.null(featureCohortDefinitionSet),
    !is.null(featureCohortTable),
    !is.null(featureCohortDatabaseSchema)
  )) {
    checkmate::assertTRUE(x = !checkmate::allMissing(
      x = c(
        featureCohortDefinitionSet,
        featureCohortTable,
        featureCohortDatabaseSchema
      )
    ), add = errorMessage)
    checkmate::reportAssertions(collection = errorMessage)
    useCohortDomain <- TRUE
  }

  checkmate::reportAssertions(collection = errorMessage)

  ParallelLogger::addDefaultFileLogger(
    fileName = file.path(exportFolder, "log.txt"),
    name = "cohort_explorer_file_logger"
  )
  ParallelLogger::addDefaultErrorReportLogger(
    fileName = file.path(exportFolder, "errorReportR.txt"),
    name = "cohort_explorer_error_logger"
  )
  on.exit(ParallelLogger::unregisterLogger("cohort_explorer_file_logger", silent = TRUE))
  on.exit(
    ParallelLogger::unregisterLogger("cohort_explorer_error_logger", silent = TRUE),
    add = TRUE
  )

  originalDatabaseId <- databaseId

  cohortTableIsTemp <- FALSE
  if (is.null(cohortDatabaseSchema)) {
    if (grepl(
      pattern = "#",
      x = cohortTable,
      fixed = TRUE
    )) {
      cohortTableIsTemp <- TRUE
    } else {
      stop("cohortDatabaseSchema is NULL, but cohortTable is not temporary.")
    }
  }

  databaseId <- as.character(gsub(
    pattern = " ",
    replacement = "",
    x = databaseId
  ))

  if (nchar(databaseId) < nchar(originalDatabaseId)) {
    stop(paste0(
      "databaseId should not have space or underscore: ",
      originalDatabaseId
    ))
  }

  rdsFileName <- paste0(
    "CohortExplorer_",
    abs(cohortDefinitionId),
    "_",
    databaseId,
    ".rds"
  )

  # Set up connection to server ----------------------------------------------------
  if (is.null(connection)) {
    if (!is.null(connectionDetails)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    } else {
      stop("No connection or connectionDetails provided.")
    }
  }

  if (cohortTableIsTemp) {
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = " DROP TABLE IF EXISTS #person_id_data;
                SELECT DISTINCT subject_id
                INTO #person_id_data
                FROM @cohort_table
                WHERE cohort_definition_id = @cohort_definition_id;",
      cohort_table = cohortTable,
      tempEmulationSchema = tempEmulationSchema,
      cohort_definition_id = cohortDefinitionId
    )
  } else {
    if (doNotExportCohortData) {
      DatabaseConnector::renderTranslateExecuteSql(
        connection = connection,
        sql = " DROP TABLE IF EXISTS #person_id_data;
                  SELECT DISTINCT person_id subject_id
                  INTO #person_id_data
                  FROM @cdm_database_schema.observation_period;",
        cdm_database_schema = cdmDatabaseSchema
      )
    } else {
      DatabaseConnector::renderTranslateExecuteSql(
        connection = connection,
        sql = " DROP TABLE IF EXISTS #person_id_data;
                  SELECT DISTINCT subject_id
                  INTO #person_id_data
                  FROM @cohort_database_schema.@cohort_table
                  WHERE cohort_definition_id = @cohort_definition_id;",
        cohort_table = cohortTable,
        cohort_database_schema = cohortDatabaseSchema,
        tempEmulationSchema = tempEmulationSchema,
        cohort_definition_id = cohortDefinitionId
      )
    }
  }

  if (!is.null(personIds)) {
    DatabaseConnector::insertTable(
      connection = connection,
      tableName = "#persons_to_filter",
      createTable = TRUE,
      dropTableIfExists = TRUE,
      tempTable = TRUE,
      tempEmulationSchema = tempEmulationSchema,
      progressBar = TRUE,
      bulkLoad = (Sys.getenv("bulkLoad") == TRUE),
      camelCaseToSnakeCase = TRUE,
      data = dplyr::tibble(subjectId = as.double(personIds) |> unique())
    )

    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = "     DROP TABLE IF EXISTS #person_id_data2;
                  SELECT DISTINCT a.subject_id
                  INTO #person_id_data2
                  FROM #person_id_data a
                  INNER JOIN #persons_to_filter b
                  ON a.subject_id = b.subject_id;

                  DROP TABLE IF EXISTS #person_id_data;
                  SELECT DISTINCT subject_id
                  INTO #person_id_data
                  FROM #person_id_data2;

                  DROP TABLE IF EXISTS #person_id_data2;
                  ",
      tempEmulationSchema = tempEmulationSchema
    )
  }

  # assign new id and filter to sample size
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = "   DROP TABLE IF EXISTS #persons_filter;
              SELECT new_id, subject_id person_id
              INTO #persons_filter
              FROM
              (
                SELECT *
                FROM
                (
                  SELECT ROW_NUMBER() OVER (ORDER BY NEWID()) AS new_id, subject_id
                  FROM #person_id_data
                ) f
    ) t
    WHERE new_id <= @sample_size;",
    tempEmulationSchema = tempEmulationSchema,
    sample_size = sampleSize
  )

  if (cohortTableIsTemp) {
    writeLines("Getting cohort table.")
    cohort <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = "SELECT c.subject_id,
                    p.new_id,
                    c.cohort_start_date AS start_date,
                	  c.cohort_end_date AS end_date
              FROM @cohort_table c
              INNER JOIN #persons_filter p
              ON c.subject_id = p.person_id
              WHERE c.cohort_definition_id = @cohort_definition_id
              ORDER BY c.subject_id, c.cohort_start_date;",
      cohort_table = cohortTable,
      tempEmulationSchema = tempEmulationSchema,
      cohort_definition_id = cohortDefinitionId,
      snakeCaseToCamelCase = TRUE
    ) %>%
      dplyr::tibble()
  } else {
    writeLines("Getting cohort table.")
    cohort <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = "SELECT {!@do_not_export_cohort_data} ? {c.subject_id} : {c.person_id subject_id},
                  p.new_id,
                  {!@do_not_export_cohort_data} ? {cohort_start_date AS start_date,
              	                              cohort_end_date AS end_date} : {
              	                              observation_period_start_date AS start_date,
              	                              observation_period_end_date AS end_date
              	                              }
              FROM @cohort_database_schema.@cohort_table c
              INNER JOIN #persons_filter p
              ON {!@do_not_export_cohort_data} ? {c.subject_id} : {c.person_id} = p.person_id
              {!@do_not_export_cohort_data} ? {WHERE cohort_definition_id = @cohort_definition_id}
          ORDER BY {!@do_not_export_cohort_data} ? {c.subject_id} : {c.person_id},
                    {!@do_not_export_cohort_data} ? {cohort_start_date} : {observation_period_start_date};",
      cohort_database_schema = cohortDatabaseSchema,
      cohort_table = cohortTable,
      tempEmulationSchema = tempEmulationSchema,
      cohort_definition_id = cohortDefinitionId,
      do_not_export_cohort_data = doNotExportCohortData,
      snakeCaseToCamelCase = TRUE
    ) %>%
      dplyr::tibble()
  }

  if (nrow(cohort) == 0) {
    warning("Cohort does not have the selected subject ids. No shiny app created.")
    return(NULL)
  }

  writeLines("Getting person table.")
  person <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT p.person_id,
                    pf.new_id,
                gender_concept_id,
                year_of_birth
        FROM @cdm_database_schema.person p
        INNER JOIN #persons_filter pf
        ON p.person_id = pf.person_id
        ORDER BY p.person_id;",
    cdm_database_schema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  ) %>%
    dplyr::tibble()

  person <- person %>%
    dplyr::inner_join(
      cohort %>%
        dplyr::group_by(.data$subjectId) %>%
        dplyr::summarise(
          yearOfCohort = min(extractYear(.data$startDate)),
          .groups = "keep"
        ) %>%
        dplyr::ungroup() %>%
        dplyr::rename("personId" = "subjectId"),
      by = "personId"
    ) %>%
    dplyr::mutate("age" = .data$yearOfCohort - .data$yearOfBirth) %>%
    dplyr::select(-"yearOfCohort", -"yearOfBirth")

  writeLines("Getting observation period table.")
  observationPeriod <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT op.person_id,
                    p.new_id,
                    observation_period_start_date AS start_date,
                    observation_period_end_date AS end_date,
                    period_type_concept_id AS type_concept_id
              FROM @cdm_database_schema.observation_period op
              INNER JOIN #persons_filter p
              ON op.person_id = p.person_id
              ORDER BY op.person_id,
                      p.new_id,
                      observation_period_start_date,
                      observation_period_end_date;",
    cdm_database_schema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  ) %>%
    dplyr::tibble()

  writeLines("Getting visit occurrence table.")
  visitOccurrence <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT v.person_id,
              p.new_id,
              visit_start_date AS start_date,
              visit_end_date AS end_date,
              visit_concept_id AS concept_id,
          	  visit_type_concept_id AS type_concept_id,
          	  visit_source_concept_id AS source_concept_id,
          	  count(*) records
        FROM @cdm_database_schema.visit_occurrence v
        INNER JOIN #persons_filter p
        ON v.person_id = p.person_id
        GROUP BY v.person_id,
                  p.new_id,
                  visit_start_date,
                  visit_end_date,
                  visit_concept_id,
                  visit_type_concept_id,
                  visit_source_concept_id
        ORDER BY v.person_id,
                p.new_id,
                visit_start_date,
                visit_end_date,
                visit_concept_id,
                visit_type_concept_id,
                visit_source_concept_id;",
    cdm_database_schema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  ) %>%
    dplyr::tibble()

  writeLines("Getting condition occurrence table.")
  conditionOccurrence <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT c.person_id,
              p.new_id,
              condition_start_date AS start_date,
              condition_end_date AS end_date,
              condition_concept_id AS concept_id,
          	  condition_type_concept_id AS type_concept_id,
          	  condition_source_concept_id AS source_concept_id,
          	  count(*) records
        FROM @cdm_database_schema.condition_occurrence c
        INNER JOIN #persons_filter p
        ON c.person_id = p.person_id
        GROUP BY c.person_id,
                  p.new_id,
                  condition_start_date,
                  condition_end_date,
                  condition_concept_id,
                  condition_type_concept_id,
                  condition_source_concept_id
        ORDER BY c.person_id,
                  p.new_id,
                  condition_start_date,
                  condition_end_date,
                  condition_concept_id,
                  condition_type_concept_id,
                  condition_source_concept_id;",
    cdm_database_schema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  ) %>%
    dplyr::tibble()

  writeLines("Getting condition era table.")
  conditionEra <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT ce.person_id,
              p.new_id,
              condition_era_start_date AS start_date,
              condition_era_end_date AS end_date,
              condition_concept_id AS concept_id,
          	  count(*) records
        FROM @cdm_database_schema.condition_era ce
        INNER JOIN #persons_filter p
        ON ce.person_id = p.person_id
        GROUP BY ce.person_id,
              p.new_id,
              condition_era_start_date,
              condition_era_end_date,
              condition_concept_id
        ORDER BY ce.person_id,
              p.new_id,
              condition_era_start_date,
              condition_era_end_date,
              condition_concept_id;",
    cdm_database_schema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  ) %>%
    dplyr::tibble() %>%
    dplyr::mutate(typeConceptId = 0, records = 1)

  writeLines("Getting observation table.")
  observation <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT o.person_id,
              p.new_id,
              observation_date AS start_date,
              observation_concept_id AS concept_id,
          	  observation_type_concept_id AS type_concept_id,
          	  observation_source_concept_id AS source_concept_id,
          	  count(*) records
        FROM @cdm_database_schema.observation o
        INNER JOIN #persons_filter p
        ON o.person_id = p.person_id
        GROUP BY o.person_id,
                  p.new_id,
                  observation_date,
                  observation_concept_id,
                  observation_type_concept_id,
                  observation_source_concept_id
        ORDER BY o.person_id,
                p.new_id,
                observation_date,
                observation_concept_id,
                observation_type_concept_id;",
    cdm_database_schema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  ) %>%
    dplyr::tibble()

  writeLines("Getting procedure occurrence table.")
  procedureOccurrence <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT p.person_id,
              pf.new_id,
              procedure_date AS start_date,
              procedure_concept_id AS concept_id,
              procedure_type_concept_id AS type_concept_id,
              procedure_source_concept_id AS source_concept_id,
          	  count(*) records
        FROM @cdm_database_schema.procedure_occurrence p
        INNER JOIN #persons_filter pf
        ON p.person_id = pf.person_id
        GROUP BY p.person_id,
                  pf.new_id,
                  procedure_date,
                  procedure_concept_id,
                  procedure_type_concept_id,
                  procedure_source_concept_id
        ORDER BY p.person_id,
                pf.new_id,
                procedure_date,
                procedure_concept_id,
                procedure_type_concept_id,
                procedure_source_concept_id;",
    cdm_database_schema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  ) %>%
    dplyr::tibble() %>%
    dplyr::mutate(endDate = .data$startDate)

  writeLines("Getting drug exposure table.")
  drugExposure <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT de.person_id,
              pf.new_id,
              drug_exposure_start_date AS start_date,
              drug_exposure_end_date AS end_date,
              drug_concept_id AS concept_id,
              drug_type_concept_id AS type_concept_id,
              drug_source_concept_id AS source_concept_id,
          	  count(*) records
        FROM @cdm_database_schema.drug_exposure de
        INNER JOIN #persons_filter pf
        ON de.person_id = pf.person_id
        GROUP BY de.person_id,
                  pf.new_id,
                  drug_exposure_start_date,
                  drug_exposure_end_date,
                  drug_concept_id,
                  drug_type_concept_id,
                  drug_source_concept_id
        ORDER BY de.person_id,
                  pf.new_id,
                  drug_exposure_start_date,
                  drug_exposure_end_date,
                  drug_concept_id,
                  drug_type_concept_id,
                  drug_source_concept_id;",
    cdm_database_schema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  ) %>%
    dplyr::tibble()

  writeLines("Getting drug era table.")
  drugEra <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT de.person_id,
              pf.new_id,
              drug_era_start_date AS start_date,
              drug_era_end_date AS end_date,
              drug_concept_id AS concept_id,
          	  count(*) records
        FROM @cdm_database_schema.drug_era de
        INNER JOIN #persons_filter pf
        ON de.person_id = pf.person_id
        GROUP BY de.person_id,
                  pf.new_id,
                  drug_era_start_date,
                  drug_era_end_date,
                  drug_concept_id
        ORDER BY de.person_id,
                  pf.new_id,
                  drug_era_start_date,
                  drug_era_end_date,
                  drug_concept_id;",
    cdm_database_schema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  ) %>%
    dplyr::tibble() %>%
    dplyr::mutate(typeConceptId = 0)

  writeLines("Getting measurement table.")
  measurement <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT m.person_id,
              pf.new_id,
              measurement_date AS start_date,
              measurement_concept_id AS concept_id,
              measurement_type_concept_id as type_concept_id,
              measurement_source_concept_id as source_concept_id,
          	  count(*) records
        FROM @cdm_database_schema.measurement m
        INNER JOIN #persons_filter pf
        ON m.person_id = pf.person_id
        GROUP BY m.person_id,
                  pf.new_id,
                  measurement_date,
                  measurement_concept_id,
                  measurement_type_concept_id,
                  measurement_source_concept_id
        ORDER BY m.person_id,
                  pf.new_id,
                  measurement_date,
                  measurement_concept_id,
                  measurement_type_concept_id,
                  measurement_source_concept_id;",
    cdm_database_schema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  ) %>%
    dplyr::tibble() %>%
    dplyr::mutate(endDate = .data$startDate)

  featureCohortData <- NULL
  if (useCohortDomain) {
    writeLines("Getting feature cohort table.")
    featureCohortData <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = "SELECT f.subject_id person_id,
                  pf.new_id,
                  f.cohort_definition_id concept_id,
                  f.cohort_start_date AS start_date,
                  f.cohort_end_date AS end_date
            FROM @feature_cohort_database_schema.@feature_cohort_table f
            INNER JOIN #persons_filter pf
            ON f.subject_id = pf.person_id
            WHERE f.cohort_definition_id IN (@feature_cohort_definition_id);",
      feature_cohort_database_schema = featureCohortDatabaseSchema,
      feature_cohort_table = featureCohortTable,
      feature_cohort_definition_id = featureCohortDefinitionSet$cohortId,
      tempEmulationSchema = tempEmulationSchema,
      snakeCaseToCamelCase = TRUE
    ) %>%
      dplyr::tibble()
  }

  writeLines("Getting concept id.")
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = " DROP TABLE IF EXISTS #person_concepts;
            DROP TABLE IF EXISTS #obs_p_concepts;
            DROP TABLE IF EXISTS #observation_concept;
            DROP TABLE IF EXISTS #obs_typ_concept;
            DROP TABLE IF EXISTS #obs_src_concept;
            DROP TABLE IF EXISTS #drug_exp_concept;
            DROP TABLE IF EXISTS #drug_typ_concept;
            DROP TABLE IF EXISTS #drug_src_concept;
            DROP TABLE IF EXISTS #drug_era_concept;
            DROP TABLE IF EXISTS #visit_concept;
            DROP TABLE IF EXISTS #visit_typ_concept;
            DROP TABLE IF EXISTS #visit_src_concept;
            DROP TABLE IF EXISTS #proc_concept;
            DROP TABLE IF EXISTS #proc_typ_concept;
            DROP TABLE IF EXISTS #proc_src_concept;
            DROP TABLE IF EXISTS #cond_concept;
            DROP TABLE IF EXISTS #cond_typ_concept;
            DROP TABLE IF EXISTS #cond_src_concept;
            DROP TABLE IF EXISTS #cond_era_concept;
            DROP TABLE IF EXISTS #meas_concept;
            DROP TABLE IF EXISTS #meas_typ_concept;
            DROP TABLE IF EXISTS #meas_src_concept;

          	SELECT DISTINCT gender_concept_id AS CONCEPT_ID
          	INTO #person_concepts
          	FROM @cdm_database_schema.person p
          	INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT period_type_concept_id AS CONCEPT_ID
            INTO #obs_p_concepts
            FROM @cdm_database_schema.observation_period p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT observation_concept_id AS CONCEPT_ID
            INTO #observation_concept
            FROM @cdm_database_schema.observation p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT observation_type_concept_id AS CONCEPT_ID
            INTO #obs_typ_concept
            FROM @cdm_database_schema.observation p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT observation_source_concept_id AS CONCEPT_ID
            INTO #obs_src_concept
            FROM @cdm_database_schema.observation p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT drug_concept_id AS concept_id
            INTO #drug_exp_concept
            FROM @cdm_database_schema.drug_exposure p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT drug_type_concept_id AS concept_id
            INTO #drug_typ_concept
            FROM @cdm_database_schema.drug_exposure p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT drug_source_concept_id AS concept_id
            INTO #drug_src_concept
            FROM @cdm_database_schema.drug_exposure p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT drug_concept_id AS concept_id
            INTO #drug_era_concept
            FROM @cdm_database_schema.drug_era p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT visit_concept_id AS concept_id
            INTO #visit_concept
            FROM @cdm_database_schema.visit_occurrence p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT visit_type_concept_id AS concept_id
            INTO #visit_typ_concept
            FROM @cdm_database_schema.visit_occurrence p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT visit_source_concept_id AS concept_id
            INTO #visit_src_concept
            FROM @cdm_database_schema.visit_occurrence p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT procedure_concept_id AS concept_id
            INTO #proc_concept
            FROM @cdm_database_schema.procedure_occurrence p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT procedure_type_concept_id AS concept_id
            INTO #proc_typ_concept
            FROM @cdm_database_schema.procedure_occurrence p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT procedure_source_concept_id AS concept_id
            INTO #proc_src_concept
            FROM @cdm_database_schema.procedure_occurrence p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT condition_concept_id AS concept_id
            INTO #cond_concept
            FROM @cdm_database_schema.condition_occurrence p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT condition_type_concept_id AS concept_id
            INTO #cond_typ_concept
            FROM @cdm_database_schema.condition_occurrence p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT condition_source_concept_id AS concept_id
            INTO #cond_src_concept
            FROM @cdm_database_schema.condition_occurrence p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT condition_concept_id AS concept_id
            INTO #cond_era_concept
            FROM @cdm_database_schema.condition_era p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT measurement_concept_id AS concept_id
            INTO #meas_concept
            FROM @cdm_database_schema.measurement p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT measurement_type_concept_id AS concept_id
            INTO #meas_typ_concept
            FROM @cdm_database_schema.measurement p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            SELECT DISTINCT measurement_source_concept_id AS concept_id
            INTO #meas_src_concept
            FROM @cdm_database_schema.measurement p
            INNER JOIN #persons_filter pf ON p.person_id = pf.person_id;

            WITH concepts AS (
            		SELECT *
            		FROM #person_concepts

            		UNION ALL

            		SELECT *
            		FROM #obs_p_concepts

            		UNION ALL

            		SELECT *
            		FROM #observation_concept

            		UNION ALL

            		SELECT *
            		FROM #obs_typ_concept

            		UNION ALL

            		SELECT *
            		FROM #obs_src_concept

            		UNION ALL

            		SELECT *
            		FROM #drug_exp_concept

            		UNION ALL

            		SELECT *
            		FROM #drug_typ_concept

            		UNION ALL

            		SELECT *
            		FROM #drug_src_concept

            		UNION ALL

            		SELECT *
            		FROM #drug_era_concept

            		UNION ALL

            		SELECT *
            		FROM #visit_concept

            		UNION ALL

            		SELECT *
            		FROM #visit_typ_concept

            		UNION ALL

            		SELECT *
            		FROM #visit_src_concept

            		UNION ALL

            		SELECT *
            		FROM #proc_concept

            		UNION ALL

            		SELECT *
            		FROM #proc_typ_concept

            		UNION ALL

            		SELECT *
            		FROM #proc_src_concept

            		UNION ALL

            		SELECT *
            		FROM #cond_concept

            		UNION ALL

            		SELECT *
            		FROM #cond_typ_concept

            		UNION ALL

            		SELECT *
            		FROM #cond_src_concept

            		UNION ALL

            		SELECT *
            		FROM #cond_era_concept

            		UNION ALL

            		SELECT *
            		FROM #meas_concept

            		UNION ALL

            		SELECT *
            		FROM #meas_typ_concept

            		UNION

            		SELECT *
            		FROM #meas_src_concept
            		)

            SELECT DISTINCT c.concept_id,
            	c.domain_id,
            	c.concept_name,
            	c.vocabulary_id,
            	c.concept_code
            INTO #all_concepts
            FROM @vocabulary_database_schema.concept c
            INNER JOIN (
            	SELECT DISTINCT CONCEPT_ID
            	FROM concepts
            	) c2 ON c.concept_id = c2.concept_id
            ORDER BY c.concept_id;

            DROP TABLE IF EXISTS #person_concepts;
            DROP TABLE IF EXISTS #obs_p_concepts;
            DROP TABLE IF EXISTS #observation_concept;
            DROP TABLE IF EXISTS #obs_typ_concept;
            DROP TABLE IF EXISTS #obs_src_concept;
            DROP TABLE IF EXISTS #drug_exp_concept;
            DROP TABLE IF EXISTS #drug_typ_concept;
            DROP TABLE IF EXISTS #drug_src_concept;
            DROP TABLE IF EXISTS #drug_era_concept;
            DROP TABLE IF EXISTS #visit_concept;
            DROP TABLE IF EXISTS #visit_typ_concept;
            DROP TABLE IF EXISTS #visit_src_concept;
            DROP TABLE IF EXISTS #proc_concept;
            DROP TABLE IF EXISTS #proc_typ_concept;
            DROP TABLE IF EXISTS #proc_src_concept;
            DROP TABLE IF EXISTS #cond_concept;
            DROP TABLE IF EXISTS #cond_typ_concept;
            DROP TABLE IF EXISTS #cond_src_concept;
            DROP TABLE IF EXISTS #cond_era_concept;
            DROP TABLE IF EXISTS #meas_concept;
            DROP TABLE IF EXISTS #meas_typ_concept;
            DROP TABLE IF EXISTS #meas_src_concept;",
    cdm_database_schema = cdmDatabaseSchema,
    vocabulary_database_schema = vocabularyDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema
  )

  conceptIds <- DatabaseConnector::renderTranslateQuerySql(
    sql = "SELECT * FROM #all_concepts;",
    connection = connection,
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema
  ) %>%
    dplyr::tibble()

  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = " DROP TABLE IF EXISTS #persons_filter;
            DROP TABLE IF EXISTS #all_concepts;",
    tempEmulationSchema = tempEmulationSchema
  )

  cohort <- cohort %>%
    dplyr::rename("personId" = "subjectId")

  subjects <- cohort %>%
    dplyr::group_by(.data$personId) %>%
    dplyr::summarise(startDate = min(.data$startDate)) %>%
    dplyr::inner_join(person,
      by = "personId"
    ) %>%
    dplyr::inner_join(conceptIds,
      by = c("genderConceptId" = "conceptId")
    ) %>%
    dplyr::rename("gender" = "conceptName") %>%
    dplyr::ungroup()

  personMinObservationPeriodDate <- observationPeriod %>%
    dplyr::group_by(.data$personId) %>%
    dplyr::summarise(
      minObservationPeriodDate = min(.data$startDate),
      .groups = "keep"
    ) %>%
    dplyr::ungroup()

  shiftDatesInData <- function(data,
                               originDate = as.Date("2000-01-01"),
                               minObservationPeriodDate = personMinObservationPeriodDate) {
    data <- data %>%
      dplyr::inner_join(personMinObservationPeriodDate,
        by = "personId"
      )

    if ("startDate" %in% colnames(data)) {
      data <- data %>%
        dplyr::mutate(startDate = addDays(
          x = as.Date(originDate),
          n = as.integer(
            difftime(
              time1 = .data$startDate,
              time2 = .data$minObservationPeriodDate,
              units = "days"
            )
          )
        ))
    }

    if ("endDate" %in% colnames(data)) {
      data <- data %>%
        dplyr::mutate(endDate = addDays(
          x = as.Date(originDate),
          n = as.integer(
            difftime(
              time1 = .data$endDate,
              time2 = .data$minObservationPeriodDate,
              units = "days"
            )
          )
        ))
    }

    data <- data %>%
      dplyr::select(-minObservationPeriodDate)
  }

  if (shiftDates) {
    observationPeriod <- shiftDatesInData(data = observationPeriod)
    cohort <- shiftDatesInData(data = cohort)
    conditionEra <- shiftDatesInData(data = conditionEra)
    conditionOccurrence <- shiftDatesInData(
      data =
        conditionOccurrence
    )
    drugExposure <- shiftDatesInData(data = drugExposure)
    measurement <- shiftDatesInData(data = measurement)
    observation <- shiftDatesInData(data = observation)
    procedureOccurrence <- shiftDatesInData(
      data =
        procedureOccurrence
    )
    visitOccurrence <- shiftDatesInData(data = visitOccurrence)
    measurement <- shiftDatesInData(data = measurement)
  }

  cohort <- replaceId(data = cohort, useNewId = assignNewId)
  person <- replaceId(data = person, useNewId = assignNewId)
  subjects <- replaceId(data = subjects, useNewId = assignNewId)
  observationPeriod <- replaceId(
    data = observationPeriod,
    useNewId = assignNewId
  )
  visitOccurrence <- replaceId(
    data = visitOccurrence,
    useNewId = assignNewId
  )
  conditionOccurrence <- replaceId(
    data = conditionOccurrence,
    useNewId = assignNewId
  )
  conditionEra <- replaceId(
    data = conditionEra,
    useNewId = assignNewId
  )
  observation <- replaceId(
    data = observation,
    useNewId = assignNewId
  )
  procedureOccurrence <- replaceId(
    data = procedureOccurrence,
    useNewId = assignNewId
  )
  drugExposure <- replaceId(
    data = drugExposure,
    useNewId = assignNewId
  )
  drugEra <- replaceId(data = drugEra, useNewId = assignNewId)
  measurement <- replaceId(
    data = measurement,
    useNewId = assignNewId
  )

  if (!is.null(featureCohortData)) {
    featureCohortData <- replaceId(
      data = featureCohortData,
      useNewId = assignNewId
    )
  }

  results <- list(
    cohort = cohort,
    person = person,
    subjects = subjects,
    observationPeriod = observationPeriod,
    visitOccurrence = visitOccurrence,
    conditionOccurrence = conditionOccurrence,
    conditionEra = conditionEra,
    observation = observation,
    procedureOccurrence = procedureOccurrence,
    drugExposure = drugExposure,
    drugEra = drugEra,
    measurement = measurement,
    conceptId = conceptIds,
    cohortName = cohortName,
    featureCohortData = featureCohortData,
    featureCohortDefinitionSet = featureCohortDefinitionSet,
    assignNewId = assignNewId,
    shiftDates = shiftDates,
    sampleSize = sampleSize,
    sampleFound = nrow(subjects)
  )

  exportCohortExplorerAppFiles(exportFolder)

  dir.create(
    path = (file.path(
      exportFolder,
      "data"
    )),
    showWarnings = FALSE,
    recursive = TRUE
  )
  saveRDS(
    object = results,
    file = file.path(exportFolder, "data", rdsFileName)
  )

  message(
    sprintf(
      "The CohortExplorer shiny app has been created at '%s'.
                     Please view the README file in that folder for instructions
                     on how to run.",
      exportFolder
    )
  )
  return(invisible(exportFolder))
}
