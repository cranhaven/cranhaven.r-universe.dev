testthat::test_that("Testing reindex cohorts", {
  testthat::skip_if(condition = skipCdmTests)

  # make up date for a cohort table
  # this cohort table will have two subjects * two cohorts, within the same cohort
  cohort <- dplyr::tibble(
    cohortDefinitionId = c(1),
    subjectId = c(1),
    cohortStartDate = c(as.Date("1999-01-01")),
    cohortEndDate = c(as.Date("1999-01-31"))
  )
  cohort <- dplyr::bind_rows(
    cohort,
    cohort |> dplyr::mutate(subjectId = 2)
  )
  cohort <- dplyr::bind_rows(
    cohort,
    cohort |> dplyr::mutate(cohortDefinitionId = 2)
  ) |>
    dplyr::arrange(
      .data$subjectId,
      .data$cohortStartDate,
      .data$cohortEndDate
    )

  observationPeriod <- dplyr::tibble(
    personId = c(1),
    observation_period_start_date = c(as.Date("1998-12-01")),
    observation_period_end_date = c(as.Date("2033-03-04"))
  )

  # upload table
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = cohortDatabaseSchema,
    tableName = cohortTableName,
    data = cohort,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE,
    camelCaseToSnakeCase = TRUE,
    progressBar = FALSE
  )
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = cohortDatabaseSchema,
    tableName = "observation_period",
    data = observationPeriod,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE,
    camelCaseToSnakeCase = TRUE,
    progressBar = FALSE
  )
  # disconnecting - as this is a test for a non temp cohort table
  DatabaseConnector::disconnect(connection)

  reindexRules <- dplyr::tibble(
    offsetStartValue = c(-100),
    offsetEndValue = c(100),
    offsetId = 1
  )

  # should not throw error
  CohortAlgebra:::reindexCohortsByDays(
    connectionDetails = connectionDetails,
    sourceCohortDatabaseSchema = cohortDatabaseSchema,
    targetCohortDatabaseSchema = cohortDatabaseSchema,
    sourceCohortTable = cohortTableName,
    targetCohortTable = cohortTableName,
    cdmDatabaseSchema = cohortDatabaseSchema,
    reindexRules = reindexRules,
    sourceCohortIds = c(1),
    purgeConflicts = FALSE,
    tempEmulationSchema = tempEmulationSchema
  )

  # extract the generated output and compare to expected
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  dataPostReindex <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = paste0(
        " SELECT * FROM @cohort_database_schema.@table_name
          where cohort_definition_id IN (1001)
          ORDER BY cohort_definition_id, subject_id, cohort_start_date;"
      ),
      cohort_database_schema = cohortDatabaseSchema,
      table_name = cohortTableName,
      snakeCaseToCamelCase = TRUE
    ) |>
    dplyr::tibble()

  testthat::expect_equal(
    object = nrow(dataPostReindex),
    expected = 1
  ) # era fy logic should collapse to 4 rows

  cohortExpected <- cohortExpected <- dplyr::tibble(
    cohortDefinitionId = c(1001),
    subjectId = c(1),
    cohortStartDate = c(as.Date("1998-12-01")),
    cohortEndDate = c(as.Date("1999-05-11"))
  )
  testthat::expect_true(object = all(dataPostReindex == cohortExpected))

  CohortAlgebra:::reindexCohortsByDays(
    connectionDetails = connectionDetails,
    sourceCohortDatabaseSchema = cohortDatabaseSchema,
    targetCohortDatabaseSchema = cohortDatabaseSchema,
    sourceCohortTable = cohortTableName,
    targetCohortTable = cohortTableName,
    cdmDatabaseSchema = cohortDatabaseSchema,
    reindexRules = reindexRules,
    sourceCohortIds = c(1),
    purgeConflicts = TRUE,
    tempEmulationSchema = tempEmulationSchema
  )
  testthat::expect_error(
    CohortAlgebra:::reindexCohortsByDays(
      connectionDetails = connectionDetails,
      sourceCohortDatabaseSchema = cohortDatabaseSchema,
      targetCohortDatabaseSchema = cohortDatabaseSchema,
      sourceCohortTable = cohortTableName,
      targetCohortTable = cohortTableName,
      cdmDatabaseSchema = cohortDatabaseSchema,
      reindexRules = reindexRules,
      sourceCohortIds = c(1),
      purgeConflicts = FALSE,
      tempEmulationSchema = tempEmulationSchema
    )
  )
  testthat::expect_error(
    CohortAlgebra:::reindexCohortsByDays(
      connectionDetails = connectionDetails,
      sourceCohortDatabaseSchema = cohortDatabaseSchema,
      targetCohortDatabaseSchema = cohortDatabaseSchema,
      sourceCohortTable = cohortTableName,
      targetCohortTable = paste(cohortTableName, "re"),
      # cohort does not exist
      cdmDatabaseSchema = cohortDatabaseSchema,
      reindexRules = reindexRules,
      sourceCohortIds = c(1),
      purgeConflicts = FALSE,
      tempEmulationSchema = tempEmulationSchema
    )
  )
  testthat::expect_error(
    CohortAlgebra:::reindexCohortsByDays(
      connectionDetails = connectionDetails,
      sourceCohortDatabaseSchema = cohortDatabaseSchema,
      targetCohortDatabaseSchema = cohortDatabaseSchema,
      sourceCohortTable = cohortTableName,
      targetCohortTable = cohortTableName,
      cdmDatabaseSchema = cohortDatabaseSchema,
      reindexRules = reindexRules,
      sourceCohortIds = c(1),
      purgeConflicts = FALSE,
      tempEmulationSchema = tempEmulationSchema,
      isTempTable = TRUE # claims its a temp table when it cannot one.
    )
  )

  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = paste0(
      "
      DROP TABLE IF EXISTS @cohort_database_schema.@table_name;"
    ),
    cohort_database_schema = cohortDatabaseSchema,
    table_name = cohortTableName,
    profile = FALSE,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )

  DatabaseConnector::disconnect(connection)

  DatabaseConnector::renderTranslateExecuteSql(
    connection = DatabaseConnector::connect(connectionDetails = connectionDetails),
    sql = "DROP TABLE IF EXISTS @cohort_database_schema.@table_temp;
           DROP TABLE IF EXISTS @cdm_database_schema.observation_period;",
    table_temp = cohortTableName,
    cohort_database_schema = cohortDatabaseSchema,
    cdm_database_schema = cohortDatabaseSchema,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
})
