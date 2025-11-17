testthat::test_that("Testing cohort era fy", {
  testthat::skip_if(condition = skipCdmTests)
  # generate unique name for a cohort table
  tempCohortTableName <- paste0("#", cohortTableName, "_1")

  # make up date for a cohort table
  # this cohort table will have two subjects * two cohorts, within the same cohort
  cohort <- dplyr::tibble(
    cohortDefinitionId = c(1, 1, 1),
    subjectId = c(1, 1, 1),
    cohortStartDate = c(
      as.Date("1999-01-01"),
      as.Date("1999-01-20"),
      as.Date("1999-03-10")
    ),
    cohortEndDate = c(
      as.Date("1999-01-31"),
      as.Date("1999-02-28"),
      as.Date("1999-03-31")
    )
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
    personId = c(1, 1, 2),
    observation_period_start_date = c(
      as.Date("1999-01-01"),
      as.Date("1999-03-06"),
      as.Date("1998-01-01")
    ),
    observation_period_end_date = c(
      as.Date("1999-03-04"),
      as.Date("1999-04-30"),
      as.Date("2000-12-31")
    )
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
  # disconnecting - as this is a test for a non temp cohort table
  DatabaseConnector::disconnect(connection)

  # should not throw error
  CohortAlgebra:::eraFyCohorts(
    connectionDetails = connectionDetails,
    sourceCohortDatabaseSchema = cohortDatabaseSchema,
    targetCohortDatabaseSchema = cohortDatabaseSchema,
    sourceCohortTable = cohortTableName,
    targetCohortTable = cohortTableName,
    oldCohortIds = 1,
    newCohortId = 9,
    eraconstructorpad = 0,
    purgeConflicts = FALSE,
    tempEmulationSchema = tempEmulationSchema
  )

  # extract the generated output and compare to expected
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  dataPostEraFy <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = paste0(
        "SELECT * FROM @cohort_database_schema.@table_name
        where cohort_definition_id = 9
        order by cohort_definition_id, subject_id, cohort_start_date;"
      ),
      cohort_database_schema = cohortDatabaseSchema,
      table_name = cohortTableName,
      snakeCaseToCamelCase = TRUE
    ) |>
    dplyr::tibble()

  testthat::expect_equal(
    object = nrow(dataPostEraFy),
    expected = 4
  ) # era fy logic should collapse to 4 rows

  # create the expected output data frame object to compare
  cohortExpected <- dplyr::tibble(
    cohortDefinitionId = c(9, 9),
    subjectId = c(1, 1),
    cohortStartDate = c(as.Date("1999-01-01"), as.Date("1999-03-10")),
    cohortEndDate = c(as.Date("1999-02-28"), as.Date("1999-03-31"))
  )
  cohortExpected <- dplyr::bind_rows(
    cohortExpected,
    cohortExpected |>
      dplyr::mutate(subjectId = 2)
  ) |>
    dplyr::distinct() |>
    dplyr::arrange(
      .data$cohortDefinitionId,
      .data$subjectId,
      .data$cohortStartDate
    )

  testthat::expect_true(object = all(dataPostEraFy == cohortExpected))

  # this should throw error as there is already a cohort with cohort_definition_id = 9
  testthat::expect_error(
    CohortAlgebra:::eraFyCohorts(
      connectionDetails = connectionDetails,
      sourceCohortDatabaseSchema = cohortDatabaseSchema,
      sourceCohortTable = cohortTableName,
      targetCohortDatabaseSchema = cohortDatabaseSchema,
      targetCohortTable = cohortTableName,
      oldCohortIds = 1,
      newCohortId = 9,
      eraconstructorpad = 0,
      purgeConflicts = FALSE
    )
  )

  CohortAlgebra:::eraFyCohorts(
    connectionDetails = connectionDetails,
    sourceCohortDatabaseSchema = cohortDatabaseSchema,
    sourceCohortTable = cohortTableName,
    targetCohortDatabaseSchema = cohortDatabaseSchema,
    targetCohortTable = cohortTableName,
    oldCohortIds = 1,
    newCohortId = 9,
    eraconstructorpad = 0,
    purgeConflicts = TRUE,
    tempEmulationSchema = tempEmulationSchema
  )

  # check era padding -on temporary table
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = paste0(
      "
      DROP TABLE IF EXISTS @temp_table_name;
      SELECT *
      INTO @temp_table_name
      FROM @cohort_database_schema.@table_name
      WHERE cohort_definition_id IN (1,2);"
    ),
    cohort_database_schema = cohortDatabaseSchema,
    table_name = cohortTableName,
    profile = FALSE,
    progressBar = FALSE,
    reportOverallTime = FALSE,
    temp_table_name = tempCohortTableName
  )

  testthat::expect_error(
    object = # throw error because cdmDatabaseSchema is not provided
      CohortAlgebra:::eraFyCohorts(
        connection = connection,
        sourceCohortTable = tempCohortTableName,
        targetCohortTable = tempCohortTableName,
        oldCohortId = 1,
        newCohortId = 10,
        eraconstructorpad = 30,
        purgeConflicts = FALSE,
        tempEmulationSchema = tempEmulationSchema
      )
  )

  # this should NOT throw error as we will purge conflicts.
  # it should return a message
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

  CohortAlgebra:::eraFyCohorts(
    connection = connection,
    sourceCohortTable = tempCohortTableName,
    targetCohortTable = tempCohortTableName,
    oldCohortIds = 1,
    newCohortId = 10,
    eraconstructorpad = 30,
    purgeConflicts = FALSE,
    cdmDatabaseSchema = cohortDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema
  )

  testthat::expect_error(
    CohortAlgebra:::eraFyCohorts(
      connection = connection,
      sourceCohortTable = tempCohortTableName,
      targetCohortTable = tempCohortTableName,
      targetCohortDatabaseSchema = cohortDatabaseSchema,
      oldCohortIds = 1,
      newCohortId = 10,
      eraconstructorpad = 30,
      purgeConflicts = FALSE,
      isTempTable = TRUE,
      # cant say temp table when it is a permanent table
      cdmDatabaseSchema = cohortDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema
    )
  )

  dataPostEraFyWithEraPad <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = paste0(
        "SELECT * FROM @table_name
        where cohort_definition_id = 10
        order by cohort_definition_id, subject_id, cohort_start_date;"
      ),
      table_name = tempCohortTableName,
      snakeCaseToCamelCase = TRUE
    ) |>
    dplyr::tibble()

  cohortExpectedEraPad <- dplyr::tibble(
    cohortDefinitionId = c(10, 10),
    subjectId = c(1, 2),
    cohortStartDate = c(as.Date("1999-01-01"), as.Date("1999-01-01")),
    cohortEndDate = c(as.Date("1999-03-04"), as.Date("1999-03-31"))
  ) |>
    dplyr::arrange(
      .data$cohortDefinitionId,
      .data$subjectId,
      .data$cohortStartDate
    )

  # this should throw error as there is already a cohort with cohort_definition_id = 10
  testthat::expect_error(
    eraFyCohorts(
      connectionDetails = connectionDetails,
      sourceCohortTable = tempCohortTableName,
      targetCohortTable = targetCohortTable,
      oldCohortIds = 1,
      newCohortId = 10,
      eraconstructorpad = 30,
      purgeConflicts = FALSE
    )
  )

  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = paste0(
      "
      DROP TABLE IF EXISTS @temp_table_name;
      DROP TABLE IF EXISTS @cohort_database_schema.@table_name;"
    ),
    cohort_database_schema = cohortDatabaseSchema,
    table_name = cohortTableName,
    profile = FALSE,
    progressBar = FALSE,
    reportOverallTime = FALSE,
    temp_table_name = tempCohortTableName
  )

  DatabaseConnector::disconnect(connection)
  testthat::expect_true(object = all.equal(target = dataPostEraFyWithEraPad, current = cohortExpectedEraPad))

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
