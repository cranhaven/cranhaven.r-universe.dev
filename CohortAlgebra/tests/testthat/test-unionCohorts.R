testthat::test_that("Testing cohort union", {
  testthat::skip_if(condition = skipCdmTests)

  tempCohortTableName1 <- paste0("#", cohortTableName, "_1")

  # make up date for a cohort table
  cohort <- dplyr::tibble(
    cohortDefinitionId = c(1, 2, 2),
    subjectId = c(1, 1, 1),
    cohortStartDate = c(
      as.Date("2022-01-01"),
      as.Date("2022-02-10"),
      as.Date("2022-08-15")
    ),
    cohortEndDate = c(
      as.Date("2022-03-01"),
      as.Date("2022-05-10"),
      as.Date("2022-12-30")
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
  unionCohorts(
    connectionDetails = connectionDetails,
    sourceCohortDatabaseSchema = cohortDatabaseSchema,
    sourceCohortTable = cohortTableName,
    oldToNewCohortId = dplyr::tibble(
      oldCohortId = c(1, 2, 2),
      newCohortId = c(3, 3, 3)
    ),
    targetCohortDatabaseSchema = cohortDatabaseSchema,
    targetCohortTable = cohortTableName,
    tempEmulationSchema = tempEmulationSchema,
    purgeConflicts = FALSE
  )

  # extract the generated output and compare to expected
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  dataPostUnion <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = paste0(
        "SELECT * FROM @cohort_database_schema.@table_name
        where cohort_definition_id = 3
        order by cohort_definition_id, subject_id, cohort_start_date;"
      ),
      cohort_database_schema = cohortDatabaseSchema,
      table_name = cohortTableName,
      snakeCaseToCamelCase = TRUE
    ) |>
    dplyr::tibble()

  testthat::expect_equal(
    object = nrow(dataPostUnion),
    expected = 2
  ) # union logic should collapse to 2 rows

  # create the expected output data frame object to compare
  cohortExpected <- dplyr::tibble(
    cohortDefinitionId = c(3, 3),
    subjectId = c(1, 1),
    cohortStartDate = c(as.Date("2022-01-01"), as.Date("2022-08-15")),
    cohortEndDate = c(as.Date("2022-05-10"), as.Date("2022-12-30"))
  )

  testthat::expect_true(object = all(dataPostUnion == cohortExpected))

  testthat::expect_error(
    unionCohorts(
      connectionDetails = connectionDetails,
      sourceCohortDatabaseSchema = cohortDatabaseSchema,
      sourceCohortTable = cohortTableName,
      oldToNewCohortId = dplyr::tibble(
        oldCohortId = c(1, 2, 2),
        newCohortId = c(3, 3, 3)
      ),
      targetCohortDatabaseSchema = cohortDatabaseSchema,
      targetCohortTable = cohortTableName,
      tempEmulationSchema = tempEmulationSchema,
      isTempTable = TRUE
    )
  )

  testthat::expect_error(
    unionCohorts(
      connectionDetails = connectionDetails,
      sourceCohortDatabaseSchema = cohortDatabaseSchema,
      sourceCohortTable = cohortTableName,
      oldToNewCohortId = dplyr::tibble(
        oldCohortId = c(1, 2, 2),
        newCohortId = c(1, 2, 2)
      ),
      targetCohortDatabaseSchema = cohortDatabaseSchema,
      targetCohortTable = cohortTableName,
      tempEmulationSchema = tempEmulationSchema,
      isTempTable = TRUE,
      purgeConflicts = FALSE
    )
  )

  unionCohorts(
    connection = connection,
    sourceCohortDatabaseSchema = cohortDatabaseSchema,
    sourceCohortTable = cohortTableName,
    oldToNewCohortId = dplyr::tibble(
      oldCohortId = c(1, 2, 2),
      newCohortId = c(3, 3, 3)
    ),
    tempEmulationSchema = tempEmulationSchema,
    targetCohortDatabaseSchema = NULL,
    targetCohortTable = tempCohortTableName1,
    isTempTable = TRUE
  )

  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = "DROP TABLE IF EXISTS @cohort_database_schema.@table_temp;
          DROP TABLE IF EXISTS @cohort_database_schema.@table_temp2;
          DROP TABLE IF EXISTS @temp_temp_1;
           DROP TABLE IF EXISTS @cdm_database_schema.observation_period;",
    table_temp = cohortTableName,
    cohort_database_schema = cohortDatabaseSchema,
    cdm_database_schema = cohortDatabaseSchema,
    temp_temp_1 = tempCohortTableName1,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )

  DatabaseConnector::disconnect(connection = connection)
})
