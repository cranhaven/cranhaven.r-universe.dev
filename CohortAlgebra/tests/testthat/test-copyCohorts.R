testthat::test_that("Testing cohort union", {
  testthat::skip_if(condition = skipCdmTests)

  # generate unique name for a cohort table
  cohortTableName1 <- paste0(cohortTableName, 1)
  cohortTableName2 <- paste0(cohortTableName, 2)
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
  ) |>
    dplyr::arrange(
      cohortDefinitionId,
      subjectId,
      cohortStartDate,
      cohortEndDate
    )

  # upload table
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)

  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = cohortDatabaseSchema,
    tableName = cohortTableName1,
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
    tableName = cohortTableName2,
    data = cohort,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE,
    camelCaseToSnakeCase = TRUE,
    progressBar = FALSE
  )

  copyCohorts(
    connection = connection,
    oldToNewCohortId = dplyr::tibble(
      oldCohortId = c(1, 2),
      newCohortId = c(1, 2)
    ),
    sourceCohortDatabaseSchema = cohortDatabaseSchema,
    targetCohortDatabaseSchema = cohortDatabaseSchema,
    sourceCohortTable = cohortTableName1,
    targetCohortTable = cohortTableName2,
    purgeConflicts = TRUE,
    tempEmulationSchema = tempEmulationSchema
  )

  tempTable2Data <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT * FROM @cohort_database_schema.@cohort
            ORDER BY cohort_definition_id, subject_id, cohort_start_date, cohort_end_date;",
    cohort_database_schema = cohortDatabaseSchema,
    cohort = cohortTableName2,
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema
  ) |>
    dplyr::tibble()

  testthat::expect_equal(
    object = nrow(tempTable2Data),
    expected = nrow(cohort)
  )

  testthat::expect_identical(
    object = tempTable2Data,
    expected = cohort
  )

  testthat::expect_error(
    copyCohorts(
      connection = connection,
      oldToNewCohortId = dplyr::tibble(
        oldCohortId = c(1, 2),
        newCohortId = c(1, 2)
      ),
      sourceCohortDatabaseSchema = cohortDatabaseSchema,
      targetCohortDatabaseSchema = cohortDatabaseSchema,
      sourceCohortTable = cohortTableName1,
      targetCohortTable = cohortTableName2,
      purgeConflicts = FALSE,
      tempEmulationSchema = tempEmulationSchema
    )
  )

  copyCohorts(
    connection = connection,
    oldToNewCohortId = dplyr::tibble(
      oldCohortId = c(1, 2),
      newCohortId = c(4, 5)
    ),
    sourceCohortDatabaseSchema = cohortDatabaseSchema,
    targetCohortDatabaseSchema = cohortDatabaseSchema,
    sourceCohortTable = cohortTableName1,
    targetCohortTable = cohortTableName2,
    purgeConflicts = FALSE,
    tempEmulationSchema = tempEmulationSchema
  )

  copyCohorts(
    connection = connection,
    oldToNewCohortId = dplyr::tibble(
      oldCohortId = c(1, 2),
      newCohortId = c(4, 5)
    ),
    sourceCohortDatabaseSchema = cohortDatabaseSchema,
    targetCohortDatabaseSchema = cohortDatabaseSchema,
    sourceCohortTable = cohortTableName1,
    targetCohortTable = cohortTableName2,
    purgeConflicts = TRUE,
    tempEmulationSchema = tempEmulationSchema
  )

  testthat::expect_error(
    copyCohorts(
      connection = connection,
      oldToNewCohortId = dplyr::tibble(
        oldCohortId = c(1, 2),
        newCohortId = c(1, 2)
      ),
      sourceCohortDatabaseSchema = cohortDatabaseSchema,
      targetCohortDatabaseSchema = NULL,
      sourceCohortTable = cohortTableName1,
      targetCohortTable = cohortTableName1,
      isTempTable = TRUE,
      purgeConflicts = FALSE,
      tempEmulationSchema = tempEmulationSchema
    )
  )

  copyCohorts(
    connection = connection,
    oldToNewCohortId = dplyr::tibble(
      oldCohortId = c(1, 2),
      newCohortId = c(1, 2)
    ),
    sourceCohortDatabaseSchema = cohortDatabaseSchema,
    targetCohortDatabaseSchema = NULL,
    sourceCohortTable = cohortTableName1,
    targetCohortTable = tempCohortTableName1,
    isTempTable = TRUE,
    purgeConflicts = FALSE,
    tempEmulationSchema = tempEmulationSchema
  )

  DatabaseConnector::disconnect(connection = connection)

  copyCohorts(
    connectionDetails = connectionDetails,
    oldToNewCohortId = dplyr::tibble(
      oldCohortId = c(1, 2),
      newCohortId = c(6, 7)
    ),
    sourceCohortDatabaseSchema = cohortDatabaseSchema,
    targetCohortDatabaseSchema = cohortDatabaseSchema,
    sourceCohortTable = cohortTableName1,
    targetCohortTable = cohortTableName2,
    purgeConflicts = FALSE,
    tempEmulationSchema = tempEmulationSchema
  )

  testthat::expect_error(
    copyCohorts(
      connectionDetails = connectionDetails,
      oldToNewCohortId = dplyr::tibble(
        oldCohortId = c(1, 2),
        newCohortId = c(1, 1)
      ),
      sourceCohortDatabaseSchema = cohortDatabaseSchema,
      targetCohortDatabaseSchema = cohortDatabaseSchema,
      sourceCohortTable = cohortTableName1,
      targetCohortTable = cohortTableName2,
      purgeConflicts = FALSE,
      tempEmulationSchema = tempEmulationSchema
    )
  )

  testthat::expect_error(
    copyCohorts(
      connectionDetails = connectionDetails,
      oldToNewCohortId = dplyr::tibble(
        oldCohortId = c(1, 1),
        newCohortId = c(1, 1)
      ),
      sourceCohortDatabaseSchema = cohortDatabaseSchema,
      targetCohortDatabaseSchema = cohortDatabaseSchema,
      sourceCohortTable = cohortTableName1,
      targetCohortTable = cohortTableName2,
      purgeConflicts = FALSE,
      tempEmulationSchema = tempEmulationSchema
    )
  )

  testthat::expect_error(
    copyCohorts(
      connectionDetails = connectionDetails,
      oldToNewCohortId = dplyr::tibble(
        oldCohortId = c(1, 1),
        newCohortId = c(1, 2)
      ),
      sourceCohortDatabaseSchema = cohortDatabaseSchema,
      targetCohortDatabaseSchema = cohortDatabaseSchema,
      sourceCohortTable = cohortTableName1,
      targetCohortTable = cohortTableName2,
      purgeConflicts = FALSE,
      tempEmulationSchema = tempEmulationSchema
    )
  )

  testthat::expect_error(
    copyCohorts(
      connectionDetails = connectionDetails,
      oldToNewCohortId = dplyr::tibble(
        oldCohortId = c(1, 2),
        newCohortId = c(1, 2)
      ),
      sourceCohortDatabaseSchema = cohortDatabaseSchema,
      targetCohortDatabaseSchema = cohortDatabaseSchema,
      sourceCohortTable = cohortTableName1,
      targetCohortTable = cohortTableName1,
      purgeConflicts = FALSE,
      tempEmulationSchema = tempEmulationSchema
    )
  )

  sqlCleanUp <- "
  DROP TABLE IF EXISTS @cohort_database_schema.@table_name1;
  DROP TABLE IF EXISTS @cohort_database_schema.@table_name2;
  DROP TABLE IF EXISTS @temp_table_1;"

  DatabaseConnector::renderTranslateExecuteSql(
    connection = DatabaseConnector::connect(connectionDetails = connectionDetails),
    sql = sqlCleanUp,
    profile = FALSE,
    progressBar = FALSE,
    reportOverallTime = FALSE,
    tempEmulationSchema = tempEmulationSchema,
    table_name1 = cohortTableName1,
    table_name2 = cohortTableName2,
    temp_table_1 = tempCohortTableName1,
    cohort_database_schema = cohortDatabaseSchema,
  )
})
