testthat::test_that("Testing cohort intersect", {
  testthat::skip_if(condition = skipCdmTests)

  tempCohortTableName <- paste0("#", cohortTableName, "_1")

  # make up date for a cohort table
  # this cohort table will have two subjects * three cohorts. minus operations are only
  # performed on cohort 1 and 2. 3 is just an additional cohort that should have no impact.
  # For subject 1, cohort 2 - ends after cohort 1  - this is handled by sequentially running intersect before.
  # for subject 2, cohort 2 end before cohort 1
  cohort <- dplyr::tibble(
    cohortDefinitionId = c(1, 2, 2, 1, 2, 3),
    subjectId = c(1, 1, 1, 2, 2, 2),
    cohortStartDate = c(
      as.Date("1999-01-01"),
      as.Date("1999-01-20"),
      as.Date("1998-12-01"),
      as.Date("1999-01-01"),
      as.Date("1999-01-20"),
      as.Date("1999-01-15")
    ),
    cohortEndDate = c(
      as.Date("1999-01-31"),
      as.Date("1999-02-28"),
      as.Date("1999-01-01"),
      as.Date("1999-01-31"),
      as.Date("1999-01-31"),
      as.Date("1999-01-25")
    )
  ) |>
    dplyr::arrange(
      .data$subjectId,
      .data$cohortDefinitionId,
      .data$cohortStartDate
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
  CohortAlgebra::minusCohorts(
    connectionDetails = connectionDetails,
    sourceCohortDatabaseSchema = cohortDatabaseSchema,
    sourceCohortTable = cohortTableName,
    targetCohortDatabaseSchema = cohortDatabaseSchema,
    targetCohortTable = cohortTableName,
    firstCohortId = 1,
    secondCohortId = 2,
    newCohortId = 9,
    purgeConflicts = FALSE
  )

  # extract the generated output and compare to expected
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  dataPostMinus <-
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
    object = nrow(dataPostMinus),
    expected = 2
  ) # should collapse to 2 rows

  # create the expected output data frame object to compare
  cohortExpected <- dplyr::tibble(
    cohortDefinitionId = c(9, 9),
    subjectId = c(1, 2),
    cohortStartDate = c(as.Date("1999-01-02"), as.Date("1999-01-01")),
    cohortEndDate = c(as.Date("1999-01-19"), as.Date("1999-01-19"))
  )

  testthat::expect_true(object = all(dataPostMinus == cohortExpected))

  # this should throw error as there is already a cohort with cohort_definition_id = 9
  testthat::expect_error(
    CohortAlgebra::minusCohorts(
      connectionDetails = connectionDetails,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      firstCohortId = 1,
      secondCohortId = 2,
      newCohortId = 9,
      purgeConflicts = FALSE
    )
  )

  testthat::expect_error(
    CohortAlgebra::minusCohorts(
      connectionDetails = connectionDetails,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      firstCohortId = 1,
      secondCohortId = 2,
      newCohortId = 1,
      purgeConflicts = FALSE
    )
  )

  # this should give a message
  testthat::expect_message(
    CohortAlgebra::minusCohorts(
      connectionDetails = connectionDetails,
      sourceCohortDatabaseSchema = cohortDatabaseSchema,
      sourceCohortTable = cohortTableName,
      targetCohortDatabaseSchema = cohortDatabaseSchema,
      targetCohortTable = cohortTableName,
      firstCohortId = 1,
      secondCohortId = 2,
      newCohortId = 9,
      purgeConflicts = TRUE
    )
  )


  # should throw warning - check for NULL result
  testthat::expect_warning(
    CohortAlgebra::minusCohorts(
      connectionDetails = connectionDetails,
      sourceCohortDatabaseSchema = cohortDatabaseSchema,
      sourceCohortTable = cohortTableName,
      targetCohortDatabaseSchema = cohortDatabaseSchema,
      targetCohortTable = cohortTableName,
      firstCohortId = 1,
      secondCohortId = 1,
      newCohortId = 10,
      purgeConflicts = FALSE
    )
  )

  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  shouldNotHaveData <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT COUNT(*) COUNT
            FROM @cohort_database_schema.@cohort_table
            WHERE cohort_definition_id = 10;",
    snakeCaseToCamelCase = TRUE,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTableName
  )
  testthat::expect_equal(object = shouldNotHaveData$count, expected = 0)


  # check on temporary table
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

  CohortAlgebra::minusCohorts(
    connection = connection,
    sourceCohortTable = tempCohortTableName,
    targetCohortTable = tempCohortTableName,
    firstCohortId = 1,
    secondCohortId = 2,
    newCohortId = 9,
    purgeConflicts = FALSE
  )


  dataPostMinusTemp <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = paste0(
        "SELECT * FROM @table_name
        where cohort_definition_id = 9
        order by cohort_definition_id, subject_id, cohort_start_date;"
      ),
      table_name = tempCohortTableName,
      snakeCaseToCamelCase = TRUE
    ) |>
    dplyr::tibble()

  # this should throw error as there is already a cohort with cohort_definition_id = 9
  testthat::expect_error(
    CohortAlgebra::minusCohorts(
      connection = connection,
      cohortTable = tempCohortTableName,
      firstCohortId = 1,
      secondCohortId = 2,
      newCohortId = 9,
      purgeConflicts = FALSE
    )
  )

  CohortAlgebra::minusCohorts(
    connection = connection,
    sourceCohortTable = tempCohortTableName,
    targetCohortTable = tempCohortTableName,
    firstCohortId = 1,
    secondCohortId = 2,
    newCohortId = 9,
    purgeConflicts = TRUE
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
  testthat::expect_true(object = all(dataPostMinusTemp == cohortExpected))

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
