testthat::test_that("Testing append Cohort Tables ", {
  testthat::skip_if(condition = skipCdmTests)

  # make up date for a cohort table
  # this cohort table will have two subjects * two cohorts, within the same cohort
  cohort1 <- dplyr::tibble(
    cohortDefinitionId = c(1),
    subjectId = c(1),
    cohortStartDate = c(as.Date("1999-01-01")),
    cohortEndDate = c(as.Date("1999-01-31"))
  )
  cohort2 <- dplyr::tibble(
    cohortDefinitionId = c(1),
    subjectId = c(2),
    cohortStartDate = c(as.Date("1999-01-01")),
    cohortEndDate = c(as.Date("1999-01-31"))
  )
  cohortTableName1 <- paste0(cohortTableName, "_", 1)
  cohortTableName2 <- paste0(cohortTableName, "_", 2)
  # upload table
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = cohortDatabaseSchema,
    tableName = cohortTableName,
    data = cohort1 |>
      dplyr::mutate(cohortDefinition = 5),
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE,
    camelCaseToSnakeCase = TRUE,
    progressBar = FALSE
  )
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = cohortDatabaseSchema,
    tableName = cohortTableName1,
    data = cohort1,
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
    data = cohort2,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE,
    camelCaseToSnakeCase = TRUE,
    progressBar = FALSE
  )
  # disconnecting - as this is a test for a non temp cohort table
  DatabaseConnector::disconnect(connection)

  sourceTables <- dplyr::tibble(
    sourceCohortDatabaseSchema = c(cohortDatabaseSchema, cohortDatabaseSchema),
    sourceCohortTableName = c(cohortTableName1, cohortTableName2)
  )

  testthat::expect_error(
    CohortAlgebra:::appendCohortTables(
      connectionDetails = connectionDetails,
      sourceTables = sourceTables,
      targetCohortDatabaseSchema = cohortDatabaseSchema,
      targetCohortTable = cohortTableName,
      isTempTable = TRUE
    )
  )

  CohortAlgebra:::appendCohortTables(
    connectionDetails = connectionDetails,
    sourceTables = sourceTables,
    targetCohortDatabaseSchema = cohortDatabaseSchema,
    targetCohortTable = cohortTableName,
    isTempTable = FALSE
  )

  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = paste0(
      "
      DROP TABLE IF EXISTS @temp_table_name;
      DROP TABLE IF EXISTS @temp_table_name1;
      DROP TABLE IF EXISTS @temp_table_name2;
      DROP TABLE IF EXISTS @cohort_database_schema.@temp_table_name;
      DROP TABLE IF EXISTS @cohort_database_schema.@temp_table_name1;
      DROP TABLE IF EXISTS @cohort_database_schema.@temp_table_name2;"
    ),
    temp_table_name = cohortTableName,
    cohort_database_schema = cohortDatabaseSchema,
    profile = FALSE,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )

  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = "DROP TABLE IF EXISTS @cohort_database_schema.@table_temp;
           DROP TABLE IF EXISTS @cdm_database_schema.observation_period;",
    table_temp = cohortTableName,
    cohort_database_schema = cohortDatabaseSchema,
    cdm_database_schema = cohortDatabaseSchema,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  DatabaseConnector::disconnect(connection)
})
