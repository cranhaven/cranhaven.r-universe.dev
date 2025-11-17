testthat::test_that("Testing cohort delete", {
  testthat::skip_if(condition = skipCdmTests)
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

  dataInserted <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = paste0(
        "SELECT * FROM @cohort_database_schema.@table_name
        order by cohort_definition_id, subject_id, cohort_start_date;"
      ),
      cohort_database_schema = cohortDatabaseSchema,
      table_name = cohortTableName,
      snakeCaseToCamelCase = TRUE
    ) |>
    dplyr::tibble()
  testthat::expect_equal(
    object = dataInserted |>
      nrow(),
    expected = 12
  )
  CohortAlgebra:::deleteCohort(
    connection = connection,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTableName,
    cohortIds = 2
  )
  dataInsertedDeleteCohortId2 <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = paste0(
        "SELECT * FROM @cohort_database_schema.@table_name
        WHERE cohort_definition_id = 2
        order by cohort_definition_id, subject_id, cohort_start_date;"
      ),
      cohort_database_schema = cohortDatabaseSchema,
      table_name = cohortTableName,
      snakeCaseToCamelCase = TRUE
    ) |>
    dplyr::tibble()
  testthat::expect_equal(
    object = dataInsertedDeleteCohortId2 |>
      nrow(),
    expected = 0
  )
  DatabaseConnector::disconnect(connection = connection)


  # test with new connection
  CohortAlgebra:::deleteCohort(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTableName,
    cohortIds = 1
  )

  dataInsertedDeleteCohortId3 <-
    DatabaseConnector::renderTranslateQuerySql(
      connect = DatabaseConnector::connect(connectionDetails),
      sql = paste0(
        "SELECT * FROM @cohort_database_schema.@table_name
        where cohort_definition_id = 1
        order by cohort_definition_id, subject_id, cohort_start_date;"
      ),
      cohort_database_schema = cohortDatabaseSchema,
      table_name = cohortTableName,
      snakeCaseToCamelCase = TRUE
    ) |>
    dplyr::tibble()
  testthat::expect_equal(
    object = dataInsertedDeleteCohortId3 |>
      nrow(),
    expected = 0
  )

  DatabaseConnector::renderTranslateExecuteSql(
    connection = DatabaseConnector::connect(connectionDetails = connectionDetails),
    sql = "DROP TABLE IF EXISTS @cohort_database_schema.@table_temp;
           DROP TABLE IF EXISTS @cdm_database_schema.observation_period;",
    table_temp = cohortTableName,
    cohort_database_schema = cohortDatabaseSchema,
    cdm_database_schema = cohortDatabaseSchema
  )

  DatabaseConnector::renderTranslateExecuteSql(
    connection = DatabaseConnector::connect(connectionDetails = connectionDetails),
    sql = "DROP TABLE IF EXISTS @cohort_database_schema.@table_temp;
           DROP TABLE IF EXISTS @cdm_database_schema.observation_period;",
    table_temp = cohortTableName,
    cohort_database_schema = cohortDatabaseSchema,
    cdm_database_schema = cohortDatabaseSchema
  )
})
