testthat::test_that("Execute Cohort Pathways", {
  testthat::skip_if(condition = skipCdmTests)

  # make up date for a cohort table
  targetCohort <- dplyr::tibble(
    cohortDefinitionId = c(1, 1, 2),
    subjectId = c(1, 1, 1),
    cohortStartDate = c(
      as.Date("1999-01-01"),
      as.Date("1999-02-20"),
      as.Date("1999-03-01")
    ),
    cohortEndDate = c(
      as.Date("1999-01-31"),
      as.Date("1999-02-28"),
      as.Date("1999-03-31")
    )
  )

  eventCohort <- dplyr::tibble(
    cohortDefinitionId = c(10, 10, 20),
    subjectId = c(1, 1, 1),
    cohortStartDate = c(
      as.Date("1999-01-01"),
      as.Date("1999-01-20"),
      as.Date("1999-04-10")
    ),
    cohortEndDate = c(
      as.Date("1999-01-10"),
      as.Date("1999-02-20"),
      as.Date("1999-04-20")
    )
  )

  # upload table
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = cohortDatabaseSchema,
    tableName = cohortTableName,
    data = dplyr::bind_rows(targetCohort, eventCohort) |> dplyr::distinct(),
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
    expected = nrow(
      dplyr::bind_rows(targetCohort, eventCohort) |> dplyr::distinct()
    )
  )

  output <- CohortPathways::executeCohortPathways(
    connection = connection,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableName = cohortTableName,
    targetCohortIds = c(1, 2),
    eventCohortIds = c(10, 20)
  )

  testthat::expect_true(object = ("pathwayAnalysisStatsData" %in% names(output)))
  testthat::expect_true(object = ("pathwaysAnalysisPathsData" %in% names(output)))
  testthat::expect_true(object = ("pathwaysAnalysisEventsData" %in% names(output)))
  testthat::expect_true(object = ("pathwaycomboIds" %in% names(output)))
  testthat::expect_true(object = ("isCombo" %in% names(output)))
  testthat::expect_true(object = ("pathwayAnalysisCodesData" %in% names(output)))

  DatabaseConnector::disconnect(connection = connection)

  output2 <- CohortPathways::executeCohortPathways(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableName = cohortTableName,
    targetCohortIds = c(1, 2),
    eventCohortIds = c(10, 20)
  )

  testthat::expect_true(object = ("pathwayAnalysisStatsData" %in% names(output2)))
  testthat::expect_true(object = ("pathwaysAnalysisPathsData" %in% names(output2)))
  testthat::expect_true(object = ("pathwaysAnalysisEventsData" %in% names(output2)))
  testthat::expect_true(object = ("pathwaycomboIds" %in% names(output2)))
  testthat::expect_true(object = ("isCombo" %in% names(output2)))
  testthat::expect_true(object = ("pathwayAnalysisCodesData" %in% names(output2)))
})
