## ----echo = FALSE, message = FALSE--------------------------------------------
knitr::opts_chunk$set(
  cache = FALSE,
  comment = "#>",
  error = FALSE,
  tidy = FALSE
)

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
# remotes::install_github("OHDSI/CohortAlgebra")

## ----tidy=FALSE,eval=TRUE, echo=FALSE-----------------------------------------
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

## ----tidy=FALSE,eval=TRUE, echo=TRUE------------------------------------------
cohort

## ----tidy=FALSE,eval=TRUE, echo=FALSE-----------------------------------------
cohortExpected <- dplyr::tibble(
  cohortDefinitionId = c(3, 3),
  subjectId = c(1, 1),
  cohortStartDate = c(as.Date("2022-01-01"), as.Date("2022-08-15")),
  cohortEndDate = c(as.Date("2022-05-10"), as.Date("2022-12-30"))
)

## ----tidy=FALSE, eval=TRUE, echo=TRUE-----------------------------------------
cohortExpected

## ----tidy=FALSE,eval=FALSE----------------------------------------------------
# oldToNewCohortId <-
#   dplyr::tibble(
#     oldCohortId = c(1, 2, 2),
#     newCohortId = c(3, 3, 3)
#   )
# 
# CohortAlgebra::unionCohorts(
#   connection = connection,
#   sourceCohortDatabaseSchema = cohortDatabaseSchema,
#   sourceCohortTable = tableName,
#   targetCohortDatabaseSchema = cohortDatabaseSchema,
#   targetCohortTable = tableName,
#   oldToNewCohortId = oldToNewCohortId
# )

## ----tidy=FALSE,eval=FALSE, echo=FALSE----------------------------------------
# data <-
#   DatabaseConnector::renderTranslateQuerySql(
#     connection = connection,
#     sql = paste0(
#       "SELECT * FROM @cohort_database_schema.@table_name
#         where cohort_definition_id = 3
#         order by cohort_definition_id, subject_id, cohort_start_date;"
#     ),
#     cohort_database_schema = cohortDatabaseSchema,
#     table_name = tableName,
#     snakeCaseToCamelCase = TRUE
#   ) |>
#   dplyr::tibble()

## ----tidy=FALSE,eval=FALSE, echo=TRUE-----------------------------------------
# data

## ----tidy=FALSE,eval=TRUE, echo=FALSE-----------------------------------------
cohortExpected

## ----tidy=FALSE,eval=TRUE, echo=FALSE-----------------------------------------
cohort <- dplyr::tibble(
  cohortDefinitionId = c(1, 2),
  subjectId = c(1, 1),
  cohortStartDate = c(
    as.Date("2022-01-01"),
    as.Date("2021-12-15")
  ),
  cohortEndDate = c(
    as.Date("2022-01-15"),
    as.Date("2022-01-30")
  )
)

## ----tidy=FALSE,eval=TRUE, echo=TRUE------------------------------------------
cohort

## ----tidy=FALSE,eval=FALSE, echo=TRUE-----------------------------------------
# CohortAlgebra::intersectCohorts(
#   connection = connection,
#   sourceCohortDatabaseSchema = cohortDatabaseSchema,
#   sourceCohortTable = tableName,
#   targetCohortDatabaseSchema = cohortDatabaseSchema,
#   targetCohortTable = tableName,
#   cohortIds = c(1, 2),
#   newCohortId = 3
# )

## ----tidy=FALSE,eval=FALSE, echo=FALSE----------------------------------------
# data <-
#   DatabaseConnector::renderTranslateQuerySql(
#     connection = connection,
#     sql = paste0(
#       "SELECT * FROM @cohort_database_schema.@table_name
#         where cohort_definition_id = 3
#         order by cohort_definition_id, subject_id, cohort_start_date;"
#     ),
#     cohort_database_schema = cohortDatabaseSchema,
#     table_name = tableName,
#     snakeCaseToCamelCase = TRUE
#   ) |>
#   dplyr::tibble()

## ----tidy=FALSE,eval=TRUE, echo=FALSE-----------------------------------------
cohort <- dplyr::tibble(
  cohortDefinitionId = c(1, 2, 2),
  subjectId = c(1, 1, 1),
  cohortStartDate = c(
    as.Date("2022-01-01"),
    as.Date("2021-12-15"),
    as.Date("2022-01-10")
  ),
  cohortEndDate = c(
    as.Date("2022-01-15"),
    as.Date("2022-01-05"),
    as.Date("2022-01-30")
  )
)

## ----tidy=FALSE,eval=TRUE, echo=TRUE------------------------------------------
cohort

## ----tidy=FALSE,eval=FALSE, echo=TRUE-----------------------------------------
# CohortAlgebra::intersectCohorts(
#   connection = connection,
#   sourceCohortDatabaseSchema = cohortDatabaseSchema,
#   sourceCohortTable = tableName,
#   targetCohortDatabaseSchema = cohortDatabaseSchema,
#   targetCohortTable = tableName,
#   cohortIds = c(1, 2),
#   newCohortId = 3
# )

## ----tidy=FALSE,eval=FALSE, echo=FALSE----------------------------------------
# data <-
#   DatabaseConnector::renderTranslateQuerySql(
#     connection = connection,
#     sql = paste0(
#       "SELECT * FROM @cohort_database_schema.@table_name
#         where cohort_definition_id = 3
#         order by cohort_definition_id, subject_id, cohort_start_date;"
#     ),
#     cohort_database_schema = cohortDatabaseSchema,
#     table_name = tableName,
#     snakeCaseToCamelCase = TRUE
#   ) |>
#   dplyr::tibble()

## ----tidy=FALSE,eval=TRUE, echo=FALSE-----------------------------------------
cohort <- dplyr::tibble(
  cohortDefinitionId = c(1, 2, 3),
  subjectId = c(1, 1, 1),
  cohortStartDate = c(
    as.Date("2022-01-01"),
    as.Date("2021-12-15"),
    as.Date("2022-03-01")
  ),
  cohortEndDate = c(
    as.Date("2022-01-15"),
    as.Date("2022-01-30"),
    as.Date("2022-03-15")
  )
)

## ----tidy=FALSE,eval=TRUE, echo=TRUE------------------------------------------
cohort

## ----tidy=FALSE,eval=FALSE, echo=FALSE----------------------------------------
# DatabaseConnector::insertTable(
#   connection = connection,
#   databaseSchema = cohortDatabaseSchema,
#   tableName = tableName,
#   data = cohort,
#   dropTableIfExists = TRUE,
#   createTable = TRUE,
#   tempTable = FALSE,
#   camelCaseToSnakeCase = TRUE,
#   progressBar = FALSE
# )

## ----tidy=FALSE,eval=FALSE, echo=TRUE-----------------------------------------
# CohortAlgebra::intersectCohorts(
#   connection = connection,
#   sourceCohortDatabaseSchema = cohortDatabaseSchema,
#   sourceCohortTable = tableName,
#   targetCohortDatabaseSchema = cohortDatabaseSchema,
#   targetCohortTable = tableName,
#   cohortIds = c(1, 2, 3),
#   newCohortId = 4
# )

## ----tidy=FALSE,eval=FALSE, echo=FALSE----------------------------------------
# data <-
#   DatabaseConnector::renderTranslateQuerySql(
#     connection = connection,
#     sql = paste0(
#       "SELECT * FROM @cohort_database_schema.@table_name
#         where cohort_definition_id = 4
#         order by cohort_definition_id, subject_id, cohort_start_date;"
#     ),
#     cohort_database_schema = cohortDatabaseSchema,
#     table_name = tableName,
#     snakeCaseToCamelCase = TRUE
#   ) |>
#   dplyr::tibble()

## ----tidy=FALSE,eval=FALSE, echo=FALSE----------------------------------------
# cohort <- dplyr::tibble(
#   cohortDefinitionId = c(1, 2),
#   subjectId = c(1, 1),
#   cohortStartDate = c(
#     as.Date("2022-01-01"),
#     as.Date("2021-12-15")
#   ),
#   cohortEndDate = c(
#     as.Date("2022-01-15"),
#     as.Date("2022-01-30")
#   )
# )

## ----tidy=FALSE,eval=TRUE, echo=TRUE------------------------------------------
cohort

## ----tidy=FALSE,eval=FALSE, echo=TRUE-----------------------------------------
# CohortAlgebra::intersectCohorts(
#   connection = connection,
#   sourceCohortDatabaseSchema = cohortDatabaseSchema,
#   sourceCohortTable = tableName,
#   targetCohortDatabaseSchema = cohortDatabaseSchema,
#   targetCohortTable = tableName,
#   cohortIds = c(1, 2, 3),
#   newCohortId = 4
# )

## ----tidy=FALSE,eval=FALSE, echo=FALSE----------------------------------------
# data <-
#   DatabaseConnector::renderTranslateQuerySql(
#     connection = connection,
#     sql = paste0(
#       "SELECT * FROM @cohort_database_schema.@table_name
#         where cohort_definition_id = 4
#         order by cohort_definition_id, subject_id, cohort_start_date;"
#     ),
#     cohort_database_schema = cohortDatabaseSchema,
#     table_name = tableName,
#     snakeCaseToCamelCase = TRUE
#   ) |>
#   dplyr::tibble()

## ----tidy=FALSE,eval=TRUE,echo=FALSE------------------------------------------
cohort <- dplyr::tibble(
  cohortDefinitionId = c(1, 2),
  subjectId = c(1, 1),
  cohortStartDate = c(
    as.Date("2022-01-01"),
    as.Date("2022-01-01")
  ),
  cohortEndDate = c(
    as.Date("2022-01-01"),
    as.Date("2022-01-02")
  )
)

## ----tidy=FALSE,eval=TRUE, echo=TRUE------------------------------------------
cohort

## ----tidy=FALSE,eval=FALSE, echo=FALSE----------------------------------------
# DatabaseConnector::insertTable(
#   connection = connection,
#   databaseSchema = cohortDatabaseSchema,
#   tableName = tableName,
#   data = cohort,
#   dropTableIfExists = TRUE,
#   createTable = TRUE,
#   tempTable = FALSE,
#   camelCaseToSnakeCase = TRUE,
#   progressBar = FALSE
# )

## ----tidy=FALSE,eval=FALSE, echo=FALSE----------------------------------------
# data <-
#   DatabaseConnector::renderTranslateQuerySql(
#     connection = connection,
#     sql = paste0(
#       "SELECT * FROM @cohort_database_schema.@table_name
#         where cohort_definition_id = 3
#         order by cohort_definition_id, subject_id, cohort_start_date;"
#     ),
#     cohort_database_schema = cohortDatabaseSchema,
#     table_name = tableName,
#     snakeCaseToCamelCase = TRUE
#   ) |>
#   dplyr::tibble()

## ----tidy=FALSE,eval=TRUE, echo=FALSE-----------------------------------------
cohort <- dplyr::tibble(
  cohortDefinitionId = c(1, 2),
  subjectId = c(1, 1),
  cohortStartDate = c(
    as.Date("2022-01-01"),
    as.Date("2022-02-10")
  ),
  cohortEndDate = c(
    as.Date("2022-03-01"),
    as.Date("2022-05-10")
  )
)

## ----tidy=FALSE,eval=TRUE, echo=TRUE------------------------------------------
cohort

## ----tidy=FALSE,eval=FALSE, echo=TRUE-----------------------------------------
# CohortAlgebra::minusCohorts(
#   connection = connection,
#   sourceCohortDatabaseSchema = cohortDatabaseSchema,
#   sourceCohortTable = tableName,
#   targetCohortDatabaseSchema = cohortDatabaseSchema,
#   targetCohortTable = tableName,
#   firstCohortId = 1,
#   secondCohortId = 2,
#   newCohortId = 3
# )

## ----tidy=FALSE,eval=FALSE, echo=FALSE----------------------------------------
# data <-
#   DatabaseConnector::renderTranslateQuerySql(
#     connection = connection,
#     sql = paste0(
#       "SELECT * FROM @cohort_database_schema.@table_name
#         where cohort_definition_id = 3
#         order by cohort_definition_id, subject_id, cohort_start_date;"
#     ),
#     cohort_database_schema = cohortDatabaseSchema,
#     table_name = tableName,
#     snakeCaseToCamelCase = TRUE
#   ) |>
#   dplyr::tibble()

## ----tidy=FALSE,eval=FALSE, echo=TRUE-----------------------------------------
# CohortAlgebra::minusCohorts(
#   connection = connection,
#   sourceCohortDatabaseSchema = cohortDatabaseSchema,
#   sourceCohortTable = tableName,
#   targetCohortDatabaseSchema = cohortDatabaseSchema,
#   targetCohortTable = tableName,
#   firstCohortId = 2,
#   secondCohortId = 1,
#   newCohortId = 4
# )

## ----tidy=FALSE,eval=FALSE, echo=FALSE----------------------------------------
# data <-
#   DatabaseConnector::renderTranslateQuerySql(
#     connection = connection,
#     sql = paste0(
#       "SELECT * FROM @cohort_database_schema.@table_name
#         where cohort_definition_id = 4
#         order by cohort_definition_id, subject_id, cohort_start_date;"
#     ),
#     cohort_database_schema = cohortDatabaseSchema,
#     table_name = tableName,
#     snakeCaseToCamelCase = TRUE
#   ) |>
#   dplyr::tibble()

