library(testthat)
library(dplyr)

cohortTableName <- paste0(
  "ct_",
  paste(sample(letters, 10), collapse = "")
)

dbms <- getOption("dbms", default = "postgresql")

message("************* Testing on ", dbms, " *************")

if (dir.exists(Sys.getenv("DATABASECONNECTOR_JAR_FOLDER"))) {
  jdbcDriverFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
} else {
  jdbcDriverFolder <- file.path(tempfile(), "jdbcDrivers")
  dir.create(jdbcDriverFolder, showWarnings = FALSE)
  DatabaseConnector::downloadJdbcDrivers("postgresql", pathToDriver = jdbcDriverFolder)

  if (!dbms %in% c("postgresql", "sqlite")) {
    DatabaseConnector::downloadJdbcDrivers(dbms, pathToDriver = jdbcDriverFolder)
  }

  withr::defer(
    {
      unlink(jdbcDriverFolder, recursive = TRUE, force = TRUE)
    },
    testthat::teardown_env()
  )
}

folder <- tempfile()
dir.create(folder, recursive = TRUE)
minCellCountValue <- 5
skipCdmTests <- FALSE

if (dbms == "postgresql") {
  dbUser <- Sys.getenv("CDM5_POSTGRESQL_USER")
  dbPassword <- Sys.getenv("CDM5_POSTGRESQL_PASSWORD")
  dbServer <- Sys.getenv("CDM5_POSTGRESQL_SERVER")
  cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  vocabularyDatabaseSchema <-
    Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  tempEmulationSchema <- NULL
  cohortDatabaseSchema <-
    Sys.getenv("CDM5_POSTGRESQL_OHDSI_SCHEMA")
} else if (dbms == "oracle") {
  dbUser <- Sys.getenv("CDM5_ORACLE_USER")
  dbPassword <- Sys.getenv("CDM5_ORACLE_PASSWORD")
  dbServer <- Sys.getenv("CDM5_ORACLE_SERVER")
  cdmDatabaseSchema <- Sys.getenv("CDM5_ORACLE_CDM_SCHEMA")
  vocabularyDatabaseSchema <- Sys.getenv("CDM5_ORACLE_CDM_SCHEMA")
  tempEmulationSchema <- Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")
  cohortDatabaseSchema <- Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")
  options(sqlRenderTempEmulationSchema = tempEmulationSchema)
} else if (dbms == "redshift") {
  dbUser <- Sys.getenv("CDM5_REDSHIFT_USER")
  dbPassword <- Sys.getenv("CDM5_REDSHIFT_PASSWORD")
  dbServer <- Sys.getenv("CDM5_REDSHIFT_SERVER")
  cdmDatabaseSchema <- Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA")
  vocabularyDatabaseSchema <-
    Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA")
  tempEmulationSchema <- NULL
  cohortDatabaseSchema <- Sys.getenv("CDM5_REDSHIFT_OHDSI_SCHEMA")
} else if (dbms == "sql server") {
  dbUser <- Sys.getenv("CDM5_SQL_SERVER_USER")
  dbPassword <- Sys.getenv("CDM5_SQL_SERVER_PASSWORD")
  dbServer <- Sys.getenv("CDM5_SQL_SERVER_SERVER")
  cdmDatabaseSchema <- Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
  vocabularyDatabaseSchema <-
    Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
  tempEmulationSchema <- NULL
  cohortDatabaseSchema <-
    Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA")
}

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = dbms,
  user = dbUser,
  password = URLdecode(dbPassword),
  server = dbServer,
  pathToDriver = jdbcDriverFolder
)

if (cdmDatabaseSchema == "" || dbServer == "") {
  skipCdmTests <- TRUE
}

# Cleanup
sql <-
  "IF OBJECT_ID('@cohort_database_schema.@cohort_table', 'U') IS NOT NULL
              DROP TABLE @cohort_database_schema.@cohort_table;"

withr::defer(
  {
    if (!skipCdmTests) {
      connection <- DatabaseConnector::connect(connectionDetails)
      DatabaseConnector::renderTranslateExecuteSql(connection,
        sql,
        cohort_database_schema = cohortDatabaseSchema,
        cohort_table = cohortTableName
      )

      # Clean up created cohort table:
      connection <-
        DatabaseConnector::connect(connectionDetails = connectionDetails)
      DatabaseConnector::renderTranslateExecuteSql(
        connection = connection,
        sql = "DROP TABLE IF EXISTS @cohort_database_schema.@cohort_table;
                DROP TABLE IF EXISTS @cohort_database_schema.@cohort_table_1;
                DROP TABLE IF EXISTS @cohort_database_schema.@cohort_table_2;
                DROP TABLE IF EXISTS @cohort_database_schema.@cohort_table_3;
                DROP TABLE IF EXISTS @cohort_table;
                DROP TABLE IF EXISTS @cohort_table_1;
                DROP TABLE IF EXISTS @cohort_table_2;
                DROP TABLE IF EXISTS @cohort_table_3;
        ",
        cohort_database_schema = cohortDatabaseSchema,
        cohort_table = cohortTableName
      )

      DatabaseConnector::disconnect(connection)
    }
  },
  testthat::teardown_env()
)
