library(testthat)
library(CohortExplorer)
library(dplyr)

dbms <- getOption("dbms", default = "postgresql")
message("************* Testing on ", dbms, " *************")

if (dir.exists(Sys.getenv("DATABASECONNECTOR_JAR_FOLDER"))) {
  jdbcDriverFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
} else {
  jdbcDriverFolder <- tempfile("jdbcDrivers")
  dir.create(jdbcDriverFolder, showWarnings = FALSE)
  DatabaseConnector::downloadJdbcDrivers(dbms, pathToDriver = jdbcDriverFolder)

  withr::defer(
    {
      unlink(jdbcDriverFolder, recursive = TRUE, force = TRUE)
    },
    testthat::teardown_env()
  )
}

folder <- tempfile()
dir.create(folder, recursive = TRUE)
skipCdmTests <- FALSE


if (dbms == "postgresql") {
  dbUser <- Sys.getenv("CDM5_POSTGRESQL_USER")
  dbPassword <- Sys.getenv("CDM5_POSTGRESQL_PASSWORD")
  dbServer <- Sys.getenv("CDM5_POSTGRESQL_SERVER")
  cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  vocabularyDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  tempEmulationSchema <- NULL
  cohortDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_OHDSI_SCHEMA")
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
  vocabularyDatabaseSchema <- Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA")
  tempEmulationSchema <- NULL
  cohortDatabaseSchema <- Sys.getenv("CDM5_REDSHIFT_OHDSI_SCHEMA")
} else if (dbms == "sql server") {
  dbUser <- Sys.getenv("CDM5_SQL_SERVER_USER")
  dbPassword <- Sys.getenv("CDM5_SQL_SERVER_PASSWORD")
  dbServer <- Sys.getenv("CDM5_SQL_SERVER_SERVER")
  cdmDatabaseSchema <- Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
  vocabularyDatabaseSchema <- Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
  tempEmulationSchema <- NULL
  cohortDatabaseSchema <- Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA")
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

cohortTable <- paste0(
  "ct_",
  paste(sample(letters, 10), collapse = "")
)

withr::defer(
  {
    if (!skipCdmTests) {
      # Clean up created cohort table:
      connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
      DatabaseConnector::renderTranslateExecuteSql(
        connection = connection,
        sql = "DROP TABLE IF EXISTS @cohort_database_schema.@cohort_table;",
        cohort_database_schema = cohortDatabaseSchema,
        cohort_table = cohortTable
      )
      DatabaseConnector::disconnect(connection)
    }
  },
  testthat::teardown_env()
)
