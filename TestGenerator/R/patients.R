#' Converts a sample of patients into Unit Testing Definition JSON file.
#'
#' @param filePath Path to the test patient data in Excel format. The Excel has sheets that represent tables from the OMOP-CDM, e.g. person, drug_exposure, condition_ocurrence, etc.
#' @param testName A name of the test population in character.
#' @param outputPath Path of the output file, if NULL, a folder will be created in the project folder inst/testCases.
#' @param cdmVersion cdm version, default "5.3".
#'
#' @return A JSON file with sample patients inside the project directory.
#'
#' @importFrom readxl read_excel excel_sheets
#' @importFrom jsonlite toJSON
#' @importFrom checkmate assertDirectoryExists assertCharacter assertFileExists assert
#' @importFrom glue glue
#' @import cli
#'
#' @examples
#' filePath <- system.file("extdata", "testPatientsRSV.xlsx", package = "TestGenerator")
#' readPatients(filePath = filePath, outputPath = tempdir())
#'
#' @export
readPatients <- function(filePath = NULL,
                         testName = "test",
                         outputPath = NULL,
                         cdmVersion = "5.3") {
  checkmate::assertFileExists(filePath)
  fileExtension <- tools::file_ext(filePath)
  checkmate::assertTRUE(fileExtension %in% c("csv", "xlsx"))

  if (fileExtension == "csv") {
    readPatients.csv(filePath, testName, outputPath, cdmVersion)
  } else {
    readPatients.xl(filePath, testName, outputPath, cdmVersion)
  }
}

#' Converts a sample of patients in XLSX format into Unit Testing Definition JSON file.
#'
#' @param filePath Path to the test patient data in Excel format. The Excel has sheets that represent tables from the OMOP-CDM, e.g. person, drug_exposure, condition_ocurrence, etc.
#' @param testName A name of the test population in character.
#' @param outputPath Path to write the test JSON files. If NULL, the files will be written at the project's testthat folder, i.e. tests/testthat/testCases.
#' @param cdmVersion cdm version, default "5.3".
#'
#' @return A directory with the test JSON files with sample patients inside the project directory.
#'
#' @importFrom readxl read_excel excel_sheets
#' @importFrom jsonlite toJSON
#' @importFrom checkmate assertDirectoryExists assertCharacter assertFileExists assert
#' @importFrom testthat test_path
#' @importFrom glue glue
#' @import cli
#'
#' @examples
#' filePath <- system.file("extdata", "testPatientsRSV.xlsx", package = "TestGenerator")
#' readPatients.xl(filePath = filePath, outputPath = tempdir())
#'
#' @export
readPatients.xl <- function(filePath = NULL,
                            testName = "test",
                            outputPath = NULL,
                            cdmVersion = "5.3") {

  checkmate::assertCharacter(filePath)
  checkmate::assertFileExists(filePath)

  # Check columns
  expectedTables <- spec_cdm_field[[cdmVersion]] %>%
    dplyr::pull(cdmTableName)

  patientTables <- readxl::excel_sheets(filePath)
  checkmate::assert(all(patientTables %in% unique(expectedTables)))

  cdmTables <- lapply(patientTables,
                              readxl::read_excel,
                              path = filePath)

  names(cdmTables) <- tolower(patientTables)

  # Convert to JSON
  testCaseFile <- jsonlite::toJSON(cdmTables,
                                   dataframe = "rows",
                                   pretty = TRUE)

  # Create testPath folder
  testPath <- createOutputFolder(outputPath, testName)

  # Write file
  write(testCaseFile, file = testPath)
  if (checkmate::checkFileExists(testPath)) {
    cli::cli_alert_success(glue::glue("Unit Test Definition Created Successfully: '{testName}'"))
  } else {
    cli::cli_alert_danger("Unit Test Definition Creation Failed")
    stop()
  }
}

#' Converts a sample of patients in CSV format into a Unit Testing Definition JSON file.
#'
#' @param filePath Path to the test patient data in CSV format. Multiple CSV files representing tables tables from the OMOP-CDM must be provided, e.g. person.csv, drug_exposure.csv, condition_ocurrence.csv, etc.
#' @param testName Name for the test population file in character.
#' @param outputPath Path of the output file, if NULL, a folder will be created in the project folder inst/testCases.
#' @param cdmVersion cdm version, default "5.3".
#' @param reduceLargeIds Reduces the length of very long ids generally in int64 format, such as those found in the MIMIC-IV database.
#'
#' @return A JSON file with sample patients inside the project directory.
#'
#' @importFrom readr read_csv
#' @importFrom jsonlite toJSON
#' @importFrom checkmate assertDirectoryExists assertCharacter assertFileExists assert
#' @importFrom glue glue
#' @importFrom tools file_path_sans_ext
#' @import cli
#'
#' @examples
#' filePath <- system.file("extdata", "mimic_sample", package = "TestGenerator")
#' readPatients.csv(filePath = filePath, outputPath = tempdir())
#'
#' @export
readPatients.csv <- function(filePath = NULL,
                             testName = "test",
                             outputPath = NULL,
                             cdmVersion = "5.3",
                             reduceLargeIds = FALSE) {

  checkmate::assertDirectoryExists(filePath)
  checkmate::assertCharacter(cdmVersion)
  checkmate::assertTRUE(cdmVersion %in% c("5.3", "5.4"))

  # Check column
  cdmTables <- fileColumnCheck(filePath, cdmVersion)

  if (reduceLargeIds) {
    cdmTables <- convertIds(cdmTables)
  }

  # Convert to JSON
  testCaseFile <- jsonlite::toJSON(cdmTables,
                                   dataframe = "rows",
                                   pretty = TRUE)

  # Create testPath folder
  testPath <- createOutputFolder(outputPath, testName)

  # Write file
  write(testCaseFile, file = testPath)
  if (checkmate::checkFileExists(testPath)) {
    cli::cli_alert_success(glue::glue("Unit Test Definition Created Successfully: '{testName}'"))
  } else {
    cli::cli_alert_danger("Unit Test Definition Creation Failed")
    stop()
  }
}

fileColumnCheck <- function(filePath, cdmVersion) {
  checkmate::assertDirectoryExists(filePath)
  checkmate::assertCharacter(cdmVersion)
  checkmate::assertTRUE(cdmVersion %in% c("5.3", "5.4"))
  csvFiles <- list.files(filePath, pattern = ".csv", full.names = TRUE)
  csvFilesNames <- list.files(filePath, pattern = ".csv")
  checkmate::assertCharacter(csvFiles, any.missing = FALSE, min.len = 1)
  checkmate::assertCharacter(csvFilesNames, any.missing = FALSE, min.len = 1)
  currentTables <- spec_cdm_field[[cdmVersion]] %>%
    dplyr::pull(cdmTableName) %>%
    unique()
  patientTables <- list()
  report <- list()
  for (i in 1:length(csvFiles)) {
    tableName <- tools::file_path_sans_ext(csvFilesNames[i])
    if (tableName %in% currentTables) {
      cdmTable <- readr::read_csv(csvFiles[i], show_col_types = FALSE)
      if (nrow(cdmTable) != 0) {
        names(cdmTable) <- tolower(names(cdmTable))
        currentCoulumns <- names(cdmTable)
        expectedColumns <- spec_cdm_field[[cdmVersion]] %>%
          dplyr::filter(cdmTableName == tableName) %>%
          dplyr::pull(cdmFieldName)
        expectedColumns <- gsub("\"", "", expectedColumns)
        if (all(currentCoulumns %in% expectedColumns)) {
          patientTables[[tableName]] <- cdmTable
        } else {
          cli::cli_alert_danger(glue::glue("'{tableName}' table columns do not match"))
          stop()
        }
      } else {
        report[["empty"]] <- append(report[["empty"]], glue::glue("{tableName}"))
      }
    }
  }
  if (!is.null(report[['empty']])) {
    empty <- paste(report[['empty']], collapse = ", ")
    cli::cli_alert_warning("Empty Tables Found:")
    cli::cli_text(empty)
  }
  names(patientTables) <- tolower(names(patientTables))
  return(patientTables)
}

convertIds <- function(cdmTables) {
  report <- list()
  for (tables in names(cdmTables)) {
    # tables <- "vocabulary"
    for (columns in names(cdmTables[[tables]])) {
      # columns <- "vocabulary_concept_id"
      if (columns %in% c("person_id",
                         "care_site_id",
                         "condition_era_id",
                         "condition_occurrence_id",
                         "device_exposure_id",
                         "visit_occurrence_id",
                         "dose_era_id",
                         "drug_era_id",
                         "drug_exposure_id",
                         "fact_id_1",
                         "fact_id_2",
                         "measurement_id",
                         "observation_id",
                         "observation_period_id",
                         "procedure_occurrence_id",
                         "specimen_id",
                         "visit_detail_id",
                         "preceding_visit_detail_id",
                         "preceding_visit_occurrence_id",
                         "vocabulary_concept_id")) {

        uniqueIdValues <- unique(cdmTables[[tables]][[columns]])
        idValues <- abs(cdmTables[[tables]][[columns]]) %>%
          abs() %>%
          format(scientific = FALSE, trim = TRUE) %>%
          substr(1, 9) %>%
          as.numeric()

        if (length(unique(idValues)) != length(unique(uniqueIdValues))) {
          if(!tables %in% c("person_id", "visit_occurrence_id", "condition_occurrence_id")) {
            cdmTables[[tables]][[columns]] <- seq(1, length(uniqueIdValues))
            report[["notUnique"]] <- append(report[["notUnique"]], glue::glue("{tables}"))
            # message(glue::glue("'{tables}' table with '{columns}' ids are not unique"))
            # message(glue::glue("'{tables}' table filled out with sequence of numbers"))
          } else {
            cli::cli_alert_danger(glue::glue("'{tables}' table with '{columns}' ids are not unique and couldn't fill with num sequence"))
            stop()
          }
        } else {
          cdmTables[[tables]][[columns]] <- idValues
          # report[["reduced"]] <- append(report[["reduced"]], glue::glue("{tables}"))
          # message(glue::glue("'{tables}' table and '{columns}' ids reduced succesfully"))
        }
      }
    }
  }
  if (!is.null(report[['notUnique']])) {
    notUnique <- paste(report[['notUnique']], collapse = ", ")
    cli::cli_alert_warning("Table with non unique ids and filled with num seq:")
    cli::cli_text(notUnique)
  }
  cli::cli_alert_success("IDs successfully reduced")
  # reduced <- paste(report[['reduced']], collapse = ", ")
  # cli::cli_text(reduced)
  return(cdmTables)
}

createOutputFolder <- function(outputPath, testName) {
  if (is.null(outputPath)) {
    testFolder <- testthat::test_path("testCases")
    if (!dir.exists(testFolder)) {
      dir.create(testFolder)
      testPath <- paste0(testFolder, "/", testName, ".json")
    } else {
      testPath <- paste0(testFolder, "/", testName, ".json")
    }
  } else {
    checkmate::assertCharacter(outputPath)
    checkmate::assertDirectoryExists(outputPath)
    testPath <- paste0(outputPath, "/", testName, ".json")
  }
  return(testPath)
}

#' Pushes test population into a blank CDM.
#'
#' @param pathJson Directory where the sample populations in json are located. If NULL, gets the default inst/testCases directory.
#' @param testName Name of the sample population JSON file. If NULL it will push the first sample population in the testCases directory.
#' @param cdmVersion cdm version, default "5.3".
#' @param cdmName Name of the cdm, default NULL.
#'
#' @return A CDM reference object with a sample population.
#' @import dplyr cli
#' @importFrom DBI dbConnect dbAppendTable dbDisconnect
#' @importFrom duckdb duckdb
#' @importFrom jsonlite fromJSON
#' @importFrom CDMConnector downloadEunomiaData example_datasets eunomia_dir cdmFromCon
#'
#' @examples
#' \donttest{
#' filePath <- system.file("extdata", "testPatientsRSV.xlsx", package = "TestGenerator")
#' TestGenerator::readPatients(filePath = filePath, outputPath = tempdir())
#' cdm <- TestGenerator::patientsCDM(pathJson = tempdir(), testName = "test")
#' duckdb::duckdb_shutdown(duckdb::duckdb())
#' }
#' @export
patientsCDM <- function(pathJson = NULL,
                        testName = NULL,
                        cdmVersion = "5.3",
                        cdmName = NULL) {

  if (is.null(pathJson)) {
    outputFolder <- testthat::test_path("testCases")
    if (dir.exists(outputFolder)) {
      pathJson <- outputFolder
    } else {
      cli::cli_alert_danger("testCases not found")
      stop()
    }
  }

  checkmate::assertClass(pathJson, "character")
  checkmate::assertDirectoryExists(pathJson)

  if (identical(list.files(pathJson), character(0))) {
    cli::cli_alert_danger("Directory empty. Provide Unit Test Definitions")
    stop()
  }

  testFiles <- list.files(pathJson, pattern = ".json")

  if (is.null(testName)) {
    testName <- testFiles[1]
  } else {
    checkmate::checkClass(testName, "character")
    testName <- paste0(testName, ".json")
  }

  fileName <- file.path(pathJson, testName)
  checkmate::assertFileExists(fileName)

  # Folder to download empty CDM

  if (!dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))) {
    Sys.setenv(EUNOMIA_DATA_FOLDER = tempdir())
  }

  # Check/Download vocabulary

  vocabPath <- file.path(Sys.getenv("EUNOMIA_DATA_FOLDER"),
                         glue::glue("empty_cdm_{cdmVersion}.zip"))

  if (!file.exists(vocabPath)) {
    CDMConnector::downloadEunomiaData(datasetName = "empty_cdm",
                                      cdmVersion = cdmVersion,
                                      pathToData = Sys.getenv("EUNOMIA_DATA_FOLDER"),
                                      overwrite = TRUE)
  }

  conn <- DBI::dbConnect(duckdb::duckdb(CDMConnector::eunomia_dir("empty_cdm")))
  cdm <- CDMConnector::cdmFromCon(con = conn,
                                  cdmSchema = "main",
                                  writeSchema = "main",
                                  cdmName = cdmName)

  # Read the JSON file into R
  jsonData <- jsonlite::fromJSON(fileName)
  # Check for the expected columns in the CDM
  for (tableName in names(jsonData)) {
    # tableName <- "person"
    classTable <- class(jsonData[[tableName]])
    if (classTable == "data.frame") {
      currentCoulumns <- names(jsonData[[tableName]])
      expectedColumns <- spec_cdm_field[[cdmVersion]] %>%
        dplyr::filter(cdmTableName == tableName) %>%
        dplyr::pull(cdmFieldName)
      jsonData[[tableName]] <- jsonData[[tableName]] %>%
        select(currentCoulumns[currentCoulumns %in% expectedColumns])
    }
  }

  # Convert the JSON data into a data frame and append it to the blank CDM

  for (tableName in names(jsonData)) {
    # tableName <- "vocabulary"
    patientData <- as.data.frame(jsonData[[tableName]])
    DBI::dbAppendTable(conn, tableName, patientData)
  }
  cli::cli_alert_success("Patients pushed to blank CDM successfully")
  return(cdm)
}

getEmptyCDM <- function(cdmName, cdmVersion) {

  vocabPath <- file.path(Sys.getenv("EUNOMIA_DATA_FOLDER"),
                         glue::glue("empty_cdm_{cdmVersion}.zip"))

  if (!file.exists(vocabPath)) {
    CDMConnector::downloadEunomiaData(datasetName = "empty_cdm",
                                      cdmVersion = cdmVersion,
                                      pathToData = Sys.getenv("EUNOMIA_DATA_FOLDER"),
                                      overwrite = TRUE)
  }

  conn <- DBI::dbConnect(duckdb::duckdb(CDMConnector::eunomia_dir("empty_cdm")))
  cdm <- CDMConnector::cdmFromCon(con = conn,
                                  cdmSchema = "main",
                                  writeSchema = "main",
                                  cdmName = cdmName)

  return(cdm)

}
