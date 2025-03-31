test_that("checkColumns function works", {
  filePath <- testthat::test_path("mimic_sample")
  cdmVersion <- "5.3"
  listPatientTables <- fileColumnCheck(filePath, cdmVersion)
  expect_equal(names(listPatientTables), c("condition_occurrence",
                                           "drug_exposure",
                                           "measurement",
                                           "observation_period",
                                           "person",
                                           "visit_detail",
                                           "visit_occurrence"))
})

test_that("Reading patients XLSX and JSON creation", {
  filePath <- testthat::test_path("testPatientsRSV.xlsx")
  # outputPath <- file.path(tempdir(), "test1")
  # dir.create(outputPath)

  # outputPath explicitly NULL to create the testCases in the testthat folder
  readPatients.xl(filePath = filePath, outputPath = NULL)
  expect_true(file.exists(file.path(testthat::test_path("testCases"), "test.json")))
})

test_that("Patients to CDM xlsx function", {
  filePath <- test_path("testPatientsRSV.xlsx")
  TestGenerator::readPatients.xl(filePath = filePath, outputPath = NULL)
  cdm <- TestGenerator::patientsCDM(pathJson = NULL, testName = "test")
  expect_equal(class(cdm), "cdm_reference")
  number_persons <- cdm[["person"]] %>% dplyr::pull(person_id)
  expect_equal(length(number_persons), 20)
  duckdb::duckdb_shutdown(duckdb::duckdb())
})

test_that("Read patients empty tables xl", {
  filePath <- test_path("test_cdm_data.xlsx")
  TestGenerator::readPatients.xl(filePath = filePath, outputPath = NULL)
  cdm <- TestGenerator::patientsCDM(pathJson = NULL, testName = "test")
  expect_equal(class(cdm), "cdm_reference")
  number_persons <- cdm[["person"]] %>% dplyr::pull(person_id)
  expect_equal(length(number_persons), 18)
  duckdb::duckdb_shutdown(duckdb::duckdb())
})

test_that("Read patients empty xl", {
  filePath <- test_path("test_cdm_data.xlsx")
  TestGenerator::readPatients.xl(filePath = filePath, outputPath = NULL)
  cdm <- TestGenerator::patientsCDM(pathJson = NULL, testName = "test")
  expect_equal(class(cdm), "cdm_reference")
  number_persons <- cdm[["person"]] %>% dplyr::pull(person_id)
  expect_equal(length(number_persons), 18)
  duckdb::duckdb_shutdown(duckdb::duckdb())
})

test_that("Reading sample MIMIC patients CSV files and JSON creation", {
  filePath <- testthat::test_path("mimic_sample")
  outputPath <- testthat::test_path("testCases")
  # outputPath <- file.path(tempdir(), "test1")
  # dir.create(outputPath)
  readPatients.csv(filePath = filePath, testName = "mimic_sample", outputPath = NULL)
  expect_true(file.exists(file.path(outputPath, "mimic_sample.json")))
  # unlink(outputPath, recursive = TRUE)
})

test_that("Reading MIMIC patients CSV files and JSON creation", {
  pathToData <- tempdir()
  pathToZipFile <- downloadTestData(datasetName = "mimicIV",
                                    cdmVersion = "5.3",
                                    pathToData = pathToData,
                                    overwrite = TRUE)
  unzip(pathToZipFile, exdir = pathToData)
  filePath <- file.path(pathToData,
                        "mimic-iv-demo-data-in-the-omop-common-data-model-0.9",
                        "1_omop_data_csv")
  outputPath <- file.path(tempdir(), "test1")
  dir.create(outputPath)
  testName <- "test"
  cdmVersion <- "5.3"
  readPatients.csv(filePath = filePath,
                   testName = testName,
                   outputPath = outputPath,
                   cdmVersion = cdmVersion)
  expect_true(file.exists(file.path(outputPath, "test.json")))
  unlink(outputPath, recursive = TRUE)
})

test_that("Mimic data Patients to CDM function", {
  pathToData <- tempdir()
  cdmVersion <- "5.3"
  pathToZipFile <- downloadTestData(datasetName = "mimicIV",
                                    cdmVersion = cdmVersion,
                                    pathToData = pathToData,
                                    overwrite = TRUE)
  unzip(pathToZipFile, exdir = pathToData)
  filePath <- file.path(pathToData,
                        "mimic-iv-demo-data-in-the-omop-common-data-model-0.9",
                        "1_omop_data_csv")
  outputPath <- file.path(tempdir(), "test1")
  dir.create(outputPath)
  testName <- "test"
  readPatients.csv(filePath = filePath,
                   testName = testName,
                   outputPath = outputPath,
                   cdmVersion = cdmVersion,
                   reduceLargeIds = TRUE)
  cdmName <- "myCDM"
  cdm <- TestGenerator::patientsCDM(pathJson = outputPath, testName = "test", cdmName = cdmName)
  expect_equal(class(cdm), "cdm_reference")
  number_persons <- cdm[["person"]] %>% dplyr::pull(person_id) %>% length()
  expect_equal(number_persons, 100)
  expect_equal(CDMConnector::cdmName(cdm), cdmName)
  unlink(outputPath, recursive = TRUE)
  duckdb::duckdb_shutdown(duckdb::duckdb())
})

test_that("convert ids function", {
  pathToData <- tempdir()
  cdmVersion <- "5.3"
  pathToZipFile <- downloadTestData(datasetName = "mimicIV",
                                    cdmVersion = cdmVersion,
                                    pathToData = pathToData,
                                    overwrite = TRUE)
  unzip(pathToZipFile, exdir = pathToData)
  filePath <- file.path(pathToData,
                        "mimic-iv-demo-data-in-the-omop-common-data-model-0.9",
                        "1_omop_data_csv")
  cdmTables <- fileColumnCheck(filePath, cdmVersion)
  cdmTables <- convertIds(cdmTables)
  measurement_ids <- cdmTables$measurement %>% pull(measurement_id)
  expect_equal(measurement_ids, seq(1, length(measurement_ids)))
  unlink(pathToData, recursive = TRUE)
  unlink(filePath, recursive = TRUE)
  duckdb::duckdb_shutdown(duckdb::duckdb())
})
