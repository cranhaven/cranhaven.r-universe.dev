test_that("invalid table names", {

  tableNames <- c("table_that_does_not_exist", "vocabulary", "visit_occurrence")
  invalidTableNames <- c("table_that_does_not_exist", "vocabulary")
  cdmVersion <- "5.3"
  outputFolder <- file.path(tempdir(), "testOutputExcel")
  dir.create(outputFolder)

  expect_error(generateTestTables(tableNames = tableNames,
                     cdmVersion = cdmVersion,
                     outputFolder = outputFolder
                     ), paste("The following filenames are invalid:", paste0(invalidTableNames, collapse = ", ")))

  unlink(outputFolder, recursive = TRUE)

})


test_that("invalid cdm version", {

  tableNames <- c("visit_occurrence")
  cdmVersion <- "10.1"
  outputFolder <- file.path(tempdir(), "testOutputExcel")
  dir.create(outputFolder)

  expect_error(generateTestTables(tableNames = tableNames,
                                               cdmVersion = cdmVersion,
                                               outputFolder = outputFolder
  ), "Invalid cdm version should be 5.3 or 5.4")

  unlink(outputFolder, recursive = TRUE)

})


test_that("output is generated with correct specifications, lower or uppercase naming should not matter", {

  tableNames <- c("visit_occurrence", "COST")
  cdmVersion <- "5.3"
  outputFolder <- file.path(tempdir(), "testOutputExcel")
  dir.create(outputFolder)

  generateTestTables(tableNames = tableNames,
                     cdmVersion = cdmVersion,
                     outputFolder = outputFolder
                     )

  excelOutputPath <- file.path(outputFolder, paste0("test_cdm_", cdmVersion, ".xlsx"))

  wb <- openxlsx::loadWorkbook(excelOutputPath)
  sheetNames <- openxlsx::getSheetNames(excelOutputPath)

  sheetData <- data.frame()
  sheetData$cost <- openxlsx::read.xlsx(wb, sheet = "cost")
  sheetData$visit_occurrence <- openxlsx::read.xlsx(wb, sheet = "visit_occurrence")

  expect_true(all(tolower(tableNames) %in% sheetNames))

  parquetCostFilePath <- system.file("cdmTableSpecifications", paste0("emptycdm_", cdmVersion), "cost.parquet", package = "TestGenerator")
  parquetCost <- arrow::read_parquet(parquetCostFilePath)
  visitOccurrenceFilePath <- system.file("cdmTableSpecifications", paste0("emptycdm_", cdmVersion), "visit_occurrence.parquet", package = "TestGenerator")
  parquetVisitOccurrence <- arrow::read_parquet(visitOccurrenceFilePath)

  expect_identical(colnames(parquetCost), colnames(sheetData$cost))
  expect_identical(colnames(parquetVisitOccurrence), colnames(sheetData$visit_occurrence))
  unlink(outputFolder, recursive = TRUE)
})



