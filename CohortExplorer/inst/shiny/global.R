library(magrittr)
source("R/private.R")

regexFilterInfo <- ""
if (file.exists("R/Default.R")) {
  source("R/Default.R")
}

files <- list.files(path = file.path("data"), pattern = ".rds", ignore.case = TRUE)
if (length(files) == 0) {
  stop("No .rds found in data folder.")
}

listOfFiles <-
  dplyr::tibble(files = files)

if (nrow(listOfFiles) == 0) {
  stop("No data found.")
}

listOfFiles$newName <-
  gsub(
    pattern = "CohortExplorer_",
    replacement = "",
    fixed = TRUE,
    x = listOfFiles$files
  )
listOfFiles$newName <-
  gsub(
    pattern = ".rds",
    replacement = "",
    fixed = TRUE,
    x = gsub(pattern = ".RDS", replacement = ".rds", x = listOfFiles$newName)
  )
listOfFiles <- listOfFiles %>%
  tidyr::separate(
    col = newName,
    sep = "_",
    into = c("cohortId", "databaseId")
  ) %>%
  dplyr::arrange(cohortId, databaseId)


initialSelectedCohortId <- listOfFiles[1, ]$cohortId
initialSelectedDatabaseId <- listOfFiles[1, ]$databaseId

tables <- c(
  "featureCohortData",
  "conditionEra",
  "conditionOccurrence",
  "drugEra",
  "drugExposure",
  "procedureOccurrence",
  "measurement",
  "observation",
  "visitOccurrence"
)
