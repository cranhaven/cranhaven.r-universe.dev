csvDownloadButton <- function(ns,
                              outputTableId,
                              buttonText = "Download CSV (filtered)") {
  shiny::tagList(shiny::tags$br(),
                 shiny::tags$button(
                   buttonText,
                   onclick = paste0("Reactable.downloadDataCSV('", outputTableId, "')")
                 ))
}

readData <- function(databaseId,
                     cohortId) {
  if (file.exists(file.path(
    "data",
    paste0("CohortExplorer_", cohortId, "_", databaseId, ".rds")
  ))) {
    return(readRDS(file = file.path(
      "data",
      paste0("CohortExplorer_", cohortId, "_", databaseId, ".rds")
    )))
  } else if (file.exists(file.path(
    "data",
    paste0("CohortExplorer_", cohortId, "_", databaseId, ".RDS")
  ))) {
    return(readRDS(file = file.path(
      "data",
      paste0("CohortExplorer_", cohortId, "_", databaseId, ".RDS")
    )))
  } else {
    return(NULL)
  }
}


snakeCaseToCamelCase <- function(string) {
  string <- tolower(string)
  for (letter in letters) {
    string <- gsub(paste("_", letter, sep = ""), toupper(letter), string)
  }
  string <- gsub("_([0-9])", "\\1", string)
  return(string)
}

camelCaseToTitleCase <- function(string) {
  string <- gsub("([A-Z])", " \\1", string)
  string <- gsub("([a-z])([0-9])", "\\1 \\2", string)
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  return(string)
}

addDays <- function(x, n) {
  # Ensure that x is of class Date
  xAsDate <- as.Date(x)
  
  # Add n days to xAsDate
  newDate <- xAsDate + as.integer(n)
  
  return(newDate)
}
