library(onc)
library(crayon)

# Global onc object
onc <- Onc(token = token, production = TRUE, showInfo = FALSE, outPath = "output", timeout = 60)


#' Prepare an onc object with outPath AND clean outPath files
#' @label internal
#'
#' @param outPath (character) Output path for the Onc object
#'
#' @return (Onc) object
prepareOnc <- function(outPath = "output") {
  onc <- Onc(token = token, production = TRUE, showInfo = FALSE, outPath = outPath, timeout = 60)
  cleanDirectory(outPath)
  return(onc)
}

#' Verifies if data is a named list with all expected fields present with their type
#' @label internal
#'
#' @param data     (named list) to verify
#' @param expected (named list) Verification rules. Keys are field names and their
#'                 value is the expected variable type. If the type string
#'                 ends with '*', the field is allowed to be NULL
#'
#' @return (logical) TRUE only if all expected fields are present with the expected type
#'         Otherwise, returns FALSE and prints a hint on what happened
verifyFields <- function(data, expected) {
  if (typeof(data) != "list") return(FALSE)   # Guard against wrong variable type
  
  dataFields     <- names(data)         # data field names
  expectedFields <- names(expected)     # expected field names
  
  for (expectedField in expectedFields) {
    expectedType <- expected[[expectedField]]
    
    # Detect if the type can be NULL
    canBeNull <- FALSE
    len <- nchar(expectedType)
    if (substring(expectedType, len) == "*") {
      expectedType <- substring(expectedType, 1, len - 1)
      canBeNull <- TRUE
    }
    #printf("\n%s %s\n", expectedField, expectedType);
    
    # Field must be there
    if (expectedField %in% dataFields) {
      value <- data[[expectedField]]
      dataFieldType <- typeof(value)
      
      # Field must be of the right type
      if (dataFieldType != expectedType) {
        if (!(canBeNull && is.null(value))) {
          cat(sprintf(red("\nERROR: ") %+% "Field '%s' of type %s was expected to be of type %s.\n", expectedField, dataFieldType, expectedType))
          return(FALSE)
        }
      }
    }
    else {
      cat(sprintf(red("\nERROR: ") %+% "Field '%s' was not found.\n", expectedField))
      return(FALSE)
    }
  }
  
  return(TRUE)
}

#' Checks if the response is an Oceans 2.0 error description
#' verifies that response has the minimal structure expected of an error
#' @label internal
#'
#' @param response Parsed httr response
#'
#' @return (logical) TRUE if response is a named list of 1 element, with the field errors
#'         which is a named list that includes fields "errorCode" and "errorMessage"
isErrorResponse <- function(response) {
  if ("errors" %in% names(response)) {
    names1 <- names(response$errors[[1]])
    return(("errorCode" %in% names1) && ("errorMessage" %in% names1))
  }
  
  return(FALSE)
}

#' Deletes a file
#' @label internal
#'
#' @param filePath (character) path to file
removeFile <- function(filePath) {
  if (file.exists(filePath)) {
    file.remove(filePath)
  }
}

#' Deletes all files inside directory in path
#' @label internal
#'
#' @param path path to directory, should not end with "/"
cleanDirectory <- function(path) {
  #
  # Args:
  #   path:
  fileList <- list.files(path, pattern = "*.*", full.names = TRUE)
  n <- length(fileList)
  for (i in 1:n) {
    unlink(fileList[i], recursive = TRUE)
  }
}

#' TEST - Verifies that the file in filePath exists
#' @label internal
#'
#' @param filePath file path
#'
#' @return (logical) TRUE if a file exists in the filePath
fileExists <- function(filePath) {
  return(file.exists(filePath))
}

#' TEST - Checks if the named list has a name with keyName
#' Optionally will also check if the item with keyName has the specific value
#' @label internal
#'
#' @param collection (named list) the list being checked
#' @param keyName    (character) The key name to look for
#' @param value      The value expected for the checked item; this check can be
#'                   skipped by not providing this argument
hasKey <- function(collection = list(), keyName = "", value = NA) {
  expect_named(collection)
  expect_true(keyName %in% names(collection))
  if (!is.na(value)) {
    expect_equal(collection[[keyName]], value)
  }
}

#' TEST - A check that passes when response doesn NOT have a next page
#' Used by real-time and archivefile suites
#' @label internal
#'
#' @param response Parsed response from hhttr
noNextPage <- function(response) {
  expect_length(response[["next"]], 0)
}


#' TEST - Checks that the path contains the amount of files
#' fails if the path doesn't contain countExpected files
#' @label internal
#'
#' @param path (character) Directory path
#' @param countExpected (numerical) Number of files expected at path
filesInPath = function(path = "output", countExpected = 0) {
  if (path == "") path = "."
  
  count <- length(list.files(path))
  expect_equal(count, countExpected)
}
