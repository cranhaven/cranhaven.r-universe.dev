#' Create a Database of Processed PET Image Data
#'
#' @description
#' Processes multiple PET image files matching a specified filename pattern.
#' Each file is processed using \code{\link{neuroCleaner}}, and the results are aggregated
#' into a unified data frame for functional data analysis. This function serves as a key step
#' in the \code{neuroSCC} workflow, bridging raw image data and Simultaneous Confidence Corridors (SCC) computation.
#'
#' @param pattern \code{character}. A regular expression defining the file pattern to match.
#'        Subject identifiers are extracted from filenames based on this pattern.
#' @param control \code{logical}. If \code{TRUE}, files are treated as control group data;
#'        if \code{FALSE}, as pathological group data. Default is \code{TRUE}.
#' @param useSequentialNumbering \code{logical}. If \code{TRUE}, assigns sequential subject numbers
#'        instead of extracting them from filenames. Default is \code{FALSE}.
#' @param demo \code{data.frame}, optional. If provided, demographic information is included for each file.
#'        Default is \code{NULL}.
#' @param quiet \code{logical}. If \code{TRUE}, suppresses progress messages. Default is \code{FALSE}.
#'
#' @return A \code{data.frame} combining processed voxel-level data from all matched files.
#' Each row represents a voxel (3D pixel). The column structure depends on input
#' \itemize{
#'   \item For the control group: \code{CN_number}, \code{z}, \code{x}, \code{y}, \code{pet}
#'   \item For the pathological group: \code{AD_number}, \code{z}, \code{x}, \code{y}, \code{pet}
#'   \item If demographics are included: additional columns \code{PPT}, \code{Group}, \code{Sex}, \code{Age}
#' }
#'
#' @details
#' The function performs the following steps
#' \enumerate{
#'   \item Identifies image files matching the given pattern.
#'   \item Processes each file using \code{\link{neuroCleaner}}, optionally merging demographic data.
#'   \item Adds a subject identifier column (\code{CN_number} or \code{AD_number}).
#'   \item Aggregates all results into a single data frame.
#' }
#'
#' If no files are successfully processed, an empty data frame is returned with a warning.
#'
#' This function is typically followed by \code{\link{matrixCreator}}, which converts the output
#' into a matrix format for functional analysis.
#'
#' @examples
#' # NOTE: To keep runtime below CRAN limits, this example processes only 1 subject.
#' # You can expand the pattern to include all subjects for real use.
#'
#' # Example: Create a database from a single synthetic PET image (control group)
#' controlPattern <- "^syntheticControl1\\.nii\\.gz$"
#' databaseControls <- databaseCreator(pattern = controlPattern, control = TRUE, quiet = TRUE)
#' head(databaseControls)
#'
#' @seealso
#' \code{\link{neuroCleaner}} for the underlying image processing function. \cr
#' \code{\link{matrixCreator}} for the next step in the workflow that converts
#' the database to a matrix format for SCC analysis.
#'
#' @export
databaseCreator <- function(pattern, control = TRUE, useSequentialNumbering = FALSE, demo = NULL, quiet = FALSE) {
  # 1. Input validation
  # ---------------------------
  if (!is.character(pattern) || length(pattern) != 1) {
    stop("'pattern' must be a single character string specifying a regular expression")
  }

  if (!is.logical(control) || length(control) != 1) {
    stop("'control' must be a single logical value (TRUE or FALSE)")
  }

  if (!is.logical(useSequentialNumbering) || length(useSequentialNumbering) != 1) {
    stop("'useSequentialNumbering' must be a single logical value (TRUE or FALSE)")
  }

  if (!is.logical(quiet) || length(quiet) != 1) {
    stop("'quiet' must be a single logical value (TRUE or FALSE)")
  }

  if (!is.null(demo) && !is.data.frame(demo)) {
    stop("'demo' must be a data frame or NULL")
  }

  # 2. Get the list of files matching the pattern
  # ---------------------------
  searchPath <- getwd()
  packagePath <- system.file("extdata", package = "neuroSCC")

  if (length(list.files(packagePath, pattern = pattern)) > 0) {
    searchPath <- packagePath
  }

  files <- list.files(path = searchPath, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    stop("Error: No files with this pattern found in: ", searchPath)
  }

  # 3. Initialize storage list for data
  # ---------------------------
  dataList <- list()
  numberLabel <- if (control) "CN_number" else "AD_number"

  # 4. Process each file
  # ---------------------------
  for (i in seq_along(files)) {
    file <- files[i]
    baseName <- basename(file)

    # Extract the number directly from the file name
    number <- sub("^.*?(\\d+)\\.nii\\.gz$", "\\1", baseName)

    if (number == baseName) {
      warning("Could not extract number from: ", baseName, ". Using file index instead.")
      number <- as.character(i)
    }

    # Print progress message if not quiet
    if (!quiet) {
      message("Processing ", numberLabel, " ", number, " - File ", i, " of ", length(files))
    }

    # Process file using neuroCleaner
    tryCatch({
      if (is.null(demo)) {
        tempData <- neuroSCC::neuroCleaner(file)
      } else {
        fileBaseName <- sub("\\.nii\\.gz$", "", baseName)
        demoRow <- demo[demo$PPT == fileBaseName, , drop = FALSE]

        if (nrow(demoRow) == 0) {
          warning("No matching demographic data for ", fileBaseName, ". Using first row instead.")
          demoRow <- demo[1, , drop = FALSE]
        }

        tempData <- neuroSCC::neuroCleaner(file, demo = demoRow)
      }

      # Add subject number to each row
      tempData[[numberLabel]] <- number

      # Store processed data in list
      dataList[[i]] <- tempData

    }, error = function(e) {
      warning("Error processing file: ", baseName, " - ", e$message)
    })
  }

  # 5. Combine all processed data
  # ---------------------------
  if (length(dataList) == 0) {
    warning("No data was successfully processed. Returning empty data frame.")
    return(data.frame())
  }

  database <- do.call(rbind, dataList)

  # 6. Reorder columns
  # ---------------------------
  columnOrder <- c(numberLabel, "PPT", "Group", "Sex", "Age", "z", "x", "y", "pet")
  existingColumns <- intersect(columnOrder, names(database))
  database <- database[, existingColumns]

  return(database)
}
