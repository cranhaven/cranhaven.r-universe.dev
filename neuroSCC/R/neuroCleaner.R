#' Clean and Load Data from NIfTI Neuroimaging Files
#'
#' @description
#' Loads a NIfTI-format neuroimaging file and transforms it into a structured data frame,
#' organizing voxel-level information for downstream analysis. This function is the first step
#' in the neuroimaging processing pipeline in \code{neuroSCC}, converting raw PET data into
#' a format suitable for functional data analysis. SCCs are later computed using functions
#' from the \code{ImageSCC} package, such as \code{ImageSCC::scc.image()}.
#'
#' @param name \code{character}. The full path to the NIfTI file to process.
#' @param demo Optional \code{data.frame} containing demographic information.
#'        If provided, it should include columns (case-insensitive): \code{PPT}, \code{Group}, \code{Sex}, and \code{Age}.
#'        If automatic matching via the \code{PPT} column fails, the row specified by \code{demoRow} is used. Default is \code{NULL}.
#' @param demoRow \code{integer}. Row to use from the demographic table if automatic matching fails. Default is \code{1}.
#'
#' @return A data frame where each row represents a voxel (3D pixel).
#' \itemize{
#'   \item If demographics are provided: the columns include \code{PPT}, \code{Group}, \code{Sex}, \code{Age}, \code{z}, \code{x}, \code{y}, and \code{pet}.
#'   \item If demographics are not provided: the columns include \code{z}, \code{x}, \code{y}, and \code{pet}.
#' }
#' The \code{pet} column contains the PET intensity value at each voxel location.
#'
#' @details
#' The function performs the following steps
#' \enumerate{
#'   \item Loads the NIfTI file using \code{oro.nifti::readNIfTI()}.
#'   \item Converts the 3D image into a tidy data frame.
#'   \item Adds \code{z}, \code{x}, and \code{y} voxel coordinates.
#'   \item If demographic data is provided, attempts to match based on \code{PPT} (case-insensitive). If no match is found, \code{demoRow} is used.
#' }
#'
#' The resulting data frame serves as input for \code{\link{databaseCreator}}, \code{\link{matrixCreator}},
#' and other core functions in the \code{neuroSCC} pipeline.
#'
#' @examples
#' # Load a sample Control NIfTI file
#' niftiFile <- system.file("extdata", "syntheticControl1.nii.gz", package = "neuroSCC")
#'
#' # Example Without demographic data
#' petData <- neuroCleaner(niftiFile)
#' petData[sample(nrow(petData), 10), ]  # Show 10 random voxels
#'
#' @seealso
#' \code{\link{databaseCreator}} for batch image processing. \cr
#' \code{\link[oro.nifti]{readNIfTI}} for reading NIfTI-format files.
#'
#' @export

neuroCleaner <- function(name, demo = NULL, demoRow = 1) {
  # 1. Input validation
  # ---------------------------
  # Check if name parameter is valid
  if (!is.character(name) || length(name) != 1) {
    stop("'name' must be a single character string specifying the file path")
  }

  # Check if the file exists
  if (!file.exists(name)) {
    stop("File not found: ", name)
  }

  # Check if demographic data is properly formatted (when provided)
  if (!is.null(demo)) {
    if (!is.data.frame(demo)) {
      stop("'demo' must be a data frame")
    }

    # Check for demographic columns case-insensitively
    demoCols <- tolower(names(demo))
    requiredCols <- c("ppt", "group", "sex", "age")

    if (!("ppt" %in% demoCols)) {
      warning("Demographic data doesn't contain 'PPT' column. Will use row ", demoRow, " instead.")
    }

    if (demoRow > nrow(demo)) {
      stop("'demoRow' (", demoRow, ") exceeds the number of rows in the demographic data (", nrow(demo), ")")
    }
  }

  # 2. Read NIFTI file
  # ---------------------------
  file <- tryCatch({
    oro.nifti::readNIfTI(
      fname = name,
      verbose = FALSE,
      warn = -1,
      reorient = TRUE,
      call = NULL,
      read_data = TRUE
    )
  }, error = function(e) {
    stop("Error reading NIFTI file: ", e$message)
  })

  # 3. Convert image data to data frame
  # ---------------------------
  n <- tryCatch({
    memisc::to.data.frame(oro.nifti::img_data(file))
  }, error = function(e) {
    stop("Error converting NIfTI data to data frame: ", e$message)
  })

  # 4. Extract dimensional information
  # ---------------------------
  dimensions <- neuroSCC::getDimensions(file)

  # Store file name without extension for demographic matching
  fileBaseName <- sub("\\.nii\\.gz$", "", basename(name))

  # Prepare empty data frame for storing the processed data
  dataframe <- data.frame(z = integer(), x = integer(), y = integer(), pet = numeric())

  # 5. Process each slice and organize data
  # ---------------------------
  for (i in seq_len(dimensions$xDim)) {
    nLim <- n[n$Var2 == i, ]
    nLim$Var1 <- NULL
    nLim$Var2 <- NULL

    z <- rep(i, length.out = dimensions$dim)
    x <- rep(1:dimensions$xDim, each = dimensions$yDim, length.out = dimensions$dim)
    y <- rep(1:dimensions$yDim, length.out = dimensions$dim)
    pet <- unlist(nLim)

    temp <- data.frame(z, x, y, pet)
    dataframe <- rbind(dataframe, temp)
  }

  # 6. Merge with demographic data (if provided)
  # ---------------------------
  if (!is.null(demo)) {
    # Helper function to retrieve column names case-insensitively
    getColumn <- function(colName) {
      idx <- which(tolower(names(demo)) == tolower(colName))
      if (length(idx) > 0) return(names(demo)[idx[1]])
      return(NULL)
    }

    pptColumn <- getColumn("ppt")

    # Ensure 'PPT' column is formatted correctly
    if (!is.null(pptColumn)) {
      demo[[pptColumn]] <- sub("\\.nii\\.gz$", "", demo[[pptColumn]])
    }

    # Try to match demographic data by PPT
    demographicData <- NULL
    if (!is.null(pptColumn)) {
      demographicData <- demo[demo[[pptColumn]] == fileBaseName, , drop = FALSE]
    }

    # If no match, fallback to demoRow selection
    if (is.null(demographicData) || nrow(demographicData) == 0) {
      if (!is.null(pptColumn)) {
        warning("No demographic data found for '", fileBaseName, "'. Using row ", demoRow, " instead.")
      }
      demographicData <- demo[demoRow, , drop = FALSE]
    }

    # Identify demographic columns
    groupColumn <- getColumn("group")
    sexColumn <- getColumn("sex")
    ageColumn <- getColumn("age")

    # Merge demographic data only if available
    if (nrow(demographicData) > 0) {
      demoData <- data.frame(
        PPT = rep(demographicData[[pptColumn]], each = nrow(dataframe)),
        Group = rep(demographicData[[groupColumn]], each = nrow(dataframe)),
        Sex = rep(demographicData[[sexColumn]], each = nrow(dataframe)),
        Age = rep(demographicData[[ageColumn]], each = nrow(dataframe))
      )
      dataframe <- cbind(demoData, dataframe)
    }
  }

  # 7. Reorder columns dynamically
  # ---------------------------
  columnOrder <- c("PPT", "Group", "Sex", "Age", "z", "x", "y", "pet")
  existingColumns <- intersect(columnOrder, names(dataframe))
  dataframe <- dataframe[, existingColumns]

  # 8. Return the processed data
  # ---------------------------
  return(dataframe)
}
