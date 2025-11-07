#' @title importSpectra
#'
#' @description In case you have your spectra in separated files (.csv) this
#'   function imports and binds them in a single data frame. The files directory
#'   must contain only the samples files.
#'
#' @param path A character vector with the full path to the data directory; by
#'   default corresponds to the working directory, \code{\link{getwd}}.
#' @param sep The field separator character; by default sep = ";".
#'
#' @return A data frame with the structure:
#' \itemize{
#'  \item \emph{First column (WN)}: wave numbers of the spectra.
#'  \item \emph{1-n}: samples spectra (the column names correspond to the files
#'  names).
#' }
#'
#' @seealso \code{\link{findPeaks}}, \code{\link{gOverview}} and
#'   \code{\link{plotPeaks}}
#'
#' @examples
#' # Create an empty directory
#' # Now create some spectra separate files
#' A <- andurinhaData[, 1:2]
#' B <- andurinhaData[, c(1, 3)]
#' C <- andurinhaData[, c(1, 4)]
#'
#' MASS::write.matrix(A, file = tempfile(pattern = "A.csv"), sep = ";")
#' MASS::write.matrix(A, file = tempfile(pattern = "B.csv"), sep = ";")
#' MASS::write.matrix(A, file = tempfile(pattern = "C.csv"), sep = ";")
#'
#' # Try
#' importSpectra(path = paste0(tempdir(), "/"), ";")
#'
#' @export

importSpectra <- function(path, sep = ";") {

  if (missing("path")) {
    path <- getwd()
  } else {
    path <- path
  }

  # Get the files names
  files <- list.files(path = path, pattern = ".csv")

  # Read *csv and cbind them (WN col excluded):
  absData <- as.data.frame(
    do.call(
      cbind,
      lapply(files, function(x) read.csv(paste(path, x, sep = ""),
                                         sep = sep,
                                         stringsAsFactors = FALSE)[, -1])
      ))

  # Sample names:
  sampleNames <- gsub(".csv", "", files, ignore.case = FALSE, fixed = FALSE)
  names(absData) <- sampleNames
  # WN:
  WN <- round(read.csv(paste(path, files[1], sep = ""), sep = sep)[, 1])
  # DATA (WN + abs):
  data <- data.frame(WN, absData)

  return(data)
}
