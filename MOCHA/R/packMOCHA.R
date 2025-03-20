#' @title \code{packMOCHA}
#'
#' @description \code{packMOCHA} combines a MOCHA object (Sample-Tile
#'   Matrix or tileResults) with its saved coverage tracks into a single zip
#'   archive. This allows MOCHA objects and the necessary coverage files for
#'   plotting to be shared to other file systems. See also:
#'   \link[MOCHA]{unpackMOCHA}
#'
#' @param MOCHAObj A MultiAssayExperiment or RangedSummarizedExperiment, from
#'   MOCHA
#' @param zipfile Filename and path of the zip archive.
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#'
#' @return zipfile Path to zip archive.
#' 
#' @examples 
#' \dontrun{
#' # Depends on and manipulates files on filesystem
#' myOutputDir <- "/home/documents/MOCHA_out"
#' zipPath <- MOCHA::packMOCHA(
#'   tileResults, zipfile = file.path(myOutputDir, "testzip.zip")
#' )
#' }
#'
#' @export
#'
packMOCHA <- function(MOCHAObj,
                      zipfile,
                      verbose = FALSE) {
  if (!requireNamespace("zip", quietly = TRUE)) {
    stop(
      "Package 'zip' is required for packMOCHA. ",
      "Please install 'zip' to proceed."
    )
  }
  
  if (!methods::is(zipfile, "character") || !grepl("\\.zip$", zipfile)) {
    stop("`zipfile` must be a character string ending in .zip")
  }

  saveRDS(MOCHAObj, paste(MOCHAObj@metadata$Directory, "/MOCHA_Object.RDS", sep = ""))

  zip::zipr(zipfile, MOCHAObj@metadata$Directory)
  if (verbose) {
    message("Object & Coverage files saved: ", zipfile)
  }

  return(zipfile)
}


#' @title \code{unpackMOCHA}
#'
#' @description \code{unpackMOCHA} will unpack a zip archive created by
#'   \link[MOCHA]{unpackMOCHA}, setting the stored MOCHA object's stored
#'   directory path to the new location. See also: \link[MOCHA]{packMOCHA}
#'
#' @param zipfile Filepath to the packed MOCHA object.
#' @param exdir The path to the external directory where you want to unpack the MOCHA object.
#' @param verbose Display additional messages. Default is FALSE.
#'
#' @return MOCHAObj the MOCHA object (tileResults or Sample-Tile Matrix)
#'
#' @examples
#' \dontrun{
#' # Depends on files existing on your system
#' MOCHA::unpackMOCHA(zipfile = "./mochaobj.zip", exdir = "./newMOCHAdir")
#' }
#' @export
#'
unpackMOCHA <- function(zipfile,
                        exdir,
                        verbose = FALSE) {
  if (!requireNamespace("zip", quietly = TRUE)) {
    stop(
      "Package 'zip' is required for unpackMOCHA. ",
      "Please install 'zip' to proceed."
    )
  }
  if (!grepl("\\.zip$", zipfile)) {
    stop("`zipfile` must be a character string ending in .zip")
  }

  zip::unzip(zipfile = zipfile, exdir = exdir)

  # Extract directory name
  newDirectory <- sub("\\/$", "", zip::zip_list(zipfile)[1, 1], )

  # load MOCHA Object into memory
  MOCHAObj <- readRDS(paste(exdir, newDirectory, "MOCHA_Object.RDS", sep = "/"))

  # Change directory path for the MOCHA object to the new directory
  MOCHAObj@metadata$Directory <- paste(exdir, newDirectory, sep = "/")

  # Over-write object so that it has the correct directory paths saved.
  saveRDS(MOCHAObj, paste(exdir, newDirectory, "MOCHA_Object.RDS", sep = "/"))

  return(MOCHAObj)
}
