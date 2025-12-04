#'@name nes_compile
#'@title Compile data to R data (rds) object
#'@export
#'@description Compile data from component flat files
#'@param version_id character nes version string
#'@param format character choice of "rds" or "sqlite"
#'@param folder file.path to data folder; set to cache_path() to have data persist between sessions.
#'@param skip numeric vector of lines to skip on file read. optional.
#'@importFrom utils read.table
#'@importFrom rappdirs user_data_dir
#'@examples \dontrun{
#' nes_get("1")
#' nes_compile("1", format = "rds")
#'
#' nes_get("1", dest_folder = cache_path(), compile = FALSE)
#' nes_compile("1", folder = cache_path())
#' }
nes_compile <- function(version_id, format = "rds", folder = tempdir(), skip = NA){

  if(is.na(folder)){
    folder <- cache_path()
  }

  res <- nes_ingest(version_id = version_id, folder = folder, skip = skip)

  # dir.exists(cache_path())
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  outpath <- file.path(folder, paste0("NES_", version_id, ".rds"))

  saveRDS(res, outpath)
  message(paste0("NES compiled to ", folder, "/", "NES_", version_id, ".rds"))
}
