#' nes_get
#'
#' @export
#' @description Retrieves external files and store in file cache.
#' @importFrom rappdirs user_data_dir
#' @importFrom utils download.file unzip
#' @importFrom dataone CNode getMNode getPackage
#' @param version_id character version id
#' @param skip numeric vector of lines to skip on file read. optional.
#' @param dest_folder file.path optional will default to the location returned by \code{\link[rappdirs]{user_data_dir}}.
#' @param compile logical perform on-the-fly compilation to rds?
#' @examples \dontrun{
#' nes_get(version_id = "5") # save to temp folder
#'
#' nes_get(version_id = "5", dest_folder = cache_path()) # save to cache folder
#' }
nes_get <- function(version_id, dest_folder = tempdir(), skip = NA, compile = TRUE){

  # skip sanity check
  if(!is.na(skip)){
      tryCatch(if(skip %% 1 != 0){
          stop("'skip' must be an integer of class numeric.")
        },
    error = function(e) stop("'skip' must be an integer of class numeric."))
  }

  # dir.exists(cache_path())
  # dir.create(cache_path(), showWarnings = FALSE)
  versioned_path  <- file.path(dest_folder, version_id)
  versioned_cache <- file.path(cache_path(), version_id)
  dir.create(versioned_path, showWarnings = FALSE)
  targets <- list(temp_target  = file.path(versioned_path, "NES", "nes_data.csv"),
                  cache_target = file.path(versioned_cache, "NES", "nes_data.csv"))

  # Run below to find appropriate knb id
  # cn <- dataone::CNode("PROD")
  # mn <- dataone::getMNode(cn, "urn:node:KNB")
  # qy <- dataone::query(mn, list(
  #   rows = "300",
  #   q = "title:*\"National Eutrophication\"*",
  #   fl = "id,title,dateModified"),
  #   as = "data.frame")
  # head(dplyr::arrange(qy, desc(dateModified))[,c("id", "dateModified")])

  get_if_not_exists(id = "urn:uuid:4c15b04f-dced-41d0-8dc4-e4080a788fcf",
                    target = targets, versioned_path = versioned_path)

  if(compile){
    nes_compile(version_id = version_id, skip = skip, folder = dest_folder)
  }
}
