#' nes_load
#' @description Load files from local file system
#' @param version_id character database version string
#' @param folder file.path to data folder; use cache_path() to load cached (non-temporary) data
#' @param format character choice of rds or sqlite
#' @param fpath file.path optionally specify custom location of rds file
#' @export
#' @importFrom rappdirs user_data_dir
#' @importFrom dplyr src_sqlite
#' @examples \dontrun{
#'
#' # load from tempdir
#' nes_get("5")
#' dt  <- nes_load("5")
#'
#' # load from cached
#' nes_get("5", dest_folder = cache_path())
#' dt  <- nes_load("5")
#' }
nes_load <- function(version_id, folder = tempdir(), format = "rds", fpath = NA){

  if(!is.na(fpath)){
        readRDS(fpath)
  }else{
      rds_path <- file.path(folder, paste0("NES_", version_id, ".rds"))
      cached_path <- file.path(cache_path(), paste0("NES_", version_id, ".rds"))

      if(file.exists(rds_path)){
        res <- readRDS(rds_path)
        res
      }else{
        # if(file.exists(cached_path)){
          res <- readRDS(cached_path)
          res
        # }else{
        #   stop_if_not_exists(cached_path)
        # }
      }
    }
}
