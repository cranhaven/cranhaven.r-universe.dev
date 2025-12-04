#' nes_ls
#'
#' @param folder file.path to NES data; pass cache_path() to use OS agnostic cache location specified by the rappdirs package.
#' @param ... extra arguments passed to list.files
#' @inheritParams nes_get
#'
#' @export
#'
#' @examples
#' nes_ls("1")
nes_ls <- function(version_id, folder = temp_path(), ...){
  list.files(file.path(folder, version_id, "NES"),
             pattern = "\\.csv$", ...)
}

#'@name nes_ingest
#'@title Ingest flat files
#'@description Ingest data from component flat files
#'@param version_id character nes version string
#'@param folder file.path to data folder. optional.
#'@param skip numeric vector of lines to skip on file read. optional.
#'@importFrom readr read_csv
#'@importFrom purrr map_df
#'@examples \dontrun{
#'nes_ingest("1")
#'}
#'
nes_ingest <- function(version_id, folder = NA, skip = NA){

  # Set-up paths ####
  flist <- nes_ls(version_id = version_id, folder = folder,
                  full.names = TRUE, include.dirs = TRUE)

  # Read data ####

  if(all(is.na(skip))){
    skip <- rep(0, length(flist))
  }

  res <- lapply(seq_along(flist),
            function(i) purrr::map_df(flist[i],
                                   readr::read_csv,
                                   skip = skip[i]))
  names(res) <- gsub("*.csv", "", basename(flist))

  res
}
