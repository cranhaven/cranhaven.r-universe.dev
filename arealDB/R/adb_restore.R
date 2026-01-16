#' Restore the database from a backup
#'
#' @param version [`character(1)][character]\cr a version tag for which to
#'   restore files.
#' @param date [`character(1)`][character]\cr a date for which to restore files.
#' @details This function searches for files that have the version and date tag,
#'   as it was defined in a previous run of \code{\link{adb_backup}}, to restore
#'   them to their original folders. This function overwrites by default, so use
#'   with care.
#' @return No return value, called for the side effect of restoring files that
#'   were previously stored in a backup.
#' @importFrom stringr str_split str_replace
#' @importFrom purrr map
#' @export

adb_restore <- function(version = NULL, date = NULL){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))

  # derive current version
  version <- paste0(version, "_", date)
  gazFolder <- str_split(tail(str_split(getOption(x = "gazetteer_path"), "\\/")[[1]], 1), "[.]")[[1]][1]
  ontoFolder <- str_split(tail(str_split(getOption(x = "ontology_path"), "\\/")[[1]], 1), "[.]")[[1]][1]

  # restore inventory tables/gazetteer/ontology
  inv_full <- list.files(path = paste0(intPaths, "/backup/_meta/"), pattern = version, full.names = TRUE)
  if(length(inv_full) == 0){
    message("no metadata found")
  }

  inv <- map(.x = inv_full, .f = function(ix){
    temp <- tail(str_split(string = ix, pattern = "/")[[1]], 1)
    str_replace(string = temp, pattern = paste0("_", version), replacement = "")
  }) %>% unlist()

  file.copy(from = inv_full,
            to = paste0(intPaths, "/_meta/", inv),
            overwrite = TRUE)

  # restore translation tables
  gaz_full <- list.files(path = paste0(intPaths, "/backup/_meta/", gazFolder), pattern = version, full.names = TRUE)
  if(length(gaz_full) == 0){
    message("no gazetteer found")
  }

  gaz <- map(.x = gaz_full, .f = function(ix){
    temp <- tail(str_split(string = ix, pattern = "/")[[1]], 1)
    str_replace(string = temp, pattern = paste0("_", version), replacement = "")
  }) %>% unlist()

  file.copy(from = gaz_full,
            to = paste0(intPaths, "/_meta/", gazFolder, "/", gaz),
            overwrite = TRUE)

  onto_full <- list.files(path = paste0(intPaths, "/backup/_meta/", ontoFolder), pattern = version, full.names = TRUE)
  if(length(onto_full) == 0){
    message("no ontology found")
  }

  onto <- map(.x = onto_full, .f = function(ix){
    temp <- tail(str_split(string = ix, pattern = "/")[[1]], 1)
    str_replace(string = temp, pattern = paste0("_", version), replacement = "")
  }) %>% unlist()

  file.copy(from = onto_full,
            to = paste0(intPaths, "/_meta/", ontoFolder, "/", onto),
            overwrite = TRUE)

  # restore tables/stage3
  stage3tables_full <- list.files(path = paste0(intPaths, "/backup/tables/"), pattern = version, full.names = TRUE)
  if(length(stage3tables_full) == 0){
    message("no tables found")
  }

  stage3tables <- map(.x = stage3tables_full, .f = function(ix){
    temp <- tail(str_split(string = ix, pattern = "/")[[1]], 1)
    str_replace(string = temp, pattern = paste0("_", version), replacement = "")
  }) %>% unlist()

  file.copy(from = stage3tables_full,
            to = paste0(intPaths, "/tables/stage3/", stage3tables),
            overwrite = TRUE)

  # restore geometries/stage3
  stage3geometries_full <- list.files(path = paste0(intPaths, "/backup/geometries/"), pattern = version, full.names = TRUE)
  if(length(stage3geometries_full) == 0){
    message("no geometries found")
  }

  stage3geometries <- map(.x = stage3geometries_full, .f = function(ix){
    temp <- tail(str_split(string = ix, pattern = "/")[[1]], 1)
    str_replace(string = temp, pattern = paste0("_", version), replacement = "")
  }) %>% unlist()

  file.copy(from = stage3geometries_full,
            to = paste0(intPaths, "/geometries/stage3/", stage3geometries),
            overwrite = TRUE)

}