#' Backup the current state of an areal database
#'
#' @details This function creates a tag that is composed of the version and the
#'   date, appends it to all stage3 files (tables and geometries), the inventory
#'   and the ontology/gazetteer files and stores them in the backup folder of
#'   the current areal database.
#' @return No return value, called for the side effect of saving the inventory,
#'   the stage3 files and modified ontology/gazetteer into the backup directory.
#' @importFrom readr read_lines
#' @importFrom stringr str_split
#' @importFrom checkmate testDirectoryExists
#' @importFrom purrr map
#' @export

adb_backup <- function(){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))

  load(paste0(intPaths, "/db_info.RData"))

  # derive current version
  version <- paste0(db_info$version, "_", format(Sys.Date(), "%Y%m%d"))

  # create backup directory
  if(!testDirectoryExists(x = paste0(intPaths, "/backup"))){
    dir.create(path = paste0(intPaths, "/backup"))
    dir.create(path = paste0(intPaths, "/backup/_meta"))
    dir.create(path = paste0(intPaths, "/backup/tables"))
    dir.create(path = paste0(intPaths, "/backup/geometries"))
  }

  # move inventory tables
  message(" -> backing up inventory")
  file.copy(from = paste0(intPaths, "/_meta/inventory.rds"),
            to = paste0(intPaths, "/backup/_meta/inventory_", version, ".rds"),
            overwrite = TRUE)

  # move tables/stage3
  stage3tables_full <- list.files(path = paste0(intPaths, "/tables/stage3"), full.names = TRUE)
  if(length(stage3tables_full) != 0){

    pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(stage3tables_full))

    message(" -> backing up tables")
    stage3tables <- map(.x = list.files(path = paste0(intPaths, "/tables/stage3")), .f = function(ix){
      pb$tick()
      temp <- str_split(ix, "[.]")[[1]]
      paste0(temp[1], "_", version, ".", temp[2])
    }) |> unlist()
    file.copy(from = stage3tables_full,
              to = paste0(intPaths, "/backup/tables/", stage3tables),
              overwrite = TRUE)

  }

  # move geometries/stage3
  stage3geometries_full <- list.files(path = paste0(intPaths, "/geometries/stage3"), full.names = TRUE)
  if(length(stage3geometries_full) != 0){

    pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(stage3geometries_full))

    message(" -> backing up geometries")
    stage3geometries <- map(.x = list.files(path = paste0(intPaths, "/geometries/stage3")), .f = function(ix){
      pb$tick()
      temp <- str_split(ix, "[.]")[[1]]
      paste0(temp[1], "_", version, ".", temp[2])
    }) |>
      unlist()
    file.copy(from = stage3geometries_full,
              to = paste0(intPaths, "/backup/geometries/", stage3geometries),
              overwrite = TRUE)

  }

  # move gazetteer
  message(" -> backing up gazetteer")
  file.copy(from = paste0(intPaths, "/_meta/lucki_gazetteer.rds"),
            to = paste0(intPaths, "/backup/_meta/lucki_gazetteer_", version, ".rds"),
            overwrite = TRUE)

  message(" -> backing up gazetteer matching tables")
  gazFolder <- str_split(tail(str_split(getOption(x = "gazetteer_path"), "\\/")[[1]], 1), "[.]")[[1]][1]
  if(!testDirectoryExists(x = paste0(intPaths, "/backup/_meta/", gazFolder))){
    dir.create(path = paste0(intPaths, "/backup/_meta/", gazFolder))
  }
  gaz_full <- list.files(path = paste0(intPaths, "/_meta/", gazFolder), full.names = TRUE)
  gaz <- map(.x = list.files(path = paste0(intPaths, "/_meta/", gazFolder)), .f = function(ix){
    temp <- str_split(ix, "[.]")[[1]]
    paste0(temp[1], "_", version, ".", temp[2])
  }) |>
    unlist()

  file.copy(from = gaz_full,
            to = paste0(intPaths, "/backup/_meta/", gazFolder, "/", gaz),
            overwrite = TRUE)

  # move ontology
  message(" -> backing up ontology")
  file.copy(from = paste0(intPaths, "/_meta/lucki_onto.rds"),
            to = paste0(intPaths, "/backup/_meta/lucki_onto_", version, ".rds"),
            overwrite = TRUE)

  message(" -> backing up ontology matching tables")
  ontoFolder <- str_split(tail(str_split(getOption(x = "ontology_path"), "\\/")[[1]], 1), "[.]")[[1]][1]
  if(!testDirectoryExists(x = paste0(intPaths, "/backup/_meta/", ontoFolder))){
    dir.create(path = paste0(intPaths, "/backup/_meta/", ontoFolder))
  }
  onto_full <- list.files(path = paste0(intPaths, "/_meta/", ontoFolder), full.names = TRUE)
  onto <- map(.x = list.files(path = paste0(intPaths, "/_meta/", ontoFolder)), .f = function(ix){
    temp <- str_split(ix, "[.]")[[1]]
    paste0(temp[1], "_", version, ".", temp[2])
  }) |>
    unlist()

  file.copy(from = onto_full,
            to = paste0(intPaths, "/backup/_meta/", ontoFolder, "/", onto),
            overwrite = TRUE)

}