#' Reset an areal database to its unfilled state
#'
#' @param what [`logical(1)`][logical]\cr what to reset, either \code{"onto"},
#'   \code{"gaz"}, \code{"schemas"}, \code{"tables"}, \code{"geometries"} or
#'   \code{"all"}, the default.
#' @return no return value, called for its side effect of reorganising an areal
#'   database into a state where no reg* or norm* functions have been run
#' @importFrom checkmate assertLogical
#' @export

adb_reset <- function(what = "all"){

  assertChoice(x = what, choices = c("onto", "gaz", "schemas", "tables", "geometries", "inventory", "all"))
  if(what == "all"){
    what <- c("onto", "gaz", "schemas", "tables", "geometries", "inventory")
  }

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))

  if(length(intPaths) == 0){
    stop("no areal database active!")
  }

  # remove metadata
  if("inventory" %in% what) unlink(paste0(intPaths, "/_meta/inventory.rds"))
  if("gaz" %in% what) unlink(paste0(intPaths, "/_meta/lucki_gazetteer.rds"))
  if("onto" %in% what) unlink(paste0(intPaths, "/_meta/lucki_onto.rds"))
  if("schemas" %in% what) unlink(list.files(paste0(intPaths, "/_meta/schemas/"), full.names = TRUE))

  # move geometries from stage2/processed, to stage2
  if("geometries" %in% what){

    geom_stage2 <- list.files(path = paste0(intPaths, "/geometries/stage2/processed/"))
    geom_stage2_full <- list.files(path = paste0(intPaths, "/geometries/stage2/processed/"), full.names = TRUE)
    if(length(geom_stage2_full) != 0){
      file.copy(from = geom_stage2_full, paste0(intPaths, "/geometries/stage2/", geom_stage2))
      file.remove(geom_stage2_full)
    }

    # delete files from stage3
    geom_stage3_full <- list.files(path = paste0(intPaths, "/geometries/stage3"), full.names = TRUE)
    if(length(geom_stage3_full) != 0){
      file.remove(geom_stage3_full)
    }

  }

  # move tables from stage2/processed, to stage2
  if("tables" %in% what){

    tab_stage2 <- list.files(path = paste0(intPaths, "/tables/stage2/processed/"))
    tab_stage2_full <- list.files(path = paste0(intPaths, "/tables/stage2/processed/"), full.names = TRUE)
    if(length(tab_stage2_full) != 0){
      file.copy(from = tab_stage2_full, paste0(intPaths, "/tables/stage2/", tab_stage2))
      file.remove(tab_stage2_full)
    }

    # delete files from stage3
    tab_stage3_full <- list.files(path = paste0(intPaths, "/tables/stage3"), full.names = TRUE)
    if(length(tab_stage3_full) != 0){
      file.remove(tab_stage3_full)
    }

  }

}