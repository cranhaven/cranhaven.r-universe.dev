#' Archive the data from an areal database
#'
#' @param pattern [`character(1)`][character]\cr a regular expression used to
#'   filter files to load.
#' @param variables [`character(.)`][character]\cr columns, typically observed
#'   variables, to select.
#' @param compress [`logical(1)`][logical]\cr whether or not the database should
#'   be compressed into a \emph{tar.gz} archive. Will delete the database folder
#'   in \code{outPath}.
#' @param outPath [`character(1)`][character]\cr directory, where the archive
#'   should be stored.
#' @details This function prepares and packages the data into an archiveable
#'   form. This contains geopacakge files for geometries and csv files for all
#'   tables, such as inventory, matching and thematic data tables.
#' @return no return value, called for the side-effect of creating a database
#'   archive.
#' @importFrom checkmate assertCharacter assertLogical assertDirectoryExists
#'   testDirectoryExists
#' @importFrom purrr map
#' @importFrom stringr str_split
#' @importFrom readr write_csv
#' @importFrom archive archive_write_dir
#' @importFrom utils capture.output sessionInfo tar
#' @export

adb_archive <- function(pattern = NULL, variables = NULL, compress = FALSE,
                        outPath = NULL){

  assertCharacter(x = pattern, len = 1, null.ok = TRUE)
  assertCharacter(x = variables, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = compress, len = 1, any.missing = FALSE)
  assertDirectoryExists(x = outPath, access = "rw")

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))

  # derive current version
  load(paste0(intPaths, "/db_info.RData"))
  version <- paste0(db_info$version, "_", format(Sys.Date(), "%Y%m%d"))

  # derive path
  archivePath <- paste0(outPath, "arealDB_", version, "/")
  message("\n-> creating archive '", archivePath, "'")
  dir.create(path = archivePath)
  dir.create(path = paste0(archivePath, "geometries"))
  dir.create(path = paste0(archivePath, "tables"))
  dir.create(path = paste0(archivePath, "_meta"))

  message("-> archiving tables")
  stage3tables_full <- list.files(path = paste0(intPaths, "/tables/stage3"), full.names = TRUE)
  if(length(stage3tables_full) != 0){

    stage3tables <- map(.x = list.files(path = paste0(intPaths, "/tables/stage3")), .f = function(ix){
      temp <- str_split(ix, "[.]")[[1]][1]
    }) |>
      unlist()

    pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(stage3tables))
    for(i in seq_along(stage3tables)){
      temp <- readRDS(file = stage3tables_full[i])
      write_csv(x = temp, file = paste0(archivePath, "tables/", stage3tables[i], ".csv"), na = "")
      pb$tick()
    }

  }

  message("-> archiving geometries")
  stage3geometries_full <- list.files(path = paste0(intPaths, "/geometries/stage3"), full.names = TRUE)
  if(length(stage3geometries_full) != 0){

    stage3geometries <- list.files(path = paste0(intPaths, "/geometries/stage3"))
    file.copy(from = stage3geometries_full,
              to = paste0(archivePath, "geometries/", stage3geometries),
              overwrite = TRUE)

  }

  message("-> archiving inventory tables")
  adb_inventory(type = "dataseries") |>
    write_csv(file = paste0(archivePath, "_meta/inv_dataseries.csv"), na = "")
  adb_inventory(type = "geometries") |>
    write_csv(file = paste0(archivePath, "_meta/inv_geometries.csv"), na = "")
  adb_inventory(type = "tables") |>
    write_csv(file = paste0(archivePath, "_meta/inv_tables.csv"), na = "")
  adb_ontology(type = "ontology") |>
    write_csv(file = paste0(archivePath, "_meta/ontology.csv"), na = "")
  adb_ontology(type = "gazetteer") |>
    write_csv(file = paste0(archivePath, "_meta/gazetteer.csv"), na = "")

  message("-> archiving metadata")
  sI <- sessionInfo()
  save(sI, file = paste0(archivePath, "R_sessionInfo.RData"))
  write_lines(x = capture.output(sI), file = paste0(archivePath, "R_sessionInfo.txt"))
  save(db_info, file = paste0(archivePath, "dbInfo.RData"))
  db_desc_lines <- paste0("version:\n", db_info$version, "\n\n",
                          "authors:\n", paste0("creator: ", paste0(db_info$author$cre, collapse = ", "), "\nauthor: ", paste0(db_info$author$aut, collapse = ", "), "\ncontributor: ", paste0(db_info$author$ctb, collapse = ", ")), "\n\n",
                          "licence:\n", db_info$licence, "\n\n",
                          "gazetteer:\n", db_info$gazetteer, "\n\n", # these two should presumably be replaced with a version label as well
                          "ontology:\n", unique(db_info$ontology), "\n\n", # these two should presumably be replaced with a version label as well
                          "variables:\n", paste0(db_info$variables, collapse = ", "), "\n\n")
  write_lines(x = db_desc_lines, file = paste0(archivePath, "dbInfo.txt"))

  if(compress){
    message("-> compressing database archive")
    archive_write_dir(archive = paste0(outPath, "arealDB_", version, ".7z"),
                      dir = archivePath, format = "7zip")
    unlink(archivePath, recursive = TRUE)
  }

}
