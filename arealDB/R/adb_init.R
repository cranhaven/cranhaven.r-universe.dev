#' Initiate an areal database
#'
#' Initiate a geospatial database or register a database that exists at the root
#' path.
#' @param root [`character(1)`][character]\cr path to the root directory that
#'   contains or shall contain an areal database.
#' @param version [`character(1)`][character]\cr version identifier for this
#'   areal database.
#' @param author [`character(1)`][character]\cr authors that contributed to
#'   building this areal database. Should be a list with items \code{"cre"}
#'   (creator), \code{"aut"} (authors) and \code{"ctb"} (contributors).
#' @param licence [`character(1)`][character]\cr licence (link) for this areal
#'   database.
#' @param gazetteer [`character(1)`][character]\cr path to the gazetteer that
#'   holds the (hierarchical) information of territorial units used in this
#'   database.
#' @param top [`character(1)`][character]\cr the label of the class in the
#'   gazetteer that represents the top-most unit (e.g. country) of the areal
#'   database that shall be started.
#' @param staged [`logical(1)`][logical]\cr whether or not the file structure is
#'   arranged according to stages (with geometries and tables separated), or
#'   merely as input/output (of all types).
#' @param ontology [`list(.)`][list]\cr named list with the path(s) of
#'   ontologies, where the list name identifies the variable that shall be
#'   matched with the ontology at the path.
#' @details This is the first function that is run in a project, as it initiates
#'   the areal database by creating the default sub-directories and initial
#'   inventory tables. When a database has already been set up, this function is
#'   used to register that path in the options of the current R session.
#' @return  No return value, called for the side effect of creating the
#'   directory structure of the new areal database and tables that contain the
#'   database metadata.
#' @examples
#' adb_init(root = paste0(tempdir(), "/newDB"),
#'          version = "1.0.0", licence = "CC-BY-0.4",
#'          author = list(cre = "Gordon Freeman", aut = "Alyx Vance", ctb = "The G-Man"),
#'          gazetteer = paste0(tempdir(), "/newDB/territories.rds"),
#'          top = "al1",
#'          ontology = list(var = paste0(tempdir(), "/newDB/ontology.rds")))
#'
#' getOption("adb_path"); getOption("gazetteer_path")
#' @importFrom checkmate testDirectory testFileExists assertFileExists
#'   assertList
#' @importFrom stringr str_detect
#' @importFrom readr write_csv write_lines
#' @importFrom utils citation
#' @export

adb_init <- function(root, version, author, licence, ontology,
                     gazetteer = NULL, top = NULL, staged = TRUE){

  oldOptions <- options()
  on.exit(options(oldOptions))

  # in testing mode?
  testing <- getOption(x = "adb_testing")

  assertCharacter(x = root, len = 1)
  if(!testing){
    if(!testFileExists(x = gazetteer, access = "rw", extension = "rds")){
      warning("no gazetteer was found in the provided path!")
    }
    assertList(x = ontology, min.len = 1, any.missing = FALSE, names = "named")
  }
  if(str_detect(string = version, pattern = "_")){
    stop("please chose a version name that does not contain the symbol '_'")
  }

  # shitty windows workaround, because a directory may not have a trailing slash
  # for the function "file.exists()" used in assertDirectory()
  lastChar <- substr(x = root, start = nchar(root), stop = nchar(root))
  if(lastChar == "/"){
    root <- substr(root, start = 1, stop = nchar(root)-1)
  }

  # test whether the required directories exist and create them if they don't exist
  if(!testDirectory(x = root, access = "rw")){
    dir.create(file.path(root))
    message("creating root directory ", root)
  }

  if(staged){

    if(!testDirectory(x = file.path(root, "tables"), access = "rw")){
      message("creating ", paste0(".../tables"))
      dir.create(file.path(root, "tables"))
    }
    if(!testDirectory(x = file.path(root, "tables", "stage1"), access = "rw")){
      message("creating ", paste0(".../tables/stage1"))
      dir.create(file.path(root, "tables", "stage1"))
    }
    if(!testDirectory(x = file.path(root, "tables", "stage2"), access = "rw")){
      message("creating ", paste0(".../tables/stage2"))
      dir.create(file.path(root, "tables", "stage2"))
    }
    if(!testDirectory(x = file.path(root, "tables", "stage2", "processed"), access = "rw")){
      message("creating ", paste0(".../tables/stage2/processed"))
      dir.create(file.path(root, "tables", "stage2", "processed"))
    }
    if(!testDirectory(x = file.path(root, "tables", "stage3"), access = "rw")){
      message("creating ", paste0(".../tables/stage3"))
      dir.create(file.path(root, "tables", "stage3"))
    }

    if(!testDirectory(x = file.path(root, "geometries"), access = "rw")){
      message("creating ", paste0(".../geometries"))
      dir.create(file.path(root, "geometries"))
    }

    if(!testDirectory(x = file.path(root, "geometries", "stage1"), access = "rw")){
      message("creating ", paste0(".../geometries/stage1"))
      dir.create(file.path(root, "geometries", "stage1"))
    }
    if(!testDirectory(x = file.path(root, "geometries", "stage2"), access = "rw")){
      message("creating ", paste0(".../geometries/stage2"))
      dir.create(file.path(root, "geometries", "stage2"))
    }
    if(!testDirectory(x = file.path(root, "geometries", "stage2", "processed"), access = "rw")){
      message("creating ", paste0(".../geometries/stage2/processed"))
      dir.create(file.path(root, "geometries", "stage2", "processed"))
    }
    if(!testDirectory(x = file.path(root, "geometries", "stage3"), access = "rw")){
      message("creating ", paste0(".../geometries/stage3"))
      dir.create(file.path(root, "geometries", "stage3"))
    }

  }

  if(!testDirectory(x = file.path(root, "_meta"), access = "rw")){
    message("creating ", paste0(".../_meta"))
    dir.create(file.path(root, "_meta"))
  }
  if(!testDirectory(x = file.path(root, "_meta", "schemas"), access = "rw")){
    message("creating ", paste0(".../_meta/schemas"))
    dir.create(file.path(root, "_meta", "schemas"))
  }
  if(!testDirectory(x = file.path(root, "_meta", "documentation"), access = "rw")){
    message("creating ", paste0(".../_meta/documentation"))
    dir.create(file.path(root, "_meta", "documentation"))
  }

  # create the gazetteer directory and copy the new gazetteer into 'meta'
  if(!is.null(gazetteer)){

    gazName <- str_split(tail(str_split(string = gazetteer, pattern = "/")[[1]], 1), "[.]")[[1]][1]
    if(!testDirectory(x = file.path(root, "_meta", gazName), access = "rw")){
      message("creating ", paste0(".../_meta/", gazName))
      dir.create(file.path(root, "_meta", gazName))
    }
    if(!testFileExists(x = paste0(file.path(root, "_meta", gazName), ".rds"))){
      message("copying gazetteer to ", paste0(".../_meta/", gazName, ".rds"))
      file.copy(from = gazetteer, to = paste0(file.path(root, "_meta", gazName), ".rds"))
    }
    gazetteer <- paste0(file.path(root, "_meta", gazName), ".rds")

    options(gazetteer_path = gazetteer)
    options(gazetteer_top = top)

  }

  for(i in seq_along(unique(ontology))){
    temp <- str_split(tail(str_split(string = unique(ontology)[i], pattern = "/")[[1]], 1), "[.]")[[1]][1]
    if(!testDirectory(x = file.path(root, "_meta", temp), access = "rw")){
      message("creating ", paste0(".../_meta/", temp))
      dir.create(file.path(root, "_meta", temp))
    }
    if(!testFileExists(x = paste0(file.path(root, "_meta", temp), ".rds"))){
      message("copying ontology to ", paste0(".../_meta/", temp, ".rds"))
      file.copy(from = unique(ontology)[[i]], to = paste0(file.path(root, "_meta", temp), ".rds"))
    }
    ontology[which(ontology == unique(ontology)[[i]])] <- paste0(file.path(root, "_meta", temp), ".rds")
  }

  options(ontology_path = ontology)

  # create the empty inventory tables, if they don't exist yet
  if(!testFileExists(x = file.path(root, "_meta", "inventory.rds"))){
    dataseries <- tibble(datID = integer(),
                         name = character(),
                         description = character(),
                         homepage = character(),
                         version = character(),
                         licence_link = character(),
                         notes = character())

    if(!testing){
      references <- citation("arealDB")
      names(references) <- paste0("ehrmann", format(Sys.Date(), "%Y"))
      references$key <- paste0("ehrmann", format(Sys.Date(), "%Y"))
    } else {
      references <- citation(package = "checkmate")
    }

    if(staged){
      tables <- tibble(tabID = integer(),
                       geoID = integer(),
                       datID = integer(),
                       geography = character(),
                       level = character(),
                       start_period = numeric(),
                       end_period = numeric(),
                       stage2_name = character(),
                       schema = character(),
                       stage1_name = character(),
                       stage1_url = character(),
                       download_date = as.Date(NA),
                       update_frequency = character(),
                       metadata_url = character(),
                       metadata_path = character(),
                       notes = character())

      geometries <- tibble(geoID = integer(),
                           datID = integer(),
                           stage2_name = character(),
                           layer = character(),
                           label = character(),
                           ancillary = character(),
                           stage1_name = character(),
                           stage1_url = character(),
                           download_date = as.Date(NA),
                           update_frequency = character(),
                           notes = character())

      inventory <- list(dataseries = dataseries, tables = tables, geometries = geometries,
                        references = references)

    } else {

      inventory <- list(dataseries = dataseries, references = references)

    }

    message("creating ", paste0(".../_meta/inventory.rds"))
    saveRDS(object = inventory, file = paste0(root, "/_meta/inventory.rds"))
  }

  if(!testFileExists(x = paste0(root, "db_info_", version, ".RData"))){
    db_info <- list(version = version,
                    author = author,
                    licence = licence,
                    gazetteer = gazetteer,
                    ontology = unique(ontology))

    message("creating ", paste0(".../db_info_", version, ".RData"))
    save(db_info, file = paste0(root, "/db_info_", version, ".RData"))
  }

  options(adb_path = root)
}