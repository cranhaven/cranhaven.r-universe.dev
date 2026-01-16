#' Build an example areal database
#'
#' This function helps setting up an example database up until a certain step.
#' @param path [`character(1)`][character]\cr The database gets created by
#'   default in tempdir(), but if you want it in a particular location, specify
#'   that in this argument.
#' @param until [`character(1)`][character]\cr The database building step in
#'   terms of the function names until which the example database shall be
#'   built, one of \code{"start_arealDB"}, \code{"regDataseries"},
#'   \code{"regGeometry"}, \code{"regTable"}, \code{"normGeometry"} or
#'   \code{"normTable"}.
#' @param verbose [`logical(1)`][logical]\cr be verbose about building the
#'   example database (default \code{FALSE}).
#' @details Setting up a database with an R-based tool can appear to be
#'   cumbersome and too complex and thus intimidating. By creating an example
#'   database, this functions allows interested users to learn step by step how
#'   to build a database of areal data. Moreover, all functions in this package
#'   contain verbose information and ask for information that would be missing
#'   or lead to an inconsistent database, before a failure renders hours of work
#'   useless.
#' @return No return value, called for the side effect of creating an example
#'   database at the specified \code{path}.
#' @examples
#' if(dev.interactive()){
#' # to build the full example database
#' adb_example(path = paste0(tempdir(), "/newDB"))
#'
#' # to make the example database until a certain step
#' adb_example(path = paste0(tempdir(), "/newDB"), until = "regDataseries")
#'
#' }
#' @importFrom checkmate assertChoice testDirectoryExists
#' @importFrom readr read_csv
#' @importFrom tabshiftr setFormat setIDVar setObsVar
#' @export

adb_example <- function(path = NULL, until = NULL, verbose = FALSE){

  # set internal paths
  inPath <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)
  steps <- c("adb_init", "regDataseries", "regGeometry", "regTable", "normGeometry", "normTable")
  if (is.null(until)) {
    until <- "normTable"
  }
  assertChoice(x = until, choices = steps)

  if(testDirectoryExists(path)){
    unlink(path, recursive = TRUE)
  }

  gazPath <- paste0(path, "/territories.rds")
  ontoPath <- list(commodity = paste0(path, "/ontology.rds"))
  theSteps <- steps[1:which(steps %in% until)]

  # enable testing, this inserts values to readLine() calls that would otherwise
  # not be answered by the test
  oldOptions <- options()
  on.exit(options(oldOptions))
  options(adb_testing = TRUE)

  dir.create(file.path(path))
  saveRDS(object = arealDB::territories, file = gazPath)
  if (any(theSteps %in% "adb_init")) {
    adb_init(root = path,
             version = "some0.0.1", licence = "https://creativecommons.org/licenses/by-sa/4.0/",
             author = "Gordon Freeman",
             gazetteer = gazPath, top = "al1",
             ontology = ontoPath)
  }

  # load input data
  file.copy(from = paste0(inPath, "/example_geom.7z"),
            to = paste0(path, "/geometries/stage1/example_geom.7z"))
  dir.create(file.path(path, "/tables/stage1/madeUp/"))
  file.copy(from = paste0(inPath, "/example_table.7z"),
            to = paste0(path, "/tables/stage1/madeUp/example_table.7z"))

  # load geometries
  file.copy(from = paste0(inPath, "/example_geom1.gpkg"),
            to = paste0(path, "/geometries/stage2/_al1__gadm.gpkg"))
  file.copy(from = paste0(inPath, "/example_geom2.gpkg"),
            to = paste0(path, "/geometries/stage2/_al2__gadm.gpkg"))
  file.copy(from = paste0(inPath, "/example_geom3.gpkg"),
            to = paste0(path, "/geometries/stage2/_al3__gadm.gpkg"))
  file.copy(from = paste0(inPath, "/example_geom4.gpkg"),
            to = paste0(path, "/geometries/stage2/_al3__madeUp.gpkg"))

  # load tables (and schema)
  file.copy(from = paste0(inPath, "/example_schema.rds"),
            to = paste0(path, "/_meta/schemas/example_schema.rds"))
  file.copy(from = paste0(inPath, "/example_table1.csv"),
            to = paste0(path, "/tables/stage2/_al1_barleyMaize_1990_2017_madeUp.csv"))
  file.copy(from = paste0(inPath, "/example_table2.csv"),
            to = paste0(path, "/tables/stage2/aNation_al2_barleyMaize_1990_2017_madeUp.csv"))

  # load gazetteer
  file.copy(from = paste0(inPath, "/match_madeUp.rds"),
            to = paste0(path, "/_meta/territories/match_madeUp.rds"))
  file.copy(from = paste0(inPath, "/match_gadm.rds"),
            to = paste0(path, "/_meta/territories/match_gadm.rds"))


  if (any(theSteps %in% "regDataseries")) {
    regDataseries(name = "gadm",
                  description = "Database of Global Administrative Areas",
                  homepage = "https://gadm.org/index.html",
                  version = "1.0",
                  licence_link = "https://gadm.org/license.html")

    regDataseries(name = "madeUp",
                  description = "Made-Up Concepts",
                  homepage = "https://en.wikipedia.org/wiki/String_theory",
                  version = "1.0",
                  licence_link = "https://creativecommons.org/share-your-work/public-domain/cc0/")
  }

  if(any(theSteps %in% "regGeometry")){

    regGeometry(gSeries = "gadm",
                label = list(al1 = "NAME_0"),
                layer = "example_geom1",
                archive = "example_geom.7z|example_geom1.gpkg",
                archiveLink = "https://gadm.org/",
                downloadDate = as.Date("2019-10-01"),
                updateFrequency = "quarterly")

    regGeometry(gSeries = "gadm",
                label = list(al1 = "NAME_0", al2 = "NAME_1"),
                layer = "example_geom2",
                archive = "example_geom.7z|example_geom2.gpkg",
                archiveLink = "https://gadm.org/",
                downloadDate = as.Date("2019-10-01"),
                updateFrequency = "quarterly")

    regGeometry(gSeries = "gadm",
                label = list(al1 = "NAME_0", al2 = "NAME_1", al3 = "NAME_2"),
                layer = "example_geom3",
                archive = "example_geom.7z|example_geom3.gpkg",
                archiveLink = "https://gadm.org/",
                downloadDate = as.Date("2019-10-01"),
                updateFrequency = "quarterly")

    regGeometry(gSeries = "madeUp",
                label = list(al1 = "NAME_0", al2 = "NAME_1", al3 = "NAME_2"),
                layer = "example_geom4",
                archive = "example_geom.7z|example_geom4.gpkg",
                archiveLink = "https://gadm.org/",
                downloadDate = as.Date("2019-10-01"),
                updateFrequency = "quarterly")

  }

  if(any(theSteps %in% "regTable")){

    meta_madeUp_1 <- tabshiftr::schema_default %>%
      setIDVar(name = "al1", columns = 1) %>%
      setIDVar(name = "year", columns = 2) %>%
      setIDVar(name = "commodity", columns = 3) %>%
      setObsVar(name = "harvested", columns = 4) %>%
      setObsVar(name = "production", columns = 5)

    meta_madeUp_2 <- tabshiftr::schema_default %>%
      setFormat(decimal = ".", na_values = c("", "NA")) %>%
      setIDVar(name = "al1", columns = 1) %>%
      setIDVar(name = "al2", columns = 2) %>%
      setIDVar(name = "year", columns = 3) %>%
      setIDVar(name = "commodity", columns = 4) %>%
      setObsVar(name = "harvested", columns = 5) %>%
      setObsVar(name = "production", columns = 6)

    regTable(subset = "barleyMaize",
             dSeries = "madeUp",
             gSeries = "gadm",
             label = "al1",
             begin = 1990,
             end = 2017,
             schema = meta_madeUp_1,
             archive = "example_table.7z|example_table1.csv",
             archiveLink = "https://en.wikipedia.org/wiki/Data_lineage",
             downloadDate = as.Date("2019-10-01"),
             updateFrequency = "quarterly",
             metadataLink = "https://en.wikipedia.org/wiki/Metadata",
             metadataPath = "my/local/path")

    regTable(al1 = "aNation",
             subset = "barleyMaize",
             dSeries = "madeUp",
             gSeries = "gadm",
             label = "al2",
             begin = 1990,
             end = 2017,
             schema = meta_madeUp_2,
             archive = "example_table.7z|example_table2.csv",
             archiveLink = "https://en.wikipedia.org/wiki/Data_lineage",
             downloadDate = as.Date("2019-10-01"),
             updateFrequency = "quarterly",
             metadataLink = "https://en.wikipedia.org/wiki/Metadata",
             metadataPath = "my/local/path")

  }

  # ... and then try to read them in via match_ontology above
  if(any(theSteps %in% "normGeometry")){
    normGeometry(verbose = verbose)
  }

  if(any(theSteps %in% "normTable")){
    normTable(verbose = verbose)
  }

}