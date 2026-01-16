#' Register a new geometry entry
#'
#' This function registers a new geometry of territorial units into the
#' geospatial database.
#' @param ... [`character(1)`][character]\cr optional named argument selecting
#'   the main territory into which this geometry is nested. The name of this
#'   must be a class of the gazetteer and the value must be one of the territory
#'   names of that class, e.g. \emph{nation = "Estonia"}.
#' @param subset [`character(1)`][character]\cr optional argument to specify
#'   which subset the file contains. This could be a subset of territorial units
#'   (e.g. only one municipality) or of a target variable.
#' @param gSeries [`character(1)`][character]\cr the name of the geometry
#'   dataseries (see \code{\link{regDataseries}}).
#' @param label [`list(.)`][list]\cr list of as many columns as there are in
#'   common in the ontology and this geometry. Must be of the form
#'   \code{list(class = columnName)}, with 'class' as the class of the ontology
#'   corresponding to the respective column name in the geometry.
#' @param ancillary [`list(.)`][list]\cr optinal list of columns containing
#'   ancillary information. Must be of the form \code{list(attribute =
#'   columnName)}, where \code{attribute} can be one or
#'   several of \itemize{
#'     \item \code{"name_ltn"} (the english name in latin letters)
#'     \item \code{"name_lcl"} (the name in local language and letters)
#'     \item \code{"code"} (any code describing the unit)
#'     \item \code{"type"} (the type of territorial unit)
#'     \item \code{"uri"} (the semantic web URI) or
#'     \item \code{"flag"} (any flag attributed to the unit).}
#' @param layer [`character(1)`][character]\cr the name of the file's layer from
#'   which the geometry should be created (if applicable).
#' @param archive [`character(1)`][character]\cr the original file (perhaps a
#'   *.zip) from which the geometry emerges.
#' @param archiveLink [`character(1)`][character]\cr download-link of the
#'   archive.
#' @param downloadDate [`character(1)`][character]\cr value describing the
#'   download date of this dataset (in YYYY-MM-DD format).
#' @param updateFrequency [`character(1)`][character]\cr value describing the
#'   frequency with which the dataset is updated, according to the ISO 19115
#'   Codelist, MD_MaintenanceFrequencyCode. Possible values are: 'continual',
#'   'daily', 'weekly', 'fortnightly', 'quarterly', 'biannually', 'annually',
#'   'asNeeded', 'irregular', 'notPlanned', 'unknown', 'periodic',
#'   'semimonthly', 'biennially'.
#' @param notes [`character(1)`][character]\cr optional notes that are assigned
#'   to all features of this geometry.
#' @param overwrite [`logical(1)`][logical]\cr whether or not the geometry to
#'   register shall overwrite a potentially already existing older version.
#' @details When processing geometries to which areal data shall be linked,
#'   carry out the following steps: \enumerate{ \item Determine the main
#'   territory (such as a nation, or any other polygon), a \code{subset} (if
#'   applicable), the dataseries of the geometry and the ontology \code{label},
#'   and provide them as arguments to this function. \item Run the function.
#'   \item Export the shapefile with the following properties: \itemize{ \item
#'   Format: GeoPackage \item File name: What is provided as message by this
#'   function \item CRS: EPSG:4326 - WGS 84 \item make sure that 'all fields are
#'   exported'} \item Confirm that you have saved the file.}
#' @return Returns a tibble of the entry that is appended to
#'   'inv_geometries.csv'.
#' @family register functions
#' @examples
#' if(dev.interactive()){
#'   # build the example database
#'   adb_exampleDB(until = "regDataseries", path = tempdir())
#'
#'   # The GADM dataset comes as *.7z archive
#'   regGeometry(gSeries = "gadm",
#'               label = list(al1 = "NAME_0"),
#'               layer = "example_geom1",
#'               archive = "example_geom.7z|example_geom1.gpkg",
#'               archiveLink = "https://gadm.org/",
#'               nextUpdate = "2019-10-01",
#'               updateFrequency = "quarterly")
#'
#'   # The second administrative level in GADM contains names in the columns
#'   # NAME_0 and NAME_1
#'   regGeometry(gSeries = "gadm",
#'               label = list(al1 = "NAME_0", al2 = "NAME_1"),
#'               ancillary = list(name_lcl = "VARNAME_1", code = "GID_1", type = "TYPE_1"),
#'               layer = "example_geom2",
#'               archive = "example_geom.7z|example_geom2.gpkg",
#'               archiveLink = "https://gadm.org/",
#'               nextUpdate = "2019-10-01",
#'               updateFrequency = "quarterly")
#' }
#' @importFrom checkmate assertNames assertCharacter assertIntegerish
#'   assertFileExists testChoice assertLogical testSubset assertDate
#' @importFrom ontologics load_ontology
#' @importFrom readr read_csv
#' @importFrom dplyr filter distinct pull
#' @importFrom stringr str_split
#' @importFrom sf st_layers read_sf
#' @importFrom tibble tibble
#' @export

regGeometry <- function(..., subset = NULL, gSeries = NULL, label = NULL,
                        ancillary = NULL, layer = NULL, archive = NULL,
                        archiveLink = NULL, downloadDate = NULL, updateFrequency = NULL,
                        notes = NULL, overwrite = FALSE){

  # set internal function
  testCompressed <- function(x){
    assertCharacter(x = x, any.missing = FALSE, len = 1)
    return(grepl("^.*(.gz|.bz2|.tar|.zip|.tgz|.gzip|.7z)[[:space:]]*$", x))
  }

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))
  gazPath <- paste0(getOption(x = "gazetteer_path"))
  topClass <- paste0(getOption(x = "gazetteer_top"))

  gaz <- load_ontology(gazPath)
  gazClasses <- get_class(ontology = gazPath)

  # get tables
  inventory <- readRDS(paste0(getOption(x = "adb_path"), "/_meta/inventory.rds"))
  inv_dataseries <- inventory$dataseries
  inv_geometries <- inventory$geometries

  if(dim(inv_dataseries)[1] == 0){
    stop("'inv_dataseries.csv' does not contain any entries!")
  }

  # in testing mode?
  testing <- getOption(x = "adb_testing")

  # check validity of arguments
  assertList(x = label, any.missing = FALSE)
  assertCharacter(x = subset, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = gSeries, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertList(x = ancillary, max.len = 6, null.ok = TRUE)
  assertCharacter(x = layer, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = archive, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = archiveLink, any.missing = FALSE, null.ok = TRUE)
  assertDate(x = downloadDate, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = updateFrequency, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = notes, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertLogical(x = overwrite, len = 1)
  assertNames(x = colnames(inv_dataseries),
              permutation.of = c("datID", "name", "description", "homepage", "version", "licence_link", "notes"))
  assertNames(x = colnames(inv_geometries),
              permutation.of = c("geoID", "datID", "stage2_name", "layer", "label", "ancillary", "stage1_name", "stage1_url", "download_date", "update_frequency", "notes"))
  if(!is.null(ancillary)) assertNames(x = names(ancillary), subset.of = c("name_ltn", "name_lcl", "code", "type", "uri", "flag"))

  broadest <- exprs(..., .named = TRUE)
  if(length(broadest) > 0){
    mainPoly <- broadest[[1]]

    # test whether broadest exists in the gazetteer
    assertSubset(x = names(broadest), choices = gazClasses$label)
  } else {
    mainPoly <- ""
  }

  if(!is.null(subset)){
    if(grepl(pattern = "_", x = subset)){
      stop("please give a subset that does not contain any '_' characters.")
    }
  }

  if(is.null(gSeries)){
    message("please type in to which series the geometry belongs: ")
    if(!testing){
      gSeries <- readline()
    } else {
      gSeries <- "gadm"
    }

    if(grepl(pattern = "_", x = gSeries)){
      stop("! please give a geometry series name that does not contain any '_' characters !")
    }

    if(!testing){
      if(!any(inv_dataseries$name %in% gSeries)){
        stop(paste0("! please first create the new geometry series '", gSeries,"' via 'regDataseries()' !"))
      }
    } else {
      dataSeries <- NA_integer_
    }
  } else{
    dataSeries <- inv_dataseries$datID[inv_dataseries$name %in% gSeries]
    if(length(dataSeries) < 1){
      stop(paste0("! please first create the new geometry series '", gSeries,"' via 'regDataseries()' !"))
    }
  }


  assertSubset(x = names(label), choices = gazClasses$label)
  theLabel <- tail(names(label), 1)
  labelString <- paste0(paste0(names(label), "=", label), collapse = "|")

  if(!is.null(ancillary)){
    ancillaryString <- paste0(paste0(names(ancillary), "=", ancillary), collapse = "|")
  } else {
    ancillaryString <- ""
  }

  if(is.null(archive)){
    message("please type in the archives' file name: ")
    if(!testing){
      archive <- readline()
    } else {
      archive <- "example_geom.7z"
    }
    if(is.na(archive)){
      archive = NA_character_
    }
  }

  # put together file name and get confirmation that file should exist now
  fileName <- paste0(mainPoly, "_", theLabel, "_", subset, "_", gSeries, ".gpkg")
  filePath <- paste0(intPaths, "/geometries/stage2/", fileName)
  filesTrace <- str_split(archive, "\\|")[[1]]

  newGID <- ifelse(length(inv_geometries$geoID)==0, 1, as.integer(max(inv_geometries$geoID)+1))
  if(any(inv_geometries$stage2_name %in% fileName)){
    if(overwrite){
      newGID <- inv_geometries$geoID[which(inv_geometries$stage2_name %in% fileName)]
    } else {
      temp <- inv_geometries[which(inv_geometries$stage2_name %in% fileName), ]
      message(paste0("! the geometry '", fileName, "' has already been registered !"))
      return(temp)
    }
  }

  if(is.null(archiveLink)){
    message("please type in the weblink from which the archive was downloaded: ")
    if(!testing){
      archiveLink <- readline()
    } else {
      archiveLink <- "https://gadm.org/downloads/example_geom.7z.html"
    }
    if(is.na(archiveLink)){
      archiveLink = NA_character_
    }
  }

  if(is.null(updateFrequency)){
    message(paste("please type in the frequency in which the table gets updated ..."))
    if(!testing){
      updateFrequency <- readline(" -> select one of: continual, daily, weekly, fortnightly, quarterly, biannually, annually, asNeeded, irregular, notPlanned, unknown, periodic, semimonthly, biennially: ")
      while(!is.element(updateFrequency,
                        c("continual", "daily","weekly", "fortnightly", "quarterly", "biannually", "annually", "asNeeded", "irregular", "notPlanned", "unknown", "periodic", "semimonthly", "biennially"))){
        # test missing
        message(paste(" -> input one of: continual, daily, weekly, fortnightly, quarterly, biannually, annually, asNeeded, irregular, notPlanned, unknown, periodic, semimonthly, biennially"))
        updateFrequency <- readline()
      }
    } else {
      updateFrequency <- "quarterly"
    }
    if(is.na(updateFrequency)){
      updateFrequency = as.Date(NA)
    }
  }

  if(is.null(downloadDate)){
    message("please type in when the geometry was downloaded (YYYY-MM-DD): ")
    if(!testing){
      downloadDate <- as.Date(readline(), "%Y-%m-%d")
    } else {
      downloadDate <- as.Date("2019-10-01", "%Y-%m-%d")
    }
    if(is.na(downloadDate)){
      downloadDate = as.Date("0001-01-01")
    }
  }

  if(is.null(notes)){
    notes = NA_character_
  }

  # test whether the archive file is available
  if(!testFileExists(x = paste0(intPaths, "/geometries/stage1/", filesTrace[1]))){
    message(paste0("... please store the archive '", filesTrace[[1]], "' in './geometries/stage1'"))
    if(!testing){
      done <- readline(" -> press any key when done: ")
    }

    # make sure that the file is really there
    assertFileExists(x = paste0(intPaths, "/geometries/stage1/", filesTrace[1]))

    # ... and if it is compressed, whether also the file therein is given that contains the data
    if(testCompressed(x = filesTrace[1]) & length(filesTrace) < 2){
      message(paste0("please give the name of the file in ", filesTrace[1]," that contains the geometries: "))
      if(!testing){
        theArchiveFile <- readline()
      } else {
        theArchiveFile <- "1__gadm.gpkg"
      }
      archive <- paste0(archive, "|", theArchiveFile)
    }
  }

  if(!testFileExists(x = filePath, extension = "gpkg")){
    processedPath <- paste0(intPaths, "/geometries/stage2/processed/", fileName)
    if(testFileExists(x = processedPath, extension = "gpkg")){
      temp <- inv_geometries[which(inv_geometries$stage2_name %in% fileName), ]
      message(paste0("! the geometry '", fileName, "' has already been normalised !"))
      return(temp)
    }

    message(paste0("... please store the geometry as '", fileName, "' in './geometries/stage2'"))
    if(!testing){
      done <- readline(" -> press any key when done: ")
    }
    # make sure that the file is really there
    assertFileExists(x = filePath, extension = "gpkg")
  }

  # to check that what has been given in 'nation' and 'nameCol' is in fact a
  # column in the geometry, load it
  if(is.null(mainPoly)){
    theGeometry <- read_sf(dsn = filePath, stringsAsFactors = FALSE)
  }

  nameCols <- unlist(label, use.names = FALSE)

  # determine which layers exist and ask the user which to chose, if none is
  # given
  layers <- st_layers(dsn = filePath)
  if(length(layers$name) != 1){
    if(is.null(layer)){
      message(paste0("... Please chose only one of the layers ", paste0(layers$name, collapse = ", "), ": "))
      if(!testing){
        layer <- readline()
      } else {
        layer <- "example_geom"
      }
    }
    assertChoice(x = layer, choices = layers$name)
  } else{
    layer <- layers$name
  }

  # construct new documentation
  doc <- tibble(geoID = newGID,
                datID = dataSeries,
                stage2_name = fileName,
                layer = layer,
                label = labelString,
                ancillary = ancillaryString,
                stage1_name = archive,
                stage1_url = archiveLink,
                download_date = downloadDate,
                update_frequency = updateFrequency,
                notes = notes)

  if(dim(inv_geometries)[1] != 0){
    if(doc$stage1_name %in% inv_geometries$stage1_name & doc$stage2_name %in% inv_geometries$stage2_name){
      doc$geoID <- inv_geometries$geoID[inv_geometries$stage1_name %in% doc$stage1_name & inv_geometries$stage2_name %in% doc$stage2_name]
      inv_geometries[inv_geometries$stage1_name %in% doc$stage1_name & inv_geometries$stage2_name %in% doc$stage2_name,] <- doc
      inventory$geometries <- inv_geometries
    } else {
      inventory$geometries <- bind_rows(inv_geometries, doc)
    }
  } else {
    inventory$geometries <- doc
  }
  saveRDS(object = inventory, file = paste0(intPaths, "/_meta/inventory.rds"))

  return(doc)
}