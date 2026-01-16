#' Register a new dataseries
#'
#' This function registers a new dataseries of both, geometries or areal data
#' into the geospatial database. This contains the name and relevant meta-data
#' of a dataseries to enable provenance tracking and reproducability.
#' @param name [`character(1)`][character]\cr the dataseries abbreviation or
#'   name.
#' @param description [`character()`][character]\cr the "long name" or "brief
#'   description" of the dataseries.
#' @param homepage [`character(1)`][character]\cr the homepage of the data
#'   provider where the dataseries or additional information can be found.
#' @param version [`character(1)`][character]\cr the version number or date when
#'   meta data of the dataseries were recorded.
#' @param licence_link [`character(1)`][character]\cr link to the licence or the
#'   webpage from which the licence was copied.
#' @param reference [`bibentry(1)`][bibentry]\cr in case the dataseries comes
#'   with a reference, provide this here as bibentry object.
#' @param notes [`character(1)`][character]\cr optional notes.
#' @param overwrite [`logical(1)`][logical]\cr whether or not the dataseries to
#'   register shall overwrite a potentially already existing older version.
#' @return Returns a tibble of the new entry that is appended to
#'   'inv_dataseries.csv'.
#' @family register functions
#' @examples
#' if(dev.interactive()){
#'   # start the example database
#'   adb_exampleDB(until = "match_gazetteer", path = tempdir())
#'
#'   regDataseries(name = "gadm",
#'                 description = "Database of Global Administrative Areas",
#'                 version = "3.6",
#'                 homepage = "https://gadm.org/index.html",
#'                 licence_link = "https://gadm.org/license.html")
#' }
#' @importFrom readr read_csv
#' @importFrom checkmate assertDataFrame assertNames assertCharacter
#'   assertLogical
#' @importFrom tibble tibble
#' @export

regDataseries <- function(name = NULL, description = NULL, homepage = NULL,
                          version = NULL, licence_link = NULL, reference = NULL,
                          notes = NULL, overwrite = FALSE){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))

  # get tables
  inventory <- readRDS(paste0(getOption(x = "adb_path"), "/_meta/inventory.rds"))
  inv_dataseries <- inventory$dataseries

  # in testing mode?
  testing <- getOption(x = "adb_testing")

  # check validity of arguments
  assertNames(x = colnames(inv_dataseries),
              permutation.of = c("datID", "name", "description", "homepage", "version", "licence_link", "notes"))
  assertCharacter(x = name, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = description, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = homepage, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = version, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = licence_link, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertClass(x = reference, classes = "bibentry", null.ok = TRUE)
  assertCharacter(x = notes, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertLogical(x = overwrite, len = 1)

  # ask for missing and required arguments
  if(is.null(name)){
    message("please type in the dataseries abbreviation: ")
    if(!testing){
      theName <- readline()
    } else {
      theName <- NA
    }
    if(is.na(theName)){
      theName = NA_character_
    }
  } else{
    if(name %in% inv_dataseries$name & !overwrite){
      message("! the dataseries '", name, "' has already been registered !")
      temp <- inv_dataseries[which(inv_dataseries$name %in% name), ]
      return(temp)
    }
    theName <- name
  }

  if(is.null(description)){
    message("please type in the long name or description of the series: ")
    if(!testing){
      theDescription <- readline()
    } else {
      theDescription <- NA
    }
    if(is.na(theDescription)){
      theDescription = NA_character_
    }
  } else{
    theDescription <- description
  }

  if(is.null(homepage)){
    message("please type in the dataseries homepage: ")
    if(!testing){
      theHomepage <- readline()
    } else {
      theHomepage <- NA
    }
    if(is.na(theHomepage)){
      theHomepage = NA_character_
    }
  } else{
    theHomepage <- homepage
  }

  if(is.null(version)){
    message("please type in the version or download date: ")
    if(!testing){
      theVersion <- readline()
    } else {
      theVersion <- NA
    }
    if(is.na(theVersion)){
      theVersion = NA_character_
    }
  } else{
    theVersion <- version
  }

  if(is.null(licence_link)){
    message("please type in the weblink to the dataseries licence: ")
    if(!testing){
      theLicence_link <- readline()
    } else {
      theLicence_link <- NA
    }
    if(is.na(theLicence_link)){
      theLicence_link = NA_character_
    }
  } else{
    theLicence_link <- licence_link
  }

  if(is.null(notes)){
    notes = NA_character_
  }

  # construct new documentation
  newDID <- ifelse(length(inv_dataseries$datID)==0, 1, as.integer(max(inv_dataseries$datID)+1))
  if(overwrite){
    if(theName %in% inv_dataseries$name){
      newDID <- inv_dataseries$datID[which(inv_dataseries$name %in% theName)]
    }
  }
  temp <- tibble(datID = as.integer(newDID),
                 name = theName,
                 description = theDescription,
                 homepage = theHomepage,
                 version = theVersion,
                 licence_link = theLicence_link,
                 notes = notes)

  if(!is.null(reference)){
    names(reference) <- name
    reference$key <- name
  }

  inventory$dataseries <- bind_rows(inv_dataseries, temp)
  inventory$references <- c(inventory$references, reference)
  saveRDS(object = inventory, file = paste0(intPaths, "/_meta/inventory.rds"))


  return(temp)
}