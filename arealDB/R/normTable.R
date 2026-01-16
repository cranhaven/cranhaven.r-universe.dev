#' Normalise data tables
#'
#' Harmonise and integrate data tables into standardised format
#' @param input [`character(1)`][character]\cr path of the file to normalise. If
#'   this is left empty, all files at stage two as subset by \code{pattern} are
#'   chosen.
#' @param pattern [`character(1)`][character]\cr an optional regular expression.
#'   Only dataset names which match the regular expression will be processed.
#' @param query [`character(1)`][character]\cr the expression that would be used
#'   in \code{\link[dplyr]{filter}} to subset a tibble in terms of the columns
#'   defined via the schema and given as a single character string, such as
#'   \code{"al1 == 'Estonia'"}.
#' @param ontoMatch [`character(.)`][character]\cr name of the column(s) that
#'   shall be matched with an ontology (defined in \code{\link{adb_init}}).
#' @param beep [`integerish(1)`][integer]\cr Number specifying what sound to be
#'   played to signal the user that a point of interaction is reached by the
#'   program, see \code{\link[beepr]{beep}}.
#' @param verbose [`logical(1)`][logical]\cr be verbose about translating terms
#'   (default \code{FALSE}). Furthermore, you can use
#'   \code{\link{suppressMessages}} to make this function completely silent.
#' @details To normalise data tables, this function proceeds as follows:
#'   \enumerate{ \item Read in \code{input} and extract initial metadata from
#'   the file name. \item Employ the function
#'   [tabshiftr::reorganise()] to reshape \code{input} according to
#'   the respective schema description. \item The territorial names are matched
#'   with the gazetteer to harmonise new territorial names (at this step, the
#'   function might ask the user to edit the file 'matching.csv' to align new
#'   names with already harmonised names). \item Harmonise territorial unit
#'   names. \item store the processed data table at
#'   stage three.}
#' @family normalise functions
#' @return This function harmonises and integrates so far unprocessed data
#'   tables at stage two into stage three of the areal database. It produces for
#'   each main polygon (e.g. nation) in the registered data tables a file that
#'   includes all thematic areal data.
#' @examples
#' if(dev.interactive()){
#'   # build the example database
#'   adb_example(until = "normGeometry", path = tempdir())
#'
#'   # normalise all available data tables ...
#'   normTable()
#'
#'   # ... and check the result
#'   output <- readRDS(paste0(tempdir(), "/tables/stage3/Estonia.rds"))
#' }
#' @importFrom checkmate assertNames assertFileExists assertLogical
#' @importFrom ontologics load_ontology
#' @importFrom rlang exprs :=
#' @importFrom tabshiftr reorganise
#' @importFrom dplyr mutate select pull full_join bind_rows
#' @importFrom magrittr %>%
#' @importFrom readr read_csv cols
#' @importFrom stringr str_split str_detect
#' @importFrom tidyselect everything
#' @importFrom utils read.csv
#' @importFrom beepr beep
#' @export

normTable <- function(input = NULL, pattern = NULL, query = NULL, ontoMatch = NULL,
                      beep = NULL, verbose = FALSE){

  # set internal paths
  intPaths <- getOption(x = "adb_path")
  gazPath <- getOption(x = "gazetteer_path")

  # get territorial context
  topClass <- paste0(getOption(x = "gazetteer_top"))
  topUnits <- get_concept(class = topClass, ontology = gazPath) %>%
    arrange(label)

  if(is.null(input)){
    input <- list.files(path = paste0(intPaths, "/tables/stage2"), full.names = TRUE, pattern = pattern)
  } else {
    assertFileExists(x = input, access = "r")
  }

  # set internal objects
  moveFile <- TRUE

  # get tables
  inventory <- readRDS(paste0(getOption(x = "adb_path"), "/_meta/inventory.rds"))
  inv_dataseries <- inventory$dataseries
  inv_tables <- inventory$tables

  # check validity of arguments
  assertCharacter(x = query, len = 1, null.ok = TRUE)
  assertNames(x = colnames(inv_tables),
              permutation.of = c("tabID", "datID", "geoID", "geography", "level", "start_period", "end_period", "stage2_name", "schema", "stage1_name", "stage1_url", "download_date", "update_frequency", "metadata_url", "metadata_path", "notes"))
  assertNames(x = colnames(inv_dataseries),
              permutation.of = c("datID", "name", "description", "homepage", "version", "licence_link", "notes"))
  assertCharacter(x = ontoMatch, min.len = 1, any.missing = FALSE, null.ok = TRUE)

  ret <- NULL
  for(i in seq_along(input)){

    thisInput <- input[i]

    # scrutinize file-name (the fields, which are delimited by "_" carry important information)
    pathStr <- str_split(thisInput, "/")[[1]]
    file_name <- pathStr[length(pathStr)]
    fields <- str_split(file_name, "_")[[1]]

    if(getOption("width")-(nchar(i)+nchar(length(input))+21+nchar(file_name)) <= 0){
      stop("please increase the width of the console, or choose smaller file names.")
    }

    if(!file_name %in% inv_tables$stage2_name){
      message("\n--- ", i, " / ", length(input), " skipping ", rep("-", times = getOption("width")-(nchar(i)+nchar(length(input))+21+nchar(file_name))), " ", file_name, " ---")
      next
    } else {
      message("\n--- ", i, " / ", length(input), " ", rep("-", times = getOption("width")-(nchar(i)+nchar(length(input))+13+nchar(file_name))), " ", file_name, " ---")
    }

    # get some variables
    lut <- inv_tables[grep(pattern = paste0("^", file_name, "$"), x = inv_tables$stage2_name),]
    if(file_name %in% lut$stage2_name){
      geoID <- lut$geoID
      tabID <- lut$tabID
      datID <- lut$datID
      thisSchema <- lut$schema

      gSeries <- inv_dataseries$name[inv_dataseries$datID == geoID]
      dSeries <- inv_dataseries$name[inv_dataseries$datID == datID]
    } else {
      stop(paste0("  ! the file '", file_name, "' has not been registered yet."))
    }

    algorithm <- readRDS(file = paste0(intPaths, "/_meta/schemas/", thisSchema, ".rds"))
    if(!exists(x = "algorithm")){
      stop(paste0("please create the schema desciption '", algorithm, "' for the file '", file_name, "'.\n  --> See '?meta_default' for details"))
    }

    # reorganise data
    message("\n--> reading new data table ...")
    thisTable <- read.csv(file = thisInput, header = FALSE, strip.white = TRUE, as.is = TRUE, colClasses = "character", na.strings = algorithm@format$na, encoding = "UTF-8") %>%
      as_tibble() %>%
      mutate(across(where(is.character), ~na_if(x = ., y = "")))

    message("    reorganising table with '", thisSchema, "' ...")
    thisTable <- thisTable %>%
      reorganise(schema = algorithm)

    if(!is.null(query)){
      moveFile <- FALSE
      thisTable <- thisTable %>%
        filter(eval(parse(text = query)))
    }

    message("    harmonizing territory names ...")
    targetCols <- get_class(ontology = gazPath) %>%
      pull(label)
    targetCols <- targetCols[targetCols %in% colnames(thisTable)]

    if(targetCols[1] != topClass){
      theUnits <- str_split(string = lut$stage2_name, pattern = "_")[[1]][1]
    } else {
      theUnits <- NULL
    }

    if(!topClass %in% targetCols){
      thisTable <- thisTable %>%
        add_column(tibble(!!topClass := theUnits), .before = targetCols[1])
      targetCols <- c(topClass, targetCols)
    }

    thatTable <- .matchOntology(table = thisTable,
                                columns = targetCols,
                                dataseries = dSeries,
                                ontology = gazPath,
                                verbose = verbose,
                                beep = beep) %>%
      unite(col = "gazMatch", match, external, sep = "--", na.rm = TRUE) %>%
      rename(gazID = id) %>%
      select(-has_broader, -class, -description) %>%
      mutate(gazID = str_replace_all(string = gazID, pattern = "[.]", replacement = "-"))

    if(!is.null(ontoMatch)){
      message("    harmonizing thematic concepts ...")
      assertNames(x = ontoMatch, subset.of = names(thatTable))
      ontoPath <- getOption(x = "ontology_path")[[ontoMatch]]
      thatTable <- .matchOntology(table = thatTable,
                                  columns = ontoMatch,
                                  dataseries = dSeries,
                                  ontology = ontoPath,
                                  verbose = verbose,
                                  beep = beep) %>%
        unite(col = "ontoMatch", match, external, sep = "--", na.rm = TRUE) %>%
        rename(ontoID = id) %>%
        select(-has_broader, -class, -description)
    }

    thatTable <- thatTable %>%
      mutate(gazID = str_replace_all(string = gazID, pattern = "-", replacement = "."),
             tabID = tabID,
             geoID = geoID)

    # produce output
    if(is.null(theUnits)){
      theUnits <- unique(eval(expr = parse(text = targetCols[1]), envir = thatTable)) %>%
        na.omit() %>%
        as.character()
    }

    for(j in seq_along(theUnits)){

      if(length(theUnits) != 1){
        tempOut <- thatTable %>%
          filter(.data[[targetCols[1]]] == theUnits[j])
      } else {
        tempOut <- thatTable
      }
      tempOut <- tempOut %>%
        unite(col = "gazName", all_of(targetCols), sep = ".") %>%
        select(tabID, geoID, gazID, gazName, gazMatch, everything()) %>%
        distinct()

      # append output to previous file
      # avail <- list.files(path = paste0(intPaths, "/tables/stage3/"), pattern = paste0("^", theUnits[j], ".", outType))
      avail <- list.files(path = paste0(intPaths, "/tables/stage3/"), pattern = paste0("^", theUnits[j], ".rds"))

      if(length(avail) == 1){

        prevData <- readRDS(file = paste0(intPaths, "/tables/stage3/", theUnits[j], ".rds"))


        out <- tempOut %>%
          bind_rows(prevData, .) %>%
          distinct()

      } else if(length(avail) > 1){
        stop("the nation '", theUnits[j], "' exists several times in the output folder '/adb_tablse/stage3/'.")
      } else {
        out <- tempOut
      }

      saveRDS(object = out, file = paste0(intPaths, "/tables/stage3/", theUnits[j], ".rds"))

      ret <- bind_rows(ret, out)
    }

    if(moveFile){
      file.copy(from = thisInput, to = paste0(intPaths, "/tables/stage2/processed"))
      file.remove(thisInput)
    }

    gc()

  }

  if(!is.null(beep)) beep(beep)
  invisible(ret)

}
