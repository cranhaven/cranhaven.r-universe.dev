#' Get the column types of a tibble
#'
#' (internal function not for user interaction)
#' @param input [data.frame][data.frame]\cr table from which to get column
#'   types.
#' @importFrom checkmate assertDataFrame
#' @importFrom tibble tibble
#' @importFrom dplyr summarise_all left_join pull
#' @importFrom tidyr gather
#' @importFrom stringr str_c

.getColTypes <- function(input = NULL){

  assertDataFrame(x = input)

  types <- tibble(col_type = c("character", "integer", "numeric", "double", "logical", "Date"),
                  code = c("c", "i", "n", "d", "l", "D"))

  out <- input %>%
    summarise_all(class) %>%
    gather(col_name, col_type) %>%
    left_join(y = types, by = "col_type") %>%
    pull("code") %>%
    str_c(collapse = "")

  return(out)

}


#' Match target terms with an ontology
#'
#' This function takes a table to replace the values of various columns with
#' harmonised values listed in the project specific gazetteer.
#' @param table [`data.frame(1)`][data.frame]\cr a table that contains columns
#'   that should be harmonised by matching with the gazetteer.
#' @param columns [`character(1)`][character]\cr the columns containing the
#'   concepts
#' @param dataseries [`character(1)`][character]\cr the source dataseries from
#'   which territories are sourced.
#' @param ontology [`onto`][ontologics::onto]\cr path where the ontology/gazetteer is stored.
#' @param beep [`integerish(1)`][integer]\cr Number specifying what sound to be
#'   played to signal the user that a point of interaction is reached by the
#'   program, see \code{\link[beepr]{beep}}.
#' @param colsAsClass [`logical(1)`][logical]\cr whether to match \code{columns}
#'   by their name with the respective classes, or with concepts of all classes.
#' @param groupMatches [`logical(1)`][logical]\cr whether or not to group
#'   harmonized concepts when there are more than one match (for example for
#'   broader or narrower matches).
#' @param stringdist [`logical(1)`][logical]\cr whether or not to use string
#'   distance to find matches (should not be used for large datasets/when a
#'   memory error is shown).
#' @param strictMatch [`logical(1)`][logical]\cr whether or not matches are
#'   strict, i.e., there should be clear one-to-one relationships and no changes
#'   in broader concepts.
#' @param verbose [`logical(1)`][logical]\cr whether or not to give detailed
#'   information on the process of this function.
#' @return Returns a table that resembles the input table where the target
#'   columns were translated according to the provided ontology.
#' @importFrom checkmate assertDataFrame assertCharacter assertIntegerish
#'   assertLogical
#' @importFrom utils head
#' @importFrom ontologics load_ontology new_source get_concept new_mapping
#'   make_tree
#' @importFrom purrr map_dfr
#' @importFrom dplyr pull filter select mutate distinct bind_cols rename
#'   everything left_join rename_with na_if anti_join
#' @importFrom tibble tibble
#' @importFrom tidyselect all_of any_of where
#' @importFrom stringr str_split_i str_replace str_replace_all
#' @importFrom tidyr separate_rows separate pivot_wider fill pivot_longer
#'   separate_longer_delim separate_wider_delim contains
#' @importFrom sf st_drop_geometry
#' @export

.matchOntology <- function(table = NULL, columns = NULL, dataseries = NULL,
                           ontology = NULL, beep = NULL, colsAsClass = TRUE,
                           groupMatches = FALSE, stringdist = TRUE, strictMatch = FALSE,
                           verbose = FALSE){

  assertDataFrame(x = table, min.cols = length(columns))
  assertCharacter(x = columns, any.missing = FALSE)
  assertCharacter(x = dataseries, len = 1, any.missing = FALSE)
  assertIntegerish(x = beep, len = 1, lower = 1, upper = 11, null.ok = TRUE)
  assertLogical(x = colsAsClass, len = 1, any.missing = FALSE)
  assertLogical(x = groupMatches, len = 1, any.missing = FALSE)
  assertLogical(x = verbose, len = 1, any.missing = FALSE)

  # set internal paths
  ontoPath <- ontology
  ontoMatching <- str_split(string = ontoPath, pattern = "[.]")[[1]][1]
  if(!testDirectoryExists(ontoMatching)){
    dir.create(ontoMatching)
  }

  allCols <- get_class(ontology = ontoPath) %>%
    pull(label)

  if(colsAsClass){
    assertSubset(x = head(columns, 1), choices = allCols)
    allCols <- allCols[which(allCols %in% head(columns, 1)) : which(allCols %in% tail(columns, 1))]
    withClass <- "class"
  } else {
    allCols <- columns
    withClass <- NULL
  }

  # remove white-space and dots
  table <- table %>%
    mutate(across(where(is.character),
                  function(x){
                    temp <- trimws(x)
                    str_replace_all(string = temp, pattern = "[.]", replacement = "")
                  }))

  # fill from left to right
  fixParent <- NULL
  for(i in seq_along(allCols)){
    if(i == 1) next
    if(!allCols[i] %in% names(table)){
      table <- add_column(.data = table, !!allCols[i] := NA_character_, .after = allCols[i-1])
      fixParent <- c(fixParent, allCols[i])
    }
    table <- table %>%
      mutate(!!allCols[i] := if_else(is.na(!!sym(allCols[i])), !!sym(allCols[i-1]), !!sym(allCols[i])))
  }
  toOut <- table

  type <- str_split(tail(str_split(string = ontoPath, pattern = "/")[[1]], 1), "[.]")[[1]][1]

  # make a new dataseries, in case it doesn't exist yet
  if(!dataseries %in% get_source(ontology = ontoPath)$label){
    new_source(name = dataseries, date = Sys.Date(),
               ontology = ontoPath)
  }

  # get the current source
  srcID <- get_source(label = dataseries, ontology = ontoPath) %>%
    pull(id)

  # prepare object to write into
  if(inherits(x = table, what = "sf")){
    tab <- table %>%
      st_drop_geometry()
    remakeSF <- TRUE
  } else {
    tab <- table
    remakeSF <- FALSE
  }

  for(i in seq_along(allCols)){

    # extract the target column and its parents
    tempTab <- tab %>%
      distinct(across(all_of(allCols[1:i]))) %>%
      filter(!is.na(!!sym(tail(allCols[1:i], 1))))

    parentLen <- get_class(ontology = ontoPath) %>%
      filter(label == allCols[i]) %>%
      pull(id)

    if(length(parentLen) != 0){
      parentLen <- length(str_split(parentLen, "[.]")[[1]])-1
    } else {
      parentLen <- 0
    }

    if(allCols[i] %in% fixParent){

      tempConcepts <- tempTab %>%
        left_join(newConcepts, by = allCols[1:(i-1)]) %>%
        select(label = allCols[i], external = allCols[i], has_broader = id) |>
        mutate(id = NA_character_)

    } else {

      # identify whether concepts were already defined as external concepts...
      if(i == 1){

        tempTab <- tempTab %>%
          select(label = allCols[i])

        externalConcepts <- get_concept(label = tempTab$label, has_source = srcID,
                                        external = TRUE, ontology = ontoPath) %>%
          mutate(len = lengths(str_split(has_broader, "[.]"))) %>%
          filter(len == parentLen) %>%
          select(-len) %>%
          left_join(tibble(label = tempTab$label), ., by = "label") %>%
          mutate(class = allCols[i],
                 has_source = srcID)

      } else {

        # first, transform the parents into the column 'has_broader' ...
        tempTab <- tempTab %>%
          left_join(newConcepts, by = allCols[1:(i-1)]) %>%
          select(label = allCols[i], has_broader = id)

        # ... then search for the concepts
        externalConcepts <- get_concept(label = tempTab$label, has_source = srcID,
                                        has_broader = tempTab$has_broader,
                                        external = TRUE, ontology = ontoPath) %>%
          left_join(tempTab |> select(label, has_broader), ., by = c("label", "has_broader")) %>%
          mutate(class = allCols[i],
                 has_source = srcID)

      }

      # if not all external concepts have an ID, edit those that don't have one
      toMatch <- externalConcepts %>%
        filter(is.na(id))
      matches <- externalConcepts %>%
        filter(!is.na(id))

      # modify 'toMatch' by inserting concepts that occur at another parent (in the same dataseries)
      if(dim(toMatch)[1] != 0){
        diffParent <- get_concept(label = toMatch$label, has_source = srcID,
                                  external = TRUE, ontology = ontoPath) |>
          mutate(len = lengths(str_split(has_broader, "[.]"))) %>%
          filter(len == parentLen) %>%
          select(-len) %>%
          mutate(class = allCols[i])

        toMatch <- toMatch |>
          anti_join(diffParent, by = c("label", "has_source", "class"))
        matches <- matches |>
          bind_rows(diffParent)

      } else {
        diffParent <- NULL
      }

      # set new matches manually, if there are any to match
      if(dim(toMatch)[1] != 0){

        newMatches <- .editMatches(new = toMatch,
                                   topLevel = if_else(i == 1, TRUE, FALSE),
                                   source = dataseries,
                                   ontology = ontology,
                                   matchDir = paste0(ontoMatching, "/"),
                                   stringdist = stringdist,
                                   verbose = verbose,
                                   beep = beep)

        # set the mappings to the newly identified concepts, but only if they are well known
        newMappings <- newMatches |>
          filter(!is.na(match)) |>
          filter(label != "ignore")

        if(dim(newMappings)[1] != 0){
          new_mapping(new = newMappings$external,
                      target = newMappings %>% select(id, label, class, has_broader),
                      source = dataseries,
                      match = newMappings$match,
                      certainty = 3,
                      ontology = ontoPath,
                      verbose = verbose,
                      beep = beep)
        }

        tempConcepts <- matches |>
          select(label, has_broader) |>
          bind_rows(newMappings |> select(label = external, has_broader)) |>
          arrange(label)

      } else {

        newMappings <- NULL
        tempConcepts <- matches |>
          select(label, has_broader) |>
          arrange(label)

      }

      # in case concepts were matched in another parent, those parents need to be corrected in 'newConcepts'
      if(i != 1 & !strictMatch){

        if(!is.null(diffParent)){
          newMappings <- diffParent |>
            mutate(external = label) |>
            bind_rows(newMappings)
        }

        if(!is.null(newMappings)){

          if(any(is.na(newMappings$has_broader))) stop("NA in 'newMappings' needs a fix")

          parentMappings <- newMappings |>
            rename(has_new_broader = has_broader) |>
            left_join(externalConcepts |> select(external = label, has_broader), by = c("external")) |>
            filter(!is.na(has_broader))

          if(any(parentMappings$has_new_broader != parentMappings$has_broader)){
            message("-------> new parents when matching <------- ")

            tempTab_broader <- make_tree(id = parentMappings$has_new_broader, ontology = ontoPath, reverse = TRUE) |>
              filter(class %in% allCols) |>
              pivot_wider(names_from = class, values_from = label) |>
              fill(allCols[1:(i-1)]) |>
              filter(if_any(allCols[i-1], ~ !is.na(.))) |>
              unite(col = "new_label", any_of(allCols), sep = "][", remove = FALSE) |>
              select(has_new_broader = id, new_label)

            newConcepts <- parentMappings |>
              left_join(tempTab_broader, by = "has_new_broader") |>
              left_join(newConcepts |> select(has_broader = id, any_of(allCols)) |> distinct(), by = "has_broader") |>
              select(id = has_new_broader, new_label, any_of(allCols)) |>
              distinct() |>
              bind_rows(newConcepts)

          }

        }
      }

      # ... and query the ontology again, this should now include the newly created
      # concepts as well (except those that were to be ignored)
      if(i == 1){

        externalConcepts <- get_concept(label = tempConcepts$label, has_source = srcID,
                                        external = TRUE, ontology = ontoPath)

        if(colsAsClass){

          externalConcepts <- externalConcepts %>%
            mutate(len = lengths(str_split(has_broader, "[.]"))) %>%
            filter(len == parentLen) |>
            left_join(tibble(label = tempConcepts$label), ., by = "label") %>%
            mutate(class = allCols[i])

        }

      } else {
        externalConcepts <- get_concept(label = tempConcepts$label, has_source = srcID,
                                        has_broader = tempConcepts$has_broader,
                                        external = TRUE, ontology = ontoPath) %>%
          left_join(tempConcepts |> select(label, has_broader), ., by = c("label", "has_broader")) %>%
          mutate(class = allCols[i])
      }

      if(dim(tempConcepts)[1] != 0){

        # search where the external concepts have been matched to and pair them up
        tempConcepts <- get_concept(str_detect(has_close_match, paste0(externalConcepts$id, collapse = "|")) |
                                      str_detect(has_broader_match, paste0(externalConcepts$id, collapse = "|")) |
                                      str_detect(has_narrower_match, paste0(externalConcepts$id, collapse = "|")) |
                                      str_detect(has_exact_match, paste0(externalConcepts$id, collapse = "|")),
                                    ontology = ontoPath) %>%
          pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                       names_to = "match", values_to = "external") %>%
          separate_longer_delim(cols = external, delim = " | ") %>%
          filter(!is.na(external)) %>%
          mutate(externalID = str_split_i(external, "[.]", 1),
                 match = str_replace(string = match, pattern = "has_", replacement = ""),
                 match = str_replace(string = match, pattern = "_match", replacement = "")) %>%
          select(-external) %>%
          distinct() %>%
          left_join(externalConcepts %>% select(externalID = id, external = label), by = "externalID") %>%
          filter(!is.na(external)) %>%
          filter(match != "exact") %>%
          select(-externalID)

      } else {

        tempConcepts <- tibble(id = character(), label = character(), description = character(), class = character(),
                               has_broader = character(), match = character(), external = character())

      }

    }

    if(i == 1){
      newConcepts <- tempConcepts %>%
        rename(!!allCols[i] := external, new_label = label)
    } else {
      newConcepts <- tempConcepts %>%
        rename(!!allCols[i] := external) %>%
        left_join(newConcepts %>% select(has_broader = id, any_of(allCols), new_label) |> distinct(), by = "has_broader") %>%
        unite(col = "new_label", new_label, label, sep = "][", remove = TRUE)
    }

  }

  # ... to join them to the input table
  toOut <- table %>%
    select(-any_of("id")) %>%
    unite(col = "external", all_of(allCols), sep = "][", remove = FALSE) %>%
    left_join(newConcepts, by = allCols, relationship = "many-to-many") %>%
    select(-all_of(allCols)) %>%
    separate_wider_delim(cols = new_label, delim = "][", names = allCols)

  if(remakeSF){
    toOut <- toOut %>%
      st_sf()
  }

  if(groupMatches){
    matchCols <- c(allCols, "id", "match", "external", "has_broader", "class", "description")
    toOut <- toOut %>%
      group_by(across(-matchCols)) %>%
      summarise(across(.cols = matchCols, .fns = ~paste0(.x, collapse = " | ")))
  }

  out <- toOut %>%
    select(all_of(allCols), id, match, external, has_broader, class, description, everything())

  return(out)

}


#' Edit matches manually in a csv-table
#'
#' Allows the user to match concepts with an already existing ontology, without
#' actually writing into the ontology, but instead storing the resulting
#' matching table as csv.
#' @param new [`data.frame(.)`][data.frame]\cr the new concepts that shall be
#'   manually matched, includes "label", "class" and "has_broader" columns.
#' @param topLevel [`logical(1)`][logical]\cr whether or not the new concepts
#'   are at the highest level only, i.e., have to be matched without context, or
#'   whether they are contain columns that must be matched within parent
#'   columns.
#' @param source [`character(1)`][character]\cr any character uniquely
#'   identifying the source dataset of the new concepts.
#' @param ontology [`ontology(1)`][list]\cr either a path where the ontology is
#'   stored, or an already loaded ontology.
#' @param matchDir [`character(1)`][character]\cr the directory where to store
#'   source-specific matching tables.
#' @param stringdist [`logical(1)`][logical]\cr whether or not to use string
#'   distance to find matches (should not be used for large datasets/when a
#'   memory error is shown).
#' @param verbose [`logical(1)`][logical]\cr whether or not to give detailed
#'   information on the process of this function.
#' @param beep [`integerish(1)`][integer]\cr Number specifying what sound to be
#'   played to signal the user that a point of interaction is reached by the
#'   program, see \code{\link[beepr]{beep}}.
#' @details In order to match new concepts into an already existing ontology, it
#'   may become necessary to carry out manual matches of the new concepts with
#'   already harmonised concepts, for example, when the new concepts are
#'   described with terms that are not yet in the ontology. This function puts
#'   together a table, in which the user would edit matches by hand. Whith the
#'   argument \code{verbose = TRUE}, detailed information about the edit process
#'   are shown to the user. After defining matches, and even if not all
#'   necessary matches are finished, the function stores a specific "matching
#'   table" with the name \emph{match_SOURCE.csv} in the respective directory
#'   (\code{matchDir}), from where work can be picked up and continued at
#'   another time.
#'
#'   Fuzzy matching is carried out and matches with 0, 1 or 2 differing
#'   charcters are presented in a respective column.
#' @return A table that contains all new matches, or if none of the new concepts
#'   weren't already in the ontology, a table of the already sucessful matches.
#' @importFrom checkmate assertDataFrame assertNames assertCharacter
#'   assertFileExists testFileExists assertDirectoryExists
#' @importFrom utils tail head
#' @importFrom ontologics load_ontology get_concept
#' @importFrom stringr str_split str_extract
#' @importFrom readr read_csv write_csv cols
#' @importFrom dplyr filter rename full_join mutate if_else select left_join
#'   bind_rows distinct arrange any_of if_any
#' @importFrom tidyselect everything starts_with
#' @importFrom tidyr pivot_longer pivot_wider separate_wider_delim
#'   separate_wider_regex
#' @importFrom tibble add_column
#' @importFrom fuzzyjoin stringdist_left_join
#' @export

.editMatches <- function(new, topLevel, source = NULL, ontology = NULL,
                         matchDir = NULL, stringdist = TRUE, verbose = TRUE,
                         beep = NULL){

  assertDataFrame(x = new)
  assertNames(x = names(new), must.include = c("label", "class", "has_broader"))
  assertLogical(x = topLevel, any.missing = FALSE, len = 1)
  assertCharacter(x = source, len = 1, any.missing = FALSE)
  assertDirectoryExists(x = matchDir, access = "rw")

  sourceFile <- paste0("match_", source, ".rds")
  sourceID <- get_source(label = source, ontology = ontology) %>%
    pull(id)

  if(inherits(x = ontology, what = "onto")){
    ontoPath <- NULL
  } else {
    assertFileExists(x = ontology, access = "rw", extension = "rds")
    ontoPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    ontology <- load_ontology(path = ontoPath)
  }

  # get the classes within which to search
  filterClasses <- ontology@classes$harmonised %>%
    filter(label %in% new$class)

  if(topLevel){
    joinCols <- c("label", "class")
  } else {
    joinCols <- c("label", "class", "has_broader")
  }

  if(dim(filterClasses)[1] != 0){

    filterClassLevel <- length(str_split(string = filterClasses$id, pattern = "[.]")[[1]])
    if(dim(filterClasses)[1] == 0){
      stop("no classes are matched in the ontology.")
    }
    while(!any(is.na(filterClasses$has_broader))){
      filterClasses <- ontology@classes$harmonised %>%
        filter(label %in% filterClasses$label | label %in% filterClasses$has_broader)
    }
    filterClasses <- filterClasses %>%
      pull(label) %>%
      unique()

    # identify level of the target concepts
    new <- new %>%
      mutate(lvl = length(str_split(has_broader, "[.]")[[1]]))

    if(any(ontology@classes$harmonised$label %in% getOption("gazetteer_top"))){
      topLength <- length(str_split(string = ontology@classes$harmonised |> filter(label %in% getOption("gazetteer_top")) |> pull(id), pattern = "[.]")[[1]])
      new <- new %>%
        mutate(top = str_extract(string = has_broader, pattern = paste0("(.[[:digit:]]+){", topLength-1, "}")))

      parentFilter <- unique(new$top)
      withBroader <- NULL
    } else {

      # set a filter in case target concepts have broader concepts
      if(all(new$lvl <= filterClassLevel-1)){
        parentFilter <- unique(new$has_broader)
        withBroader <- NULL
      } else {
        parentFilter <- NA
        withBroader <- "has_broader"
      }

    }

    ignoreClass <- tail(filterClasses, 1)
  } else {
    filterClasses <- get_class(ontology = ontology) %>%
      pull(label)
    parentFilter <- NA
    withBroader <- NULL
    ignoreClass <- head(filterClasses, 1)
  }

  # ontoMatches <- get_concept(label = new$label, class = new$class, has_broader = new$has_broader, ontology = ontology) %>%
  #   rename(harmLab = label) %>%
  #   pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
  #                names_to = "match", values_to = "label")

  # determine previous matches from matching table (and make them long)
  if(testFileExists(paste0(matchDir, sourceFile))){
    dsMatches <- readRDS(file = paste0(matchDir, sourceFile))
  } else {
    dsMatches <- tibble(id = character(), label = character(), class = character(), has_broader = character(), description = character(),
                        has_broader_match = character(), has_close_match = character(), has_exact_match = character(), has_narrower_match = character())
  }

  dsMatchesLong <- dsMatches %>%
    filter(class %in% filterClasses) %>%
    rename(harmLab = label) %>%
    pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                 names_to = "match", values_to = "label") %>%
    separate_longer_delim(cols = label, delim = " | ") |>
    filter(!is.na(label))

  # ignore concepts that were previously tagged 'ignore'
  tempIgnore <- dsMatchesLong |>
    filter(harmLab == "ignore") |>
    select(label, class, harmLab) |>
    distinct()

  new <- new |>
    left_join(tempIgnore, by = c("label", "class")) |>
    filter(!harmLab %in% "ignore") |>
    filter(label != "") |>
    distinct() |>
    select(-harmLab)

  # ... and return an empty object if everything is to ignore
  if(dim(new)[1] == 0){
    out <- tibble(id = character(), has_broader = character(), label = character(), class = character(),
                  description = character(), match = character(), external = character())
    return(out)
  }

  # gather all concepts for the focal data-series (previous matches from
  # matching table and matches that may already be in the ontology) and join
  # with new concepts
  dsConcepts <- dsMatchesLong %>%
    full_join(new |> select(all_of(joinCols)), by = joinCols) %>%
    mutate(harmLab = if_else(is.na(harmLab), label, harmLab),
           label = if_else(is.na(match), if_else(!is.na(id), label, NA_character_), label),
           match = if_else(is.na(match), if_else(!is.na(id), "has_close_match", "sort_in"), match)) %>%
    pivot_wider(id_cols = c(harmLab, class, id, has_broader, description), names_from = match,
                values_from = label, values_fn = ~paste0(na.omit(.x), collapse = " | ")) %>%
    mutate(across(where(is.character), ~na_if(x = ., y = ""))) %>%
    filter(harmLab != "ignore") %>%
    rename(label = harmLab)

  if("sort_in" %in% colnames(dsConcepts)){
    dsConcepts <- dsConcepts %>%
      select(-sort_in)
  }
  if(!"has_broader_match" %in% colnames(dsConcepts)){
    dsConcepts <- dsConcepts %>%
      add_column(has_broader_match = NA_character_, .after = "description")
  }
  if(!"has_close_match" %in% colnames(dsConcepts)){
    dsConcepts <- dsConcepts %>%
      add_column(has_close_match = NA_character_, .after = "has_broader_match")
  }
  if(!"has_exact_match" %in% colnames(dsConcepts)){
    dsConcepts <- dsConcepts %>%
      add_column(has_exact_match = NA_character_, .after = "has_close_match")
  }
  if(!"has_narrower_match" %in% colnames(dsConcepts)){
    dsConcepts <- dsConcepts %>%
      add_column(has_narrower_match = NA_character_, .after = "has_exact_match")
  }
  dsConcepts <- dsConcepts |>
    select(label, class, id, has_broader, description, has_broader_match, has_close_match, has_exact_match, has_narrower_match)

  # ... and determine which are already included concepts and which are still missing
  inclConcepts <- dsConcepts %>%
    filter(!is.na(id))
  missingConcepts <- dsConcepts %>%
    filter(is.na(id))

  if(dim(missingConcepts)[1] != 0){

    toRelate <- ontology@concepts$harmonised
    if(!any(is.na(parentFilter))){
      toRelate <- make_tree(id = parentFilter, ontology = ontology)
    }

    extConcepts <- ontology@concepts$external %>%
      separate_wider_delim(cols = id, names = c("dataseries", "nr"), delim = "_", cols_remove = FALSE) %>%
      rowwise() %>%
      mutate(label = paste0(label, " [", dataseries, "]")) %>%
      select(external = label, temp = id)

    relate <- toRelate %>%
      filter(class %in% filterClasses) %>%
      filter(!label == "ignore") %>%
      pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                   names_to = "match", values_to = "external") %>%
      separate_rows(external, sep = " \\| ") %>%
      separate_wider_delim(cols = external, names = c("temp"), delim = ".", too_many = "drop") %>%
      left_join(extConcepts, by = "temp") %>%
      group_by(across(all_of(c("id", "label", "class", "has_broader", "description", "match")))) %>%
      summarise(external = paste0(na.omit(external), collapse = " | ")) %>%
      ungroup() %>%
      mutate(external = na_if(external, "")) %>%
      pivot_wider(id_cols = c("id", "label", "class", "has_broader", "description"), names_from = match, values_from = external) %>%
      rowwise() %>%
      mutate(description = paste0(na.omit(c(description,
                                            if_else(!is.na(has_close_match), paste0("close: ", has_close_match), NA),
                                            if_else(!is.na(has_broader_match), paste0("broader: ", has_broader_match), NA),
                                            if_else(!is.na(has_narrower_match), paste0("narrower: ", has_narrower_match), NA))),
                                  collapse = " -- ")) %>%
      mutate(description = na_if(description, "")) %>%
      select(id, label, class, has_broader, description) %>%
      left_join(inclConcepts %>% select(-description), by = c("id", "label", "class", "has_broader")) %>%
      distinct(label, class, id, has_broader, .keep_all = TRUE) |>
      filter(!is.na(class))

    toJoin <- relate %>%
      rename(label_harm = label) %>%
      mutate(label = tolower(label_harm)) %>%
      filter(!is.na(label_harm)) %>%
      filter(class == tail(filterClasses, 1)) |>
      # filter(has_broader %in% parentFilter) |>
      select(-has_broader, -has_broader_match, -has_close_match, -has_exact_match, -has_narrower_match)

    tempJoin <- missingConcepts %>%
      select(label_new = label, has_broader) %>%
      mutate(label = tolower(label_new))

    if(stringdist){
      joined <- stringdist_left_join(x = tempJoin, y = toJoin, by = "label", distance_col = "dist", max_dist = 2) |>
        separate_wider_regex(id, c(id_new = ".*", "[.]", rest = ".*"), cols_remove = FALSE) |>
        filter(has_broader == id_new) |>
        select(-id_new, -rest)
    } else {
      joined <- left_join(x = tempJoin, y = toJoin, by = "label") |>
        separate_wider_regex(id, c(id_new = ".*", "[.]", rest = ".*"), cols_remove = FALSE) |>
        filter(has_broader == id_new) |>
        select(-label, -id_new, -rest) |>
        mutate(label.x = NA, label.y = NA, dist = 0)
    }

    if(!all(is.na(joined$dist))){
      joined <- joined %>%
        select(-label.x, -label.y) %>%
        distinct() %>%
        arrange(dist) %>%
        mutate(dist = paste0("dist_", dist)) %>%
        pivot_wider(names_from = dist, values_from = label_harm)

      if(!"dist_0" %in% colnames(joined)){
        joined <- joined %>%
          add_column(dist_0 = NA_character_, .after = "description")
      }
      if(!"dist_1" %in% colnames(joined)){
        joined <- joined %>%
          add_column(dist_1 = NA_character_, .after = "dist_0")
      }
      if(!"dist_2" %in% colnames(joined)){
        joined <- joined %>%
          add_column(dist_2 = NA_character_, .after = "dist_1")
      }

      joined <- joined %>%
        group_by(label_new, id, class, has_broader) %>%
        summarise(across(starts_with("dist_"), ~ paste0(na.omit(unique(.x)), collapse = " | "))) %>%
        mutate(across(where(is.character), function(x) na_if(x, ""))) %>%
        ungroup() %>%
        select(label = label_new, id, class, has_broader, has_0_differences = dist_0, has_1_difference = dist_1, has_2_differences = dist_2)

      hits <- joined %>%
        filter(!is.na(has_0_differences)) %>%
        mutate(has_new_close_match = label,
               label = has_0_differences) %>%
        select(label, id, all_of(withBroader), class, has_new_close_match)

      relate <- relate %>%
        left_join(hits, by = c("id", "label", "class", withBroader)) %>%
        rowwise() %>%
        mutate(has_new_close_match = if_else(has_close_match %in% has_new_close_match, NA_character_, has_new_close_match)) %>%
        unite(col = "has_close_match", has_close_match, has_new_close_match, sep = " | ", na.rm = TRUE) %>%
        mutate(across(where(is.character), function(x) na_if(x, "")))

      stillMissing <- joined %>%
        filter(is.na(has_0_differences)) %>%
        select(-has_broader, -has_0_differences, -id, -class) %>%
        group_by(label) %>%
        summarise(across(starts_with("has_"), ~ paste0(na.omit(unique(.x)), collapse = " | "))) %>%
        mutate(across(where(is.character), function(x) na_if(x, ""))) %>%
        ungroup()

      stillMissing <- missingConcepts %>%
        filter(!label %in% hits$has_new_close_match) %>%
        left_join(stillMissing, by = "label")

    } else {
      stillMissing <- missingConcepts
    }

    sortIn <- stillMissing %>%
      # could also include the class of the stillMissing concepts to clarify
      left_join(tibble(label = unique(new$label)), by = "label") %>%
      mutate(sort_in = label,
             label = NA_character_,
             class = NA_character_) %>%
      select(sort_in, names(attributes), id, has_broader, label, class, everything())

    # put together the object that shall be edited by the user ...
    if(dim(sortIn)[1] != 0){

      sortIn %>%
        bind_rows(relate) %>%
        write_csv(file = paste0(matchDir, "/matching.csv"), quote = "all", na = "")

      if(!is.null(beep)){
        beep(sound = beep)
      }

      message("-> please edit the file '", paste0(matchDir, "/matching.csv"), "' (at class ", tail(filterClasses, 1), ") \n")
      if(verbose){
        message("--- column description ---\n")
        message("sort_in             cut out these values and sort them either into 'has_broader_match', \n                    'has_exact_match', has_narrower_match or 'has_close_match'")
        message("has_broader         the id of the broader concept of the already harmonised concepts")
        message("id                  the id of concepts to which the new terms should be related")
        message("label               concepts to which the new terms should be related")
        message("class               the class of harmonised concepts")
        message("description         the description of each concept")
        message("has_close_match     in case a new concept is a close match to the harmonised concept, paste \n                    it here, delimit several concepts with a '|'")
        message("has_broader_match   in case a new concept is a broader match than the harmonised concept, \n                    paste it here, delimit several concepts with a '|'")
        message("has_narrower_match  in case a new concept is a narrower match than the harmonised concept, \n                    paste it here, delimit several concepts with a '|'")
        message("has_exact_match     in case a new concept is an exact match to the harmonised concept \n                    (which is only the case when it's from the same ontology), paste it \n                    here, delimit several concepts with a '|'")
        message("has_x_differences   in case a new concepts matches via fuzzy matching with any of the already \n                    existing concepts, those concepts are shown in the columns with the \n                    respective number of character differences")
        message("\n--- some useful tips ---")
        message("\n-> values that were already successfully matched by previous translations are listed, \n   however, altering already matched concepts doesn't change the ontology. \n\n-> any row that doesn't contain a value in the column 'id' will be discarded. Hence, \n   if you want a value to be ignored, simply don't paste it anywhere. \n\n-> do not change the values in the columns 'id', 'label' and 'class', as they are \n   important to insert the new matches into the ontology. \n\n-> if a term shall be nested into a position that doesn't have a class, (for example, \n   because that class occurrs the first time with this term) first create that nested \n   class with 'new_class()'.\n")
      }
      done <- readline(" -> press any key when done: ")

      related <- read_csv(paste0(matchDir, "/matching.csv"), col_types = cols(.default = "c"))
      assertNames(x = names(related), must.include = c("sort_in", "has_broader", "id", "label", "class", "description", "has_broader_match", "has_close_match", "has_exact_match", "has_narrower_match"))
      toIgnore <- related %>%
        filter(id == "ignore") %>%
        mutate(label = "ignore",
               class = ignoreClass,
               has_close_match = sort_in,
               id = str_replace_all(ontology@classes$harmonised$id[ontology@classes$harmonised$label == ignoreClass], pattern = "x", replacement = "0")) %>%
        group_by(id, label, class, has_broader_match, has_exact_match, has_narrower_match) %>%
        summarise(has_close_match = paste0(na.omit(has_close_match), collapse = " | "), .groups = "keep") %>%
        ungroup()

      related <- related %>%
        filter(!is.na(id) & id != "ignore") %>%
        select(-sort_in) %>%
        mutate(description = NA_character_) %>%
        select(-any_of(c("has_0_differences", "has_1_difference", "has_2_differences"))) %>%
        bind_rows(toIgnore)

      if(dim(related)[1] == 0){
        related <- NULL
      }

    } else {
      related <- relate %>%
        mutate(description = NA_character_)
    }

  } else {
    related <- inclConcepts
  }

  if(!is.null(related)){

    matchingTable <- dsMatches %>%
      filter(!id == "ignore") %>%
      bind_rows(related) %>%
      pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                   names_to = "match", values_to = "new_label") %>%
      separate_longer_delim(cols = new_label, delim = " | ") %>%
      distinct() %>%
      pivot_wider(id_cols = c("id", "label", "class", "has_broader", "description"), names_from = match,
                  values_from = new_label, values_fn = ~paste0(na.omit(.x), collapse = " | ")) %>%
      mutate(across(where(is.character), ~na_if(x = ., y = ""))) %>%
      filter(!is.na(has_broader_match) | !is.na(has_close_match) | !is.na(has_narrower_match) | !is.na(has_exact_match)) %>%
      filter(!is.na(id)) %>%
      arrange(id)

    saveRDS(object = matchingTable, file = paste0(matchDir, sourceFile))

    # only return objects that fall within those broader concepts that are in
    # the final matching table at the current class
    new_parentFilter <- matchingTable |>
      filter(class == tail(filterClasses, 1)) |>
      distinct(has_broader) |>
      pull(has_broader)

    if(!all(is.na(new_parentFilter))){
      related <- related %>%
        filter(has_broader %in% new_parentFilter)
    }

    newGrep <- str_replace_all(new$label, c("\\(" = "\\\\(", "\\)" = "\\\\)", "\\*" = "\\\\*",
                                            "\\[" = "\\\\[", "\\]" = "\\\\]"))

    out <- related %>%
      pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                   names_to = "match", values_to = "external") %>%
      separate_longer_delim(cols = external, delim = " | ") %>%
      filter(!is.na(external)) |>
      distinct() |>
      mutate(match = str_replace(string = match, pattern = "has_", replacement = ""),
             match = str_replace(string = match, pattern = "_match", replacement = "")) |>
      filter(external %in% new$label)

  }

  return(out)
}


#' Update an ontology
#'
#' This function takes a table (spatial) and updates all territorial concepts in
#' the provided gazetteer.
#' @param table [`character(1)`][character]\cr a table that contains a match
#'   column as the basis to update the gazetteer.
#' @param threshold [`numeric(1)`][numeric]\cr a threshold value above which
#'   matches are updated in the gazetteer.
#' @param dataseries [`character(1)`][character]\cr the source dataseries of the
#'   external concepts for which the gazetteer shall be updated.
#' @param ontology [onto][ontologics::onto]\cr path where the ontology/gazetteer
#'   is stored.
#' @return called for its side-effect of updating a gazetteer
#' @importFrom checkmate assertNumeric assertCharacter
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr rowwise mutate distinct filter select left_join
#' @importFrom stringr str_split_1 str_replace_all
#' @importFrom tidyr separate_rows separate
#' @importFrom ontologics get_concept new_concept new_mapping
#' @export

.updateOntology <- function(table = NULL, threshold = NULL, dataseries = NULL,
                            ontology = NULL){

  assertNumeric(x = threshold, len = 1, lower = 0, upper = 100, any.missing = FALSE)
  assertCharacter(x = dataseries, len = 1, any.missing = FALSE)
  ontoPath <- ontology

  longTable <- table %>%
    rowwise() %>%
    mutate(parentID = paste0(head(str_split_1(gazID, "[.]"), -1), collapse = ".")) %>%
    separate_rows("match", sep = " \\| ") %>%
    mutate(match = str_replace_all(match, "\\[|\\]", "")) %>%
    separate(col = match, into = c("match", "amount"), sep = " ", fill = "right") %>%
    separate(col = amount, into = c("amount", "targetID"), sep = "_", fill = "right") %>%
    separate(col = amount, into = c("source_overlap", "target_overlap"), sep = "<>", fill = "right") %>%
    mutate(source_overlap = suppressWarnings(as.numeric(source_overlap)),
           target_overlap = suppressWarnings(as.numeric(target_overlap))) %>%
    distinct()

  # 1. include new hamonised concepts in the ontology, in case they deviate more than 'threshold'
  newConcepts <- longTable %>%
    filter(new_name)
  newConcepts <- newConcepts %>%
    select(id = parentID, external, gazClass) %>%
    distinct() %>%
    left_join(get_concept(id = newConcepts$parentID, ontology = ontoPath), by = "id") %>%
    select(external, gazClass, id, label, class)

  if(dim(newConcepts)[1] != 0){

    new_concept(new = newConcepts$external,
                broader = newConcepts %>% select(id, label, class),
                description = paste0("external concept originating in the dataseries '", dataseries, "'"),
                class = newConcepts$gazClass,
                ontology =  ontoPath)
    temp <- get_concept(has_broader = newConcepts$id, label = newConcepts$external, str_detect(description, !!dataseries), ontology = ontoPath) %>%
      arrange(label) %>%
      select(id, label, class, has_broader) %>%
      group_by(label, class, has_broader) %>%
      filter(row_number() == 1) %>% # in case an entry is various times in the table, filter only the first one
      ungroup()
    new_mapping(new = sort(newConcepts$external),
                target = temp,
                source = dataseries, match = "close", certainty = 3, type = "concept", ontology = ontoPath)

  }

  # 2. define mappings of the new concepts with harmonised concepts
  harmOnto <- get_concept(id = longTable$targetID, ontology = ontoPath)

  if(dim(harmOnto)[1] != 0){

    if(any(longTable$match == "close")){
      newClose <- longTable %>%
        filter(match == "close")
      newClose <- harmOnto %>%
        filter(id %in% newClose$targetID) %>%
        select(id, label, class, has_broader) %>%
        left_join(newClose %>% select(id = targetID, external), ., by = "id")

      assertCharacter(x = newClose$id, any.missing = FALSE)
      assertCharacter(x = newClose$label, any.missing = FALSE)
      assertCharacter(x = newClose$class, any.missing = FALSE)
      new_mapping(new =  newClose$external,
                  target = newClose %>% select(id, label, class, has_broader),
                  source = dataseries, match = "close", certainty = 3, type = "concept", ontology = ontoPath)
    }

    if(any(longTable$match == "narrower")){
      newNarrower <- longTable %>%
        filter(match == "narrower")
      newNarrower <- harmOnto %>%
        filter(id %in% newNarrower$targetID) %>%
        select(id, label, class, has_broader) %>%
        left_join(newNarrower %>% select(id = targetID, external), ., by = "id")

      assertCharacter(x = newNarrower$id, any.missing = FALSE)
      assertCharacter(x = newNarrower$label, any.missing = FALSE)
      assertCharacter(x = newNarrower$class, any.missing = FALSE)
      new_mapping(new = newNarrower$external,
                  target = newNarrower %>% select(id, label, class, has_broader),
                  source = dataseries, match = "narrower", certainty = 3, type = "concept", ontology = ontoPath)
    }

    if(any(longTable$match == "broader")){
      newBroader <- longTable %>%
        filter(match == "broader")
      newBroader <- harmOnto %>%
        filter(id %in% newBroader$targetID) %>%
        select(id, label, class, has_broader) %>%
        left_join(newBroader %>% select(id = targetID, external), ., by = "id")

      assertCharacter(x = newBroader$id, any.missing = FALSE)
      assertCharacter(x = newBroader$label, any.missing = FALSE)
      assertCharacter(x = newBroader$class, any.missing = FALSE)
      new_mapping(new = newBroader$external,
                  target = newBroader %>% select(id, label, class, has_broader),
                  source = dataseries, match = "broader", certainty = 3, type = "concept", ontology = ontoPath)
    }

  }

}


#' Open file when it's not open
#'
#' @param file a file to store with a delay.
#' @param fun the function to use.
#' @param ... arguments to the \emph{write} functions.
#' @details This function takes the \code{file} and only opens it, if it is not
#' currently already open. In the case it's already open, it prompts the user to
#' confirm when the file is closed (and processed by the other process) and then
#' opens it again.
#' @importFrom checkmate assertChoice

# delayedRead <- function(file, fun = NULL, ...){
#
#   assertChoice(x = fun, choices = c("readRDS", "read_csv"))
#
#   # readRDS()
#   # read_csv()
#
#
#   # https://stackoverflow.com/questions/20038732/how-to-check-a-file-is-opened-or-closed-in-r
#
#
#
# }
