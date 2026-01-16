#' Normalise geometries
#'
#' Harmonise and integrate geometries into a standardised format
#' @param input [`character(1)`][character]\cr path of the file to normalise. If
#'   this is left empty, all files at stage two as subset by \code{pattern} are
#'   chosen.
#' @param thresh [`integerish(1)`][integer]\cr percent value of overlap below
#'   which two geometries (the input and the base) are considered to be the
#'   same. This is required, because often the polygons from different sources,
#'   albeit describing the same territorial unit, aren't completely the same.
#' @param pattern [`character(1)`][character]\cr an optional regular expression.
#'   Only dataset names which match the regular expression will be processed.
#' @param query [`character(1)`][character]\cr part of the SQL query (starting
#'   from
#'   WHERE) used to subset the input geometries, for example \code{"where NAME_0
#'   = 'Estonia'"}. The first part of the query (where the layer is defined) is
#'   derived from the meta-data of the currently handled geometry.
#' @param beep [`integerish(1)`][integer]\cr Number specifying what sound to be
#'   played to signal the user that a point of interaction is reached by the
#'   program, see \code{\link[beepr]{beep}}.
#' @param simplify [`logical(1)`][logical]\cr whether or not to simplify
#'   geometries.
#' @param stringdist [`logical(1)`][logical]\cr whether or not to use string
#'   distance to find matches (should not be used for large datasets/when a
#'   memory error is shown).
#' @param strictMatch [`logical(1)`][logical]\cr whether or not matches are
#'   strict, i.e., there should be clear one-to-one relationships and no changes
#'   in broader concepts.
#' @param verbose [`logical(1)`][logical]\cr be verbose about what is happening
#'   (default \code{FALSE}). Furthermore, you can use
#'   \code{\link{suppressMessages}} to make this function completely silent.
#' @details To normalise geometries, this function proceeds as follows:
#'   \enumerate{ \item Read in \code{input} and extract initial metadata from
#'   the file name. \item In case filters are set, the new geometry is filtered
#'   by those. \item The territorial names are matched with the gazetteer to
#'   harmonise new territorial names (at this step, the function might ask the
#'   user to edit the file 'matching.csv' to align new names with already
#'   harmonised names). \item Loop through every nation potentially included in
#'   the file that shall be processed and carry out the following steps:
#'   \itemize{ \item In case the geometries are provided as a list of simple
#'   feature POLYGONS, they are dissolved into a single MULTIPOLYGON per main
#'   polygon. \item In case the nation to which a geometry belongs has not yet
#'   been created at stage three, the following steps are carried out:
#'   \enumerate{ \item Store the current geometry as basis of the respective
#'   level (the user needs to make sure that all following levels of the same
#'   dataseries are perfectly nested into those parent territories, for example
#'   by using the GADM dataset) } \item In case the nation to which the geometry
#'   belongs has already been created, the following steps are carried out:
#'   \enumerate{ \item Check whether the new geometries have the same coordinate
#'   reference system as the already existing database and re-project the new
#'   geometries if this is not the case. \item Check whether all new geometries
#'   are already exactly matched spatially and stop if that is the case. \item
#'   Check whether the new geometries are all within the already defined
#'   parents, and save those that are not as a new geometry. \item Calculate
#'   spatial overlap and distinguish the geometries into those that overlap with
#'   more and those with less than \code{thresh}. \item For all units that dName
#'   match, copy gazID from the geometries they overlap. \item For all units
#'   that dName not match, rebuild metadata and a new gazID.} \item store the
#'   processed geometry at stage three.} \item Move the geometry to the folder
#'   '/processed', if it is fully processed.}
#' @family normalise functions
#' @return This function harmonises and integrates so far unprocessed geometries
#'   at stage two into stage three of the geospatial database. It produces for
#'   each main polygon (e.g. nation) in the registered geometries a spatial file
#'   of the specified file-type.
#' @examples
#' if(dev.interactive()){
#'   library(sf)
#'
#'   # build the example database
#'   adb_example(until = "regGeometry", path = tempdir())
#'
#'   # normalise all geometries ...
#'   normGeometry(pattern = "estonia")
#'
#'   # ... and check the result
#'   st_layers(paste0(tempdir(), "/geometries/stage3/Estonia.gpkg"))
#'   output <- st_read(paste0(tempdir(), "/geometries/stage3/Estonia.gpkg"))
#' }
#' @importFrom checkmate assertFileExists assertIntegerish assertLogical
#'   assertCharacter assertChoice testFileExists
#' @importFrom ontologics new_source new_mapping get_class get_source
#' @importFrom dplyr filter distinct select mutate rowwise filter_at vars
#'   all_vars pull group_by arrange summarise mutate_if rename n if_else ungroup
#'   across
#' @importFrom rlang sym exprs
#' @importFrom readr read_csv read_rds
#' @importFrom purrr map
#' @importFrom tools file_ext
#' @importFrom sf st_layers read_sf st_write st_join st_buffer st_equals st_sf
#'   st_transform st_crs st_crs<- st_geometry_type st_area st_intersection
#'   st_drivers NA_crs_ st_is_valid st_make_valid st_as_sf st_geometry
#'   st_intersects
#' @importFrom rmapshaper ms_simplify
#' @importFrom stringr str_split_1 str_to_title str_pad str_detect
#' @importFrom tibble as_tibble add_column
#' @importFrom dplyr bind_rows slice lag desc n_distinct left_join right_join
#'   first row_number
#' @importFrom tidyr unite
#' @importFrom tidyselect starts_with all_of
#' @importFrom progress progress_bar
#' @importFrom utils tail head
#' @importFrom stats na.omit
#' @importFrom beepr beep
#' @export

normGeometry <- function(input = NULL, pattern = NULL, query = NULL, thresh = 10,
                         beep = NULL, simplify = FALSE, stringdist = TRUE,
                         strictMatch = FALSE, verbose = FALSE){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))
  gazPath <- paste0(getOption(x = "gazetteer_path"))

  type <- str_split(tail(str_split(string = gazPath, pattern = "/")[[1]], 1), "[.]")[[1]][1]

  # get territorial context
  topClass <- paste0(getOption(x = "gazetteer_top"))
  topUnits <- get_concept(class = topClass, ontology = gazPath) %>%
    arrange(label)
  allClasses <- get_class(ontology = gazPath) %>%
    pull(label)

  if(is.null(input)){
    input <- list.files(path = paste0(intPaths, "/geometries/stage2"), full.names = TRUE, pattern = pattern)
  } else {
    assertFileExists(x = input, access = "r")
  }

  # set internal objects
  moveFile <- TRUE

  # get tables
  inventory <- readRDS(paste0(getOption(x = "adb_path"), "/_meta/inventory.rds"))
  inv_dataseries <- inventory$dataseries
  inv_geometries <- inventory$geometries

  # check validity of arguments
  assertCharacter(x = query, len = 1, null.ok = TRUE)
  assertIntegerish(x = thresh, any.missing = FALSE)
  assertLogical(x = simplify, len = 1)
  assertNames(x = colnames(inv_geometries),
              permutation.of = c("geoID", "datID", "stage2_name", "layer", "label", "ancillary", "stage1_name", "stage1_url", "download_date", "update_frequency", "notes"))
  assertNames(x = colnames(inv_dataseries),
              permutation.of = c("datID", "name", "description", "homepage", "version", "licence_link", "notes"))

  outgSeries <- NULL
  for(i in seq_along(input)){

    thisInput <- input[i]

    # scrutinize file-name (the fields, which are delimited by "_", carry important information)
    pathStr <- str_split(thisInput, "/")[[1]]
    file_name <- pathStr[length(pathStr)]
    fields <- str_split(file_name, "_")[[1]]

    if(!file_name %in% inv_geometries$stage2_name){
      message("\n--- ", i, " / ", length(input), " skipping ", rep("-", times = getOption("width")-(nchar(i)+nchar(length(input))+21+nchar(file_name))), " ", file_name, " ---")
      next
    } else {
      message("\n--- ", i, " / ", length(input), " ", rep("-", times = getOption("width")-(nchar(i)+nchar(length(input))+13+nchar(file_name))), " ", file_name, " ---")
    }

    # open the look-up table for the current file
    gSeries <- inv_geometries[grep(pattern = paste0("^", file_name, "$"), x = inv_geometries$stage2_name),]
    if(file_name %in% gSeries$stage2_name){
      newGID <- gSeries$geoID
      gLayer <- gSeries$layer
      gLabel <- gSeries$label
      gAncill <- gSeries$ancillary
      gIDs <- inv_geometries$geoID[inv_geometries$datID == gSeries$datID]

      # manage dataseries
      dSeries <- inv_dataseries[inv_dataseries$datID == gSeries$datID,]
      dName <- dSeries$name
      if(!dName %in% get_source(ontology = gazPath)$label){
        new_source(name = dName,
                   version = dSeries$version,
                   date = Sys.Date(),
                   description = dSeries$description,
                   homepage = dSeries$homepage,
                   license = dSeries$licence_link,
                   ontology = gazPath)
      }

      # extract class and label ...
      targetClass <- map(.x = gLabel,
                         .f = function(x){
                           temp <- str_split(x, "\\|")[[1]]
                           map(str_split(temp, "="), head, 1)
                         }) %>%
        unlist() %>%
        get_class(label = ., ontology = gazPath)
      parentClass <- allClasses[which(allClasses %in% tail(targetClass$label, 1)) - 1]

      targetLabel <- map(.x = gLabel,
                        .f = function(x){
                          temp <- str_split(x, "\\|")[[1]]
                          map(str_split(temp, "="), tail, 1)
                        }) %>%
        unlist()

      # ... and add them to the ontology
      new_mapping(new = targetLabel, target = targetClass, source = dName,
                  match = "exact", certainty = 3, type = "class",
                  ontology = gazPath)

      territoryCols <- targetClass$label
      filledClasses <- allClasses[which(allClasses %in% head(territoryCols, 1)) : which(allClasses %in% tail(territoryCols, 1))]

      if(territoryCols[1] != topClass){
        theUnits <- str_split(string = gSeries$stage2_name, pattern = "_")[[1]][1]
        if(theUnits == ""){
          theUnits <- NULL
        } else {
          assertSubset(x = theUnits, choices = topUnits$label)
        }
      } else {
        theUnits <- NULL
      }

    } else{
      stop(paste0("  ! the file '", file_name, "' has not been registered yet !"))
    }

    # read the object
    message("\n--> reading new geometries ...")
    if(!is.null(query)){
      moveFile <- FALSE
      input_geom <- read_sf(dsn = thisInput,
                            query = paste0("select * from ", gLayer, " ", query),
                            stringsAsFactors = FALSE)
    } else {
      input_geom <- read_sf(dsn = thisInput,
                            layer = gLayer,
                            stringsAsFactors = FALSE)
    }

    # stop this iteration when 'input_geom' has zero rows
    if(dim(input_geom)[1] == 0){
      message("  ! the file '", file_name, "' doesn't contain any features !")
      next
    }

    for(k in seq_along(territoryCols)){
      names(input_geom)[[which(names(input_geom) == targetLabel[k])]] <- territoryCols[k]
    }

    if(!topClass %in% names(input_geom)){
      input_geom <- input_geom %>%
        add_column(tibble(!!topClass := theUnits), .before = territoryCols[1])
      territoryCols <- c(topClass, territoryCols)
      allCols <- filledClasses
    } else {
      allCols <- territoryCols
    }

    # identify whether the new geometry is first and therefore the geometric
    # basis, or whether something has already been defined at that level
    gazClasses <- get_class(external = TRUE, ontology = gazPath)
    newParent <- gazClasses %>%
      filter(label %in% tail(targetLabel, 1)) %>%
      pull(has_broader)
    testBasis <- gazClasses %>%
      filter(has_broader %in% newParent) %>%
      filter(row_number() == 1)

    if(tail(targetLabel, 1) == testBasis$label & str_detect(testBasis$id, dName)){
      tempCols <- territoryCols
      spatMatch <- FALSE
    } else {
      tempCols <- territoryCols[1]
      spatMatch <- TRUE
    }

    # construct the harmonised names and ID
    harmonised_geom <- .matchOntology(table = input_geom,
                                      columns = tempCols,
                                      dataseries = dName,
                                      ontology = gazPath,
                                      stringdist = stringdist,
                                      strictMatch = strictMatch,
                                      verbose = verbose,
                                      beep = beep) %>%
      mutate(unitCol := !!sym(topClass))

    if(spatMatch){

      if(tail(targetClass$label, 1) != topClass){
        harmonised_geom <- harmonised_geom %>%
          mutate(id = NA_character_) %>%
          mutate(external = paste0(!!sym(tail(territoryCols, 1)), "_-_-", row_number()))
      } else {
        harmonised_geom <- harmonised_geom %>%
          mutate(external = paste0(external, "_-_-", row_number()))
      }

    } else {
      # if geometries are not matched spatially, just ignore all territories that have no match in the ontology
      harmonised_geom <- harmonised_geom %>%
        filter(!is.na(id))
    }

    harmonised_geom <- harmonised_geom %>%
      select(all_of(territoryCols), id, match, external, everything())

    if(is.null(theUnits)){
      theUnits <- unique(eval(expr = parse(text = "unitCol"), envir = harmonised_geom)) %>%
        na.omit() %>%
        as.character()
    }

    if(length(theUnits) == 0){
      moveFile <- FALSE
      message("    ! New geometries not part of subset !")
    } else {

      # then we loop through all nations
      for(j in seq_along(theUnits)){

        tempUnit <- theUnits[j]

        if(is.na(tempUnit)) next
        if(tempUnit == "ignore") next
        message(paste0(" -> processing '", tempUnit, "' ..."))

        if(length(na.omit(theUnits)) != 1){
          new_geom <- harmonised_geom %>%
            filter_at(vars("unitCol"), all_vars(. %in% tempUnit)) %>%
            select(all_of(allCols), id, match, external)
          assertChoice(x = allCols[1], choices = names(new_geom), .var.name = "names(nation_column)")
        } else {
          new_geom <- harmonised_geom %>%
            select(any_of(allCols), id, match, external)
        }

        # in case the object consists of several POLYGONs per unique name, dissolve
        # them into a single MULTIPOLYGON
        if(unique(st_geometry_type(new_geom)) == "POLYGON"){
          uniqueUnits <- new_geom %>%
            as_tibble() %>%
            select(!!territoryCols) %>%
            unique()

          if(all(!is.na(uniqueUnits))){
            if(dim(new_geom)[1] > dim(uniqueUnits)[1]){
              message("    Dissolving multiple polygons into a single multipolygon")

              temp <- new_geom %>%
                select(-c(all_of(territoryCols))) %>%
                st_drop_geometry()

              new_geom <- new_geom %>%
                group_by(across(c(all_of(territoryCols), "id"))) %>%
                mutate(dup = if_else(n() > 1, TRUE, FALSE)) %>%
                group_by(across(c(all_of(territoryCols), "id", "dup"))) %>%
                summarise() %>%
                ungroup() %>%
                mutate(id = if_else(dup, NA_character_, id),
                       across(any_of(territoryCols), ~if_else(dup, NA_character_, .x))) %>%
                left_join(temp, by = "id") %>%
                select(colnames(new_geom))
            }
          } else{
            message("  ! The geometry contains only POLYGON features but no unique names to summarise them.")
          }
        }

        # in case the object is not fully valid (e.g., degenerate edges), make
        # it valid
        if(!all(st_is_valid(new_geom))){
          new_geom <- st_make_valid(x = new_geom)
        }

        # determine whether a geometry with the nation as name already exists and
        # whether that contains the correct layer ...
        fileExists <- testFileExists(x = paste0(intPaths, "/geometries/stage3/", tempUnit, ".gpkg"))
        if(fileExists){
          targetLayers <- st_layers(dsn = paste0(intPaths, "/geometries/stage3/", tempUnit, ".gpkg"))
          if(!grepl(pattern = tail(targetClass$label, 1), x = paste0(targetLayers$name, collapse = "|"))){
            fileExists <- FALSE
          }
        }

        # ... if yes, read it in, otherwise create it
        if(fileExists){

          # read target geoms ----
          message("    Reading target geometries")
          topLayer <- targetLayers$name[which.min(targetLayers$features)]

          target_geom <- read_sf(dsn = paste0(intPaths, "/geometries/stage3/", tempUnit, ".gpkg"),
                                 layer = tail(targetClass$label, 1),
                                 stringsAsFactors = FALSE)

          # determine the size of the sibling group
          target_geom <- target_geom %>%
            rowwise() %>%
            mutate(siblings = paste0(head(str_split_1(gazName, "[.]"), -1), collapse = ".")) %>%
            group_by(siblings) %>%
            mutate(siblings = n()) %>%
            ungroup()

          if(st_crs(target_geom)$input == "Undefined Cartesian SRS"){
            st_crs(target_geom) <- NA_crs_
          }

          # reproject new geom ----
          if(st_crs(new_geom) != st_crs(target_geom)){
            message("    Reprojecting new geometries")
            new_geom <- st_transform(x = new_geom, crs = st_crs(target_geom))
          }

          # test whether/which of the new features are already (spatially) in the target
          # geom and stop if all of them are there already.
          message("    Checking for exact spatial matches")
          equals <- unlist(st_equals(new_geom, target_geom))
          if(length(equals) == dim(new_geom)[1]){
            message("  ! --> all features of the new geometry are already part of the target geometry !")
            next
          }

          # determine geoms that are already ok ... ----
          message("    Simplifying/validating geometries")
          # !!!!!! perhaps include here a more sophisticated system for versioning in case there are several external geometries read in (also see line 441) !!!!!!
          base_geom <- target_geom %>%
            filter(geoID == target_geom$geoID[1])

          # simplify and store in separate object
          if(simplify){
            new_geom_simple <- new_geom %>%
              ms_simplify(keep = 0.5, method = "dp", keep_shapes = TRUE) %>%
              st_make_valid()

            base_geom_simple <- base_geom %>%
              ms_simplify(keep = 0.5, method = "dp", keep_shapes = TRUE) %>%
              st_make_valid()
          } else {
            new_geom_simple <- new_geom
            base_geom_simple <- base_geom
          }

          # calculate area and bind in original geom
          base_geom <- base_geom_simple %>%
            mutate(base_area = as.numeric(st_area(.))) %>%
            select(base_ID = gazID, base_name = gazName, base_class = gazClass, base_geoID = geoID, base_siblings = siblings, base_area)

          new_geom <- new_geom_simple %>%
            mutate(new_area = as.numeric(st_area(.))) %>%
            bind_cols(new_geom = st_geometry(new_geom))

          message("    Joining target and source geometries")

          # ... and then carrying out an intersection
          message("    -> Calculating intersection")
          geom_intersect <- st_intersects(x = base_geom, y = new_geom)
          pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = dim(base_geom)[1])
          geom_overlap <- suppressWarnings(
            map_dfr(1:dim(base_geom)[1], function(ix){
              pb$tick()
              st_intersection(x = base_geom[ix,], y = new_geom[geom_intersect[[ix]],],
                              dimensions = "polygon")
            })) %>%
            st_make_valid() %>%
            mutate(intersect_area = as.numeric(st_area(.)),
                   base_prop = round(intersect_area/base_area*100, 5),
                   new_prop = round(intersect_area/new_area*100, 5))

          message("    -> Determining matches")
          temp_geom <- geom_overlap %>%
            mutate(outString = paste0(round(new_prop), "<>", round(base_prop)),
                   outMatch = if_else(new_prop >= (100 - thresh),
                                      if_else(base_prop >= (100 - thresh),
                                              "close",
                                              "narrower"),
                                      if_else(new_prop < (100 - thresh) & new_prop >= thresh,
                                              "broader",
                                              if_else(new_prop < thresh,
                                                      if_else(base_prop >= (100 - thresh),
                                                              "broader",
                                                              if_else(base_prop >= thresh,
                                                                      "reassign",
                                                                      "ignore")),
                                                      "ignore")))) %>%
            filter(outMatch != "ignore") %>%
            group_by(base_name) %>%
            mutate(base_overlap = n()) %>%
            arrange(desc(base_prop)) %>%
            filter(outMatch != "reassign" | row_number() == 1L) %>%
            ungroup() %>%
            group_by(external) %>%
            mutate(new_overlap = n()) %>%
            ungroup() %>%
            mutate(outMatch = if_else((outMatch == "narrower" & new_overlap == 1L & base_overlap == 1L) | outMatch == "close",
                                      "close",
                                      if_else(outMatch == "reassign", "broader", outMatch)),
                   match = paste0(outMatch, " [", outString, "_", base_ID, "]"),
                   new_name = if_else((new_overlap != 1L | base_overlap != 1L), TRUE, FALSE))

          message("    -> Building new IDs")
          temp_geom <- temp_geom %>%
            rowwise() %>%
            mutate(gazID = if_else(new_name, paste0(head(str_split_1(base_ID, "[.]"), -1), collapse = "."), base_ID),
                   gazName = if_else(new_name, paste0(head(str_split_1(base_name, "[.]"), -1), collapse = "."), base_name)) %>%
            group_by(external, new_geom, gazID, gazName, new_name, base_siblings) %>%
            summarise(match = paste0(match, collapse = " | ")) %>%
            ungroup()

          # if we are not at the topmost class, read the parent geometry
          if(!topClass %in% tail(targetClass$label, 1)){
            targetParent_geom <- read_sf(dsn = paste0(intPaths, "/geometries/stage3/", tempUnit, ".gpkg"),
                                         layer = allClasses[which(allClasses %in% tail(targetClass$label, 1)) - 1],
                                         stringsAsFactors = FALSE)

            if(!any(targetParent_geom$geoID %in% gIDs)){
              targetParent_geom <- targetParent_geom %>%
                filter(geoID %in% targetParent_geom$geoID[1] & gazClass == parentClass)
            } else {
              targetParent_geom <- targetParent_geom %>%
                filter(geoID %in% gIDs & gazClass == parentClass)
            }

            if(simplify){
              targetParent_geom <- targetParent_geom %>%
                ms_simplify(keep = 0.5, method = "dp", keep_shapes = TRUE)
            }
            targetParent_geom <- targetParent_geom %>%
              st_make_valid() %>%
              mutate(target_area = as.numeric(st_area(.))) %>%
              rowwise() %>%
              mutate(siblings = 0L) %>%
              select(-match, -external)

            message("    -> Adapting IDs to parent level")
            pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = dim(targetParent_geom)[1])
            geom_intersect <- st_intersects(x = targetParent_geom, y = new_geom)
            parentOverlap <- suppressWarnings(
              map_dfr(1:dim(targetParent_geom)[1], function(ix){
                pb$tick()
                st_intersection(x = targetParent_geom[ix,], y = new_geom[geom_intersect[[ix]],])
              })) %>%
              st_make_valid() %>%
              mutate(intersect_area = as.numeric(st_area(.)),
                     target_prop = round(intersect_area/target_area*100, 5),
                     new_prop = round(intersect_area/new_area*100, 5))

            parentOverlap <- parentOverlap |>
              filter(new_prop > thresh) |>
              select(parentID = gazID, parentName = gazName, external) %>%
              st_drop_geometry()
          } else {
            parentOverlap <- tibble(parentID = character(), parentName = character(), external = character())
          }

          # in case new geometries overlap at the parent administrative level,
          # get these information in there and assign the ID
          if(dim(temp_geom)[1] != length(unique(temp_geom$external))){

            temp_geom_clean <- temp_geom %>%
              group_by(external) %>%
              filter(n() == 1)

            # for duplicates, reassign siblings, gazID and gazName according to
            # parent geometries ...
            temp_geom_dups <- temp_geom %>%
              group_by(external) %>%
              filter(n() > 1) %>%
              select(-gazName)

            # ... and summarise according to the external cocepts
            temp_geom <- temp_geom_dups %>%
              left_join(parentOverlap, by = "external") %>%
              group_by(external) %>%
              separate_rows(match, sep = " \\| ") %>%
              mutate(base_siblings = if_else(gazID == parentID, base_siblings, NA_integer_),
                     base_siblings = first(na.omit(base_siblings))) %>%
              ungroup() %>%
              group_by(external, new_geom, new_name, gazID = parentID, gazName = parentName, base_siblings) %>%
              summarise(match = paste0(match, collapse = " | ")) %>%
              bind_rows(temp_geom_clean)

          }

          # after summarizing overlaps, bring in the parent gazID and gazName
          # and reconstruct the full ID and Name
          output_geom <- temp_geom %>%
            left_join(parentOverlap, by = "external") %>%
            separate_wider_delim(cols = external, delim = "_-_-", names = "external", too_many = "drop") %>%
            group_by(parentName, new_name) %>%
            mutate(rn = row_number()) %>%
            ungroup() %>%
            mutate(base_siblings = if_else(is.na(base_siblings) | gazID != parentID, 0, base_siblings),
                   tempID = str_pad(string = rn + base_siblings, width = 3, pad = 0),
                   thisName = external,
                   gazID = if_else(!new_name, gazID, paste0(parentID, ".", tempID)),
                   gazName = if_else(!new_name, gazName, paste0(parentName, ".", thisName)),
                   geoID = newGID,
                   gazClass = tail(allCols, 1)) %>%
          select(gazID, gazName, gazClass, match, external, new_name, geoID, geom = new_geom)

          # update ontology, but only if we are not handling the topmost class,
          # because in that case it has been harmonised with the gazetteer
          # already
          if(unique(output_geom$gazClass) != topClass){

            message("    -> Updating ontology")
            .updateOntology(table = output_geom %>% select(-geom),
                            threshold = thresh,
                            dataseries = dName,
                            ontology = gazPath)

          }

          output_geom <- target_geom %>%
            select(-siblings) %>%
            bind_rows(output_geom %>% select(-new_name)) %>%
            arrange(gazID, geoID)

        } else {

          # if it has been identified that the new geometries need to be matched spatially in the
          # absence of a file/layer, it means that it must be intersected with the parent layer,
          # to derive the correct IDs
          if(spatMatch){
            new_geom <- new_geom %>%
              rename(new_label = !!sym(tail(territoryCols, 1))) %>%
              select(-external, -match, -id, -all_of(head(territoryCols, -1))) %>%
              ms_simplify(keep = 0.5, method = "dp", keep_shapes = TRUE) %>%
              mutate(new_area = as.numeric(st_area(.)))  %>%
              st_make_valid()

            targetParent_geom <- read_sf(dsn = paste0(intPaths, "/geometries/stage3/", tempUnit, ".gpkg"),
                                         layer = allClasses[which(allClasses %in% tail(targetClass$label, 1)) - 1],
                                         stringsAsFactors = FALSE)

            if(!any(targetParent_geom$geoID %in% gIDs)){
              targetParent_geom <- targetParent_geom %>%
                filter(geoID %in% targetParent_geom$geoID[1] & gazClass == parentClass)
            } else {
              targetParent_geom <- targetParent_geom %>%
                filter(geoID %in% gIDs & gazClass == parentClass)
            }
            targetParent_geom <- targetParent_geom %>%
              mutate(target_area = as.numeric(st_area(.)))

            message("    -> Adapting IDs to parent level")
            pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = dim(targetParent_geom)[1])
            geom_intersect <- st_intersects(x = targetParent_geom, y = new_geom)
            parentOverlap <-  suppressWarnings(
              map_dfr(1:dim(targetParent_geom)[1], function(ix){
                pb$tick()
                st_intersection(x = targetParent_geom[ix,], y = new_geom[geom_intersect[[ix]],])
              })) %>%
              st_make_valid() %>%
              mutate(intersect_area = as.numeric(st_area(.)),
                     target_prop = round(intersect_area/target_area*100, 5),
                     new_prop = round(intersect_area/new_area*100, 5))

            new_geom <- parentOverlap %>%
              filter(new_prop > thresh) %>%
              group_by(gazName) %>%
              arrange(new_label) %>%
              mutate(newID = str_pad(string = row_number(), width = 3, pad = 0)) %>%
              ungroup() %>%
              rowwise() %>%
              mutate(gazID = paste0(gazID, ".", newID),
                     !!tail(territoryCols, 1) := new_label,
                     external = new_label,
                     match = "close",
                     new_name = TRUE,
                     gazClass = tail(allCols, 1)) %>%
              separate_wider_delim(cols = gazName, delim = ".", names = head(filledClasses, -1)) %>%
              select(all_of(filledClasses), gazID, match, external, geom, new_name, gazClass) %>%
              arrange(gazID)

            message("    -> Updating ontology")
            .updateOntology(table = new_geom %>% st_drop_geometry() %>% select(-geom),
                            threshold = thresh,
                            dataseries = dName,
                            ontology = gazPath)

            message("    Creating new basis dataset for class ", tail(targetClass$label, 1), ".")
            output_geom <- suppressMessages(
              new_geom %>%
                unite(col = "gazName", all_of(filledClasses), sep = ".") %>%
                mutate(gazClass = tail(targetClass$label, 1),
                       geoID = newGID) %>%
                st_sf() %>%
                select(gazID, gazName, gazClass, match, external, geoID))

          } else {

            message("    Creating new basis dataset for class ", tail(targetClass$label, 1), ".")
            output_geom <- suppressMessages(
              new_geom %>%
                unite(col = "gazName", all_of(filledClasses), sep = ".") %>%
                mutate(gazClass = tail(targetClass$label, 1),
                       geoID = newGID) %>%
                st_sf() %>%
                select(gazID = id, gazName, gazClass, match, external, geoID))
          }

        }

        st_write(obj = output_geom,
                 dsn = paste0(intPaths, "/geometries/stage3/", tempUnit, ".gpkg"),
                 layer = tail(targetClass$label, 1),
                 append = FALSE,
                 quiet = TRUE)

      }
    }


    if(moveFile){
      message(paste0("    Moving '", file_name, "' to './stage2/processed'"))
      firstStage <- paste0(intPaths, "/geometries/stage2")
      file.copy(from = paste0(firstStage, "/", file_name), to = paste0(firstStage, "/processed/", file_name))
      file.remove(paste0(firstStage, "/", file_name))
    }

    outgSeries <- bind_rows(outgSeries, gSeries)

  }

  gc()

  if(!is.null(beep)) beep(beep)
  message()
  return(outgSeries)
}
