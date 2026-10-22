# locate_event

#' Locate Event
#'
#' @param text Vector of texts to be geolocated.
#' @param landmark_gazetteer `sf` spatial data.frame representing landmarks.
#' @param landmark_gazetteer.name_var Name of variable indicating `name` of landmark.
#' @param landmark_gazetteer.type_var Name of variable indicating `type` of landmark.
#' @param roads `sf` spatial data.frame representing roads.
#' @param roads.name_var Name of variable indicating `name` of road.
#' @param areas `sf` spatial data.frame representing areas, such as administrative areas or neighborhoods.
#' @param areas.name_var Name of variable indicating `name` of area.
#' @param event_words Vector of event words, representing events to be geocoded.
#' @param prepositions_list List of vectors of prepositions. Order of list determines order of preposition precedence. (Default: `list(c("at", "next to","around", "just after", "opposite","opp", "apa", "hapa","happened at", "just before","at the","outside", "right before"), c("near", "after", "toward", "along", "towards", "approach"), c("past","from","on"))`).
#' @param junction_words Vector of junction words to check for when determining intersection of roads. (Default: `c("intersection", "junction")`).
#' @param false_positive_phrases Common words found in text that include spurious location references (eg, __githurai bus__ is the name of a bus, but __githurai__ is also a place). These may be common phrases that should be checked and ignored in the text. (Default: `""`).
#' @param type_list List of vectors of types. Order of list determines order or type precedence. (Default: `NULL`).
#' @param clost_dist_thresh Distance (meters) as to what is considered "close"; for example, when considering whether a landmark is close to a road. (Default: `500`).
#' @param fuzzy_match Whether to implement fuzzy matching of landmarks using levenstein distance. (Default: `TRUE`).
#' @param fuzzy_match.min_word_length Minimum word length to use for fuzzy matching; vector length must be the same as `fuzzy_match.dist`. (Default: `c(5,11)`).
#' @param fuzzy_match.dist Allowable levenstein distances for fuzzy matching; vector length must be same as `fuzzy_match.min_word_length`. (Default: `c(1,2)`).
#' @param fuzzy_match.ngram_max The number of n-grams that should be extracted from text to calculate a levensteing distance against landmarks. For example, if the text is composed of 5 words: w1 w2 w3 w4 and `fuzzy_match.ngram_max = 3`, the function extracts `w1 w2 w3` and compares the levenstein distance to all landmarks. Then in checks `w2 w3 w4`, etc. (Default: `3`).
#' @param fuzzy_match.first_letters_same When implementing a fuzzy match, should the first letter of the original and found word be the same? (Default: `TRUE`).
#' @param fuzzy_match.last_letters_same When implementing a fuzzy match, should the last letter of the original and found word be the same? (Default: `TRUE`).
#' @param quiet If `FALSE`, prints text that is being geocoded. (Default: `TRUE`).
#' @param mc_cores If > 1, uses geolocates events in parallel across multiple cores relying on the `parallel` package. (Default: `1`).
#'
#' @return `sf` spatial dataframe of geolocated events.
#' @examples
#' library(ulex)
#' library(sf)
#'
#' ## Landmarks
#' landmarks_sf <- data.frame(lat = runif(3),
#'                            lon = runif(3),
#'                            name = c("restaurant", "bank", "hotel"),
#'                            type = c("poi", "poi", "poi")) |>
#'   st_as_sf(coords = c("lon", "lat"),
#'            crs = 4326)
#'
#' ## Road
#' coords <- matrix(runif(4), ncol = 2)
#' road_sf <- coords |>
#'   st_linestring() |>
#'   st_sfc(crs = 4326)
#' road_sf <- st_sf(geometry = road_sf)
#' road_sf$name <- "main st"
#'
#' ## Area
#' n <- 5
#' coords <- matrix(runif(2 * n, min = 0, max = 10), ncol = 2)
#' coords <- rbind(coords, coords[1,])
#' polygon <- st_polygon(list(coords))
#' area_sf <- st_sfc(polygon, crs = 4326)
#' area_sf <- st_sf(geometry = area_sf)
#' area_sf$name <- "place"
#'
#' ## Locate Event
#' event_sf <- locate_event(text = "accident near hotel",
#'                          landmark_gazetteer = landmarks_sf,
#'                          roads = road_sf,
#'                          areas = area_sf,
#'                          event_words = c("accident", "crash"))
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import readr
#' @import purrr
#' @import tidytext
#' @import stringr
#' @import stringi
#' @import ngram
#' @import hunspell
#' @import tm
#' @import parallel
#' @import sf
#' @import geodist
#' @import spacyr
#' @import utils
#' @rawNamespace import(stringdist, except = c(extract))
#' @rawNamespace import(raster, except = c(union, intersect, select, extract, tail, stack, head, unstack))
#' @rawNamespace import(quanteda, except = c(stopwords, dictionary))

locate_event <- function(text,
                         landmark_gazetteer,
                         landmark_gazetteer.name_var = "name",
                         landmark_gazetteer.type_var = "type",
                         roads,
                         roads.name_var = "name",
                         areas,
                         areas.name_var = "name",
                         event_words,
                         prepositions_list = list(c("at", "next to","around",
                                                    "just after", "opposite","opp",
                                                    "apa", "hapa","happened at",
                                                    "just before","at the","outside",
                                                    "right before"),
                                                  c("near", "after", "toward",
                                                    "along", "towards", "approach"),
                                                  c("past","from","on")),
                         junction_words = c("intersection", "junction"),
                         false_positive_phrases = "",
                         type_list = NULL,
                         clost_dist_thresh = 500,
                         fuzzy_match = TRUE,
                         fuzzy_match.min_word_length = c(5,11),
                         fuzzy_match.dist = c(1,2),
                         fuzzy_match.ngram_max = 3,
                         fuzzy_match.first_letters_same = TRUE,
                         fuzzy_match.last_letters_same = TRUE,
                         quiet = TRUE,
                         mc_cores = 1){

  ## Defaults
  crs_distance     <- 4326
  crs_out          <- 4326
  quiet_debug      <- T
  prep_check_order <- "prep_then_pattern"

  # 1. Checks ------------------------------------------------------------------
  # Check inputs and output errors if anything is wrong.

  if(is.null(landmark_gazetteer[[landmark_gazetteer.name_var]])){
    stop(paste0(landmark_gazetteer.name_var, " is not a variable in landmark_gazetteer"))
  }

  if(is.null(landmark_gazetteer[[landmark_gazetteer.type_var]])){
    stop(paste0(landmark_gazetteer.type_var, " is not a variable in landmark_gazetteer"))
  }

  if(!(class(roads)[1] %in% c("sf"))){
    stop("roads must be a spatial object")
  }

  if(is.null(roads[[roads.name_var]])){
    stop(paste0(roads.name_var, " is not a variable in roads"))
  }

  if(!(class(areas)[1] %in% c("SpatialPolygonsDataFrame",
                              "sf"))){
    stop("roads must be a SpatialPolygonsDataFrame or an sf object")
  }

  if(is.null(areas[[areas.name_var]])){
    stop(paste0(areas.name_var, " is not a variable in areas"))
  }

  if(!is.list(prepositions_list)) stop("prepositions_list must be a list")

  # 2. Clean/Prep Input Files --------------------------------------------------
  # Cleans and preps gazetteer and road files

  #### Add variables
  landmark_gazetteer$name <- landmark_gazetteer[[landmark_gazetteer.name_var]]
  landmark_gazetteer$type <- landmark_gazetteer[[landmark_gazetteer.type_var]]
  #landmark_gazetteer$general_specific <- landmark_gazetteer[[landmark_gazetteer.gs_var]]

  if(!("name_original" %in% names(landmark_gazetteer))){
    landmark_gazetteer <- landmark_gazetteer %>%
      dplyr::mutate(name_original = .data$name)
  }

  roads$name <- roads[[roads.name_var]]
  areas$name <- areas[[areas.name_var]]

  #### Spatial objects -- make sp

  if(F){
    # The function depends on the coordinate names being lon at lat. Here, use
    # an efficient and hacky way to ensure that. TODO: NEED TO FIX, NOT DO
    # THIS HACKY SOLUTION.

    #landmark_gazetteer_df <- landmark_gazetteer@data
    #landmark_gazetteer_coords <- landmark_gazetteer %>% coordinates() %>% as.data.frame()
    #names(landmark_gazetteer_coords) <- c("lon", "lat")
    #landmark_gazetteer_spdf <- bind_cols(landmark_gazetteer_df, landmark_gazetteer_coords)
    #coordinates(landmark_gazetteer_spdf) <- ~lon+lat
    #crs(landmark_gazetteer_spdf) <- CRS(as.character(landmark_gazetteer@proj4string))
    #landmark_gazetteer <- landmark_gazetteer_spdf
  }

  #### Project Data
  landmark_gazetteer <- st_transform(landmark_gazetteer, crs_distance)
  roads              <- st_transform(roads,              crs_distance)
  areas              <- st_transform(areas,              crs_distance)


  #### Clean Names
  landmark_gazetteer$name <- landmark_gazetteer$name %>%
    str_replace_all("[[:punct:]]", "")

  roads$name <- roads$name %>%
    as.character %>%
    tolower

  #### Preposition List
  # Replace "EVENT_WORD" with words from event_words
  prepositions_list <- lapply(1:length(prepositions_list), function(i){
    preps_i <- prepositions_list[[i]]

    preps_event_i <- preps_i[grepl("EVENT_WORD", preps_i)]
    preps_noevent_i <- preps_i[!grepl("EVENT_WORD", preps_i)]

    preps_event_all_i <- lapply(preps_event_i, function(phrase) str_replace_all(phrase, "EVENT_WORD", event_words)) %>%
      unlist()

    preps_out_i <- c(preps_event_all_i, preps_noevent_i) %>% as.vector()

    return(preps_out_i)
  })

  ## Names into lists
  landmark_list <- landmark_gazetteer$name
  roads_list <- roads$name %>% as.character %>% tolower
  areas_list <- areas$name

  prepositions_all <- prepositions_list %>% unlist()

  ## Add unique ID to gazetteer
  landmark_gazetteer$uid <- 1:nrow(landmark_gazetteer)
  roads$uid              <- 1:nrow(roads)
  areas$uid              <- 1:nrow(areas)

  ## Intersection Words
  junction_words_regex <- paste0("\\b", junction_words, "\\b") %>% paste(collapse = "|")

  # 3. Clean/Prep Text ---------------------------------------------------------
  # Cleans and preps text

  #### Clean text
  text <- text %>%

    iconv("latin1", "ASCII", sub="") %>%

    str_to_lower %>%
    str_replace_all("\\br/about\\b", "round about") %>%

    # Remove select stopwords
    str_replace_all("\\bthe\\b", " ") %>%
    str_replace_all("\\ba\\b", " ") %>%

    str_replace_all("-", " ") %>%
    str_replace_all("\\.", " ") %>%
    str_replace_all("via @[a-z_,A-Z_,0-9_]*", "") %>%
    str_replace_all("\\@", "at ") %>% # "@cabanas, saying crash is at cabanas"
    str_replace_all("@[a-z_,A-Z_,0-9_]*", "") %>%
    str_replace_all(","," , ") %>% # Add space between commas (eg, "road,allsops")
    str_replace_all("\n", "") %>%
    str_replace_all("~", "") %>%
    str_replace_all("\\b(http|https)://t.co/[0-9,A-Z, a-z]*\\b", "") %>%
    str_replace_all("\\b(http|https)://t.co/[0-9,A-Z, a-z]", "") %>%
    str_replace_all("\\b(http|https)://t.co\\b", "") %>%
    str_replace_all("\\b(http|https):", "") %>%
    str_replace_all("~more*", "") %>%
    str_replace_all("(RT|rt) @[a-z,A-Z,0-9, _]*:", "") %>%
    str_replace_all("^[0-9][0-9]\\:[0-9][0-9]", "") %>%
    str_replace_all("\\+", " ") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("\\^", "") %>% # ^ not captured by punct
    str_replace_all("\\bamp\\b", "and") %>%
    str_replace_all("via at.*", "") %>% # remove everything include and after
    #                                     "via at", which comes at end
    str_replace_all("https.*", "") %>% # remove everything include and after
    #                                   "https", which comes at end

    # Remove numbers at beginning
    # Squish strings. Should be last thing done
    str_squish() %>%
    str_replace_all("^[:digit:]|^ [:digit:]", "") %>%
    str_replace_all("^[:digit:]|^ [:digit:]", "") %>%
    str_replace_all("^[:digit:]|^ [:digit:]", "") %>%
    str_squish() %>%
    str_replace_all("^[:digit:]|^ [:digit:]", "") %>%
    str_replace_all("^[:digit:]|^ [:digit:]", "") %>%
    str_replace_all("^[:digit:]|^ [:digit:]", "") %>%
    str_replace_all("^[:digit:]|^ [:digit:]", "") %>%

    # Replace Accronmys
    # TODO: Make this a separate file csv file that gets used
    str_replace_all("\\bpri sch\\b", "primary school") %>%
    str_replace_all("\\bsec sch\\b", "secondary school") %>%

    # Squish strings. Should be last thing done
    str_squish()

  #### Remove false positive phrases
  for(phrase in false_positive_phrases){
    # Replace false positive phrases with "blankword", where blankword appears the
    # number of times for each word in the phrase. For example, "githurai bus"
    # becomes "blankword blankword" and "githurai 45 bus" becomes "blankword
    # blankword blankword". Replacing with "bankword" is important as it preserves
    # the number of words between different words -- which is used later in the algorithm.
    text <- text %>% str_replace_all(paste0("\\b",phrase,"\\b"),
                                     rep("blankword", wordcount(phrase)) %>%
                                       paste(collapse=" "))
  }

  # 4. Implement Algorithm -----------------------------------------------------

  if(mc_cores == 1){
    out_all <- lapply(text,
                      locate_event_i,
                      landmark_gazetteer            = landmark_gazetteer,
                      roads                          = roads,
                      areas                          = areas,
                      prepositions_list              = prepositions_list,
                      prep_check_order               = prep_check_order,
                      event_words                    = event_words,
                      junction_words                 = junction_words,
                      false_positive_phrases         = false_positive_phrases,
                      type_list                      = type_list,
                      clost_dist_thresh              = clost_dist_thresh,
                      fuzzy_match                    = fuzzy_match,
                      fuzzy_match.min_word_length    = fuzzy_match.min_word_length,
                      fuzzy_match.dist               = fuzzy_match.dist,
                      fuzzy_match.ngram_max          = fuzzy_match.ngram_max,
                      fuzzy_match.first_letters_same = fuzzy_match.first_letters_same,
                      fuzzy_match.last_letters_same  = fuzzy_match.last_letters_same,
                      crs_distance                   = crs_distance,
                      crs_out                        = crs_out,
                      quiet                          = quiet,

                      landmark_list = landmark_list,
                      roads_list    = roads_list,
                      areas_list    = areas_list,
                      prepositions_all = prepositions_all,
                      junction_words_regex = junction_words_regex) %>%
      bind_rows()
  } else{
    out_all <- mclapply(text,
                        locate_event_i,
                        landmark_gazetteer            = landmark_gazetteer,
                        roads                          = roads,
                        areas                          = areas,
                        prepositions_list              = prepositions_list,
                        prep_check_order               = prep_check_order,
                        event_words                    = event_words,
                        junction_words                 = junction_words,
                        false_positive_phrases         = false_positive_phrases,
                        type_list                      = type_list,
                        clost_dist_thresh              = clost_dist_thresh,
                        fuzzy_match                    = fuzzy_match,
                        fuzzy_match.min_word_length    = fuzzy_match.min_word_length,
                        fuzzy_match.dist               = fuzzy_match.dist,
                        fuzzy_match.ngram_max          = fuzzy_match.ngram_max,
                        fuzzy_match.first_letters_same = fuzzy_match.first_letters_same,
                        fuzzy_match.last_letters_same  = fuzzy_match.last_letters_same,
                        crs_distance                   = crs_distance,
                        crs_out                        = crs_out,
                        quiet                          = quiet,

                        landmark_list = landmark_list,
                        roads_list    = roads_list,
                        areas_list    = areas_list,
                        prepositions_all = prepositions_all,
                        junction_words_regex = junction_words_regex,
                        mc.cores = mc_cores) %>%
      bind_rows()
  }

  if(class(out_all)[1] == "sf"){
    st_crs(out_all) <- crs_out
  }

  # 5. Cleanup Output ----------------------------------------------------------

  for(var in c("matched_words_correct_spelling",
               "matched_words_text_spelling",
               "dist_closest_event_word",
               "type",
               "how_determined_location",
               "dist_mentioned_road_m")){

    if(is.null(out_all[[var]])){
      out_all[[var]] <- NA
    }

  }

  out_all <- out_all %>%
    dplyr::select(.data$text,
                  .data$matched_words_correct_spelling,
                  .data$matched_words_text_spelling,
                  .data$dist_closest_event_word,
                  .data$type,
                  .data$how_determined_location,
                  .data$dist_mentioned_road_m,
                  everything())

  return(out_all)
}

locate_event_i <- function(text_i,
                           landmark_gazetteer,
                           roads,
                           areas,
                           prepositions_list,
                           prep_check_order,
                           event_words,
                           junction_words,
                           false_positive_phrases,
                           type_list,
                           clost_dist_thresh,
                           fuzzy_match,
                           fuzzy_match.min_word_length,
                           fuzzy_match.dist,
                           fuzzy_match.ngram_max,
                           fuzzy_match.first_letters_same,
                           fuzzy_match.last_letters_same,
                           crs_distance,
                           crs_out,
                           quiet,
                           landmark_list,
                           roads_list,
                           areas_list,
                           prepositions_all,
                           junction_words_regex){

  #text_i %>% as.data.frame() %>% write.csv(paste0("~/Desktop/where_are_we/",Sys.time() %>% str_replace_all("-| |:", ""), ".csv"))

  quiet_debug <- T

  # Before subsetting
  landmark_gazetteer_orig <- landmark_gazetteer
  # 1. Determine Location Matches in Gazetteer ---------------------------------
  if(!quiet) message(text_i)

  if(!quiet_debug) message("Section - 1")
  #### Exact Match
  ## Remove certain words when looking for an exact match

  text_i_extract_exact <- text_i %>%
    str_replace_all(paste0("\\b", paste(event_words, collapse = "|"), "\\b"),
                    "blankwordblankword") # add dummy word to prevent new
  # word pair from being made

  landmark_match     <- phrase_in_sentence_exact(text_i_extract_exact,
                                                 landmark_list)
  road_match         <- phrase_in_sentence_exact(text_i_extract_exact,
                                                 roads_list)
  area_match <- phrase_in_sentence_exact(text_i_extract_exact,
                                         areas_list)

  #### Fuzzy
  if(fuzzy_match %in% TRUE){
    # TODO: paramaterize additional words that can't be spelled wrong - eg, nairobi
    words_to_remove_fuzzy <- c(prepositions_all, event_words, junction_words, "nairobi") %>% unlist()
    landmark_match_fuzzy <- phrase_in_sentence_fuzzy(text_i,
                                                     landmark_list,
                                                     fuzzy_match.min_word_length,
                                                     fuzzy_match.dist,
                                                     fuzzy_match.ngram_max,
                                                     fuzzy_match.first_letters_same,
                                                     fuzzy_match.last_letters_same,
                                                     words_to_remove_fuzzy)
    road_match_fuzzy <- phrase_in_sentence_fuzzy(text_i,
                                                 roads_list,
                                                 fuzzy_match.min_word_length,
                                                 fuzzy_match.dist,
                                                 fuzzy_match.ngram_max,
                                                 fuzzy_match.first_letters_same,
                                                 fuzzy_match.last_letters_same,
                                                 words_to_remove_fuzzy)
    area_match_fuzzy <- phrase_in_sentence_fuzzy(text_i,
                                                 areas_list,
                                                 fuzzy_match.min_word_length,
                                                 fuzzy_match.dist,
                                                 fuzzy_match.ngram_max,
                                                 fuzzy_match.first_letters_same,
                                                 fuzzy_match.last_letters_same,
                                                 words_to_remove_fuzzy)

    #### Remove fuzzy match if:
    # (1) Tweet spelling is one word
    # (2) Tweet spelling is correctly spelled

    landmark_match_fuzzy <- landmark_match_fuzzy %>%
      #filter(!(str_count(matched_words_text_spelling, "\\S+") %in% 1)) %>% # tweet: tajmall; correct: taj mall
      dplyr::filter(!hunspell_check(.data$matched_words_text_spelling))

    road_match_fuzzy <- road_match_fuzzy %>%
      #filter(!(str_count(matched_words_text_spelling, "\\S+") %in% 1)) %>%
      dplyr::filter(!hunspell_check(.data$matched_words_text_spelling))

    area_match_fuzzy <- area_match_fuzzy %>%
      #filter(!(str_count(matched_words_text_spelling, "\\S+") %in% 1)) %>%
      dplyr::filter(!hunspell_check(.data$matched_words_text_spelling))

    #### Add fuzzy match to full match list
    # Starting with exact match ensures, if both exact and fuzzy, only
    # exact is kept.
    landmark_match <- bind_rows(landmark_match, landmark_match_fuzzy) %>%
      distinct(.data$matched_words_correct_spelling, .keep_all = TRUE)

    road_match <- bind_rows(road_match, road_match_fuzzy) %>%
      distinct(.data$matched_words_correct_spelling, .keep_all = TRUE)

    area_match <- bind_rows(area_match, area_match_fuzzy) %>%
      distinct(.data$matched_words_correct_spelling, .keep_all = TRUE)

  }

  #### Dataframe of all locations found in tweet, appending across
  ## Add types
  landmark_match     <- landmark_match %>% dplyr::mutate(location_type = "landmark")
  road_match         <- road_match     %>% dplyr::mutate(location_type = "road")
  area_match         <- area_match     %>% dplyr::mutate(location_type = "area")

  ## Append
  # Don't append before as there could be cases where a landmark and road has
  # the same name, and appending and making distict would pick one over the
  # other (?), which we deal with in a separate process.
  locations_in_tweet <- bind_rows(landmark_match, road_match, area_match)

  # 2. Landmarks after prepositions --------------------------------------------
  if(!quiet_debug) message("Section - 2")
  # ** 2.1 Subset locations by roads and neighborhood -----------------------------
  if(!quiet_debug) message("Section - 2.1")
  # Before searching for words after landmarks, restrict by roads and neighborhoods.
  # This process restricts the gazetteer, and may make more likely to find a
  # dominant cluster

  ## Roads
  # Roads take precedent over landmarks, so OK to do this here.
  # Fails in case where "accident at pangani towards muaranga rd", where pangani not near muranga rd and muranga is a landmark
  if((nrow(road_match) > 0) & (nrow(landmark_match) > 0) & F){
    road_match_sp <- roads[roads$name %in% road_match$matched_words_correct_spelling,]

    land_road_restrict <- restrict_landmarks_by_location(landmark_match,
                                                         landmark_gazetteer,
                                                         road_match_sp)
    landmark_match     <- land_road_restrict$landmark_match
    landmark_gazetteer <- land_road_restrict$landmark_gazetteer
  }

  ## Areas
  # Neighborhoods may not take precedent over landmarks. For example, "garden city"
  # matches the "garden" and other garden landmarks. Garden city is not near
  # "garden" estate, so it's removed.
  if(nrow(area_match) > 0 & F){
    area_match_sp <- areas[areas$name %in% area_match$matched_words_correct_spelling,]

    area_road_restrict <- restrict_landmarks_by_location(landmark_match,
                                                         landmark_gazetteer,
                                                         area_match_sp)
    landmark_match     <- area_road_restrict$landmark_match
    landmark_gazetteer <- area_road_restrict$landmark_gazetteer
  }

  ## Update locations_in_tweet with new landmark dataframe
  locations_in_tweet <- locations_in_tweet %>%
    dplyr::filter((.data$location_type %in% "road") |
                    (.data$location_type %in% "area") |
                    ((.data$location_type %in% "landmark") &
                       (.data$matched_words_correct_spelling %in% landmark_match$matched_words_correct_spelling)))

  # ** 2.2 Preposition Locations --------------------------------------------------
  if(!quiet_debug) message("Section - 2.2")
  # When grabbing landmarks after prepositions we ignore stopwords. So for:
  # "accident near the garden city", we ignore "the"
  text_i_no_stopwords <- text_i %>% str_replace_all("\\bthe\\b", " ") %>% str_squish

  ## Create vector of locations in tweets where prepositions occur
  preps_in_tweet <- phrase_in_sentence_exact(text_i_no_stopwords, prepositions_all)

  ## Locations of Prepositions
  if(length(as.character(preps_in_tweet$matched_words_text_spelling)) %in% 0){
    prep_locs <- NULL
  } else{
    prep_locs_df <- lapply(as.character(preps_in_tweet$matched_words_text_spelling),
                           phrase_locate,
                           text_i_no_stopwords) %>%
      bind_rows %>%
      dplyr::filter(!(.data$word_loc_max %in% c(-Inf, Inf)))

    prep_locs <- prep_locs_df$word_loc_max %>% unique() # vector of locations of prepositions in tweet

  }


  # ** 2.3 Extract landmarks ------------------------------------------------------
  if(!quiet_debug) message("Section - 2.3")
  if(length(prep_locs) > 0){
    locations_in_tweet_prep <- map_df(prep_locs,
                                      extract_locations_after_words,
                                      text_i_no_stopwords,
                                      landmark_gazetteer)
  } else{
    locations_in_tweet_prep <- data.frame(NULL)
  }

  # ** 2.4 Remove if landmark already found ---------------------------------------
  if(!quiet_debug) message("Section - 2.4")
  if(nrow(locations_in_tweet_prep) > 0){

    locations_in_tweet_prep <- locations_in_tweet_prep %>%

      ## Prep Variables
      dplyr::select("matched_words_text_spelling",
                    "matched_words_correct_spelling") %>%
      dplyr::mutate(exact_match = FALSE,
                    location_type = "landmark") %>%

      ## Remove if landmark already found
      dplyr::filter(!(.data$matched_words_text_spelling %in% locations_in_tweet$matched_words_text_spelling))

    ## Add to main locations dataframe
    locations_in_tweet <- bind_rows(locations_in_tweet, locations_in_tweet_prep)
  }

  # 4. Choosing which landmarks to use -----------------------------------------
  if(!quiet_debug) message("Section - 4")

  df_out <- data.frame(lat = NA,
                       lon = NA)

  ## Original Landmarks
  # Before any subsetting, save locations dataframe. Add all locations originally
  # found as variables in final dataframe.
  locations_in_tweet_original <- locations_in_tweet

  N_check <- locations_in_tweet %>%
    dplyr::filter(!(.data$location_type %in% "area")) %>%
    nrow()

  if(N_check > 0){

    # ** 4.-1 If road small, make landmark -------------------------------------
    if(!quiet_debug) message("Section - 4.-.1")

    roads_match <- locations_in_tweet[locations_in_tweet$location_type %in% "road",]

    if(nrow(roads_match) > 0){

      roads_match_sp <- roads[roads$name %in% roads_match$matched_words_correct_spelling,]

      road_points <- lapply(unique(roads_match_sp$name), function(name){

        roads_match_sp_i <- roads_match_sp[roads_match_sp$name %in% name,]

        max_dist_m <- st_distance(roads_match_sp_i) %>% max() %>% as.numeric()

        max_dist_km <- max_dist_m / 1000

        if(max_dist_km <= 0.75){

          #road_point <- gCentroid(roads_match_sp_i, byid=F)
          road_point <- roads_match_sp_i %>%
            st_union() %>%
            st_centroid() %>%
            st_as_sf() %>%
            dplyr::rename(geometry = .data$x)
          road_point$name <- name
          road_point$type <- "road"
          road_point$general_specific <- "specific"
          road_point$name_original <- name

        } else{
          road_point <- NULL
        }

        return(road_point)

      }) %>%
        purrr::discard(is.null) %>%
        do.call(what="rbind")

      if(!is.null(road_points)){

        #### Add to gazetteer
        road_points$uid <- max(landmark_gazetteer$uid) + 1:nrow(road_points)

        landmark_gazetteer <- landmark_gazetteer %>%
          dplyr::select("uid", "name", "name_original", "type", "general_specific")

        landmark_gazetteer <- list(landmark_gazetteer, road_points) %>% do.call(what = "rbind")

        #### Switch type from "road" to "landmark"
        # Can't have both road and landmark type, as later preference
        # roads over landmark if same name
        locations_in_tweet$location_type[locations_in_tweet$matched_words_correct_spelling %in%
                                           road_points$name] <- "landmark"


      }
    }

    # ** 4.0 Add Preposition Variables to Dataframe ----------------------------
    if(!quiet_debug) message("Section - 4.0")
    # Loop through preposition tiers
    for(i in 1:length(prepositions_list)){

      prepositions <- prepositions_list[[i]]

      # Consider event words as tier 1 [eg, accident X]
      #if(i %in% 1){
      #  prepositions <- c(prepositions, event_words)
      #}

      #### locations_in_tweet dataframe
      locations_in_tweet[[paste0("crashword_prepos_tier_", i)]] <-
        search_crashword_prepos(text_i, locations_in_tweet$matched_words_text_spelling, event_words, prepositions)

      locations_in_tweet[[paste0("crashword_other_prepos_tier_", i)]] <-
        search_crashword_other_prepos(text_i, locations_in_tweet$matched_words_text_spelling, event_words, prepositions)

      locations_in_tweet[[paste0("prepos_before_crashword_tier_", i)]] <-
        search_prep_loc(text_i, locations_in_tweet$matched_words_text_spelling, prepositions)

    }


    # ** 4.1 Location Dataset Prep ------------------------------------------------
    if(!quiet_debug) message("Section - 4.1")
    # Prep location datasets before continuing with search

    ## Dataset per type
    # Create dataset for each type
    landmark_match <- locations_in_tweet[locations_in_tweet$location_type %in% "landmark",]
    road_match     <- locations_in_tweet[locations_in_tweet$location_type %in% "road",]
    area_match     <- locations_in_tweet[locations_in_tweet$location_type %in% "area",]

    ## Locations in tweet
    # Take area out. By doing this, we allow name conflicts between locations and
    # areas.
    locations_in_tweet <- locations_in_tweet[!(locations_in_tweet$location_type %in% "area"),]



    ## Road shapefile
    if(nrow(road_match) > 0){
      road_match_sp <- roads[roads$name %in% road_match$matched_words_correct_spelling,]

      ## Aggregate roads so one row; makes distance calculations easier
      road_match_agg_sp <- road_match_sp %>%
        mutate(id = 1) %>%
        group_by(.data$id) %>%
        dplyr::summarise(geometry = st_union(.data$geometry)) %>%
        ungroup()

      #road_match_agg_sp$id <- 1
      #road_match_agg_sp <- raster::aggregate(road_match_agg_sp, by="id")
    } else{
      road_match_sp <- NULL # create variable; lateer functions check if null
    }

    ## Areas shapefile
    if(nrow(area_match) > 0){
      area_match_sp <- areas[areas$name %in% area_match$matched_words_correct_spelling,]

      ## Aggregate roads so one row; makes distance calculations easier
      area_match_agg_sp <- area_match_sp

      area_match_agg_sp <- area_match_agg_sp %>%
        mutate(id = 1) %>%
        group_by(.data$id) %>%
        dplyr::summarise(geometry = st_union(.data$geometry)) %>%
        ungroup()

      # area_match_agg_sp$id <- 1
      # area_match_agg_sp <- raster::aggregate(area_match_agg_sp, by="id")
    } else{
      area_match_sp <- NULL
    }

    # ** 4.2 Locations of Words in Tweet ---------------------------------------
    if(!quiet_debug) message("Section - 4.2")
    #### Locations
    # Add location of words in tweet to locations_in_tweet dataframe
    word_locations <- lapply(as.character(locations_in_tweet$matched_words_text_spelling), phrase_locate, text_i) %>% bind_rows
    locations_in_tweet <- merge(locations_in_tweet, word_locations, by.x="matched_words_text_spelling", by.y="word")
    #locations_in_tweet_original <- locations_in_tweet

    ## check this issue??
    locations_in_tweet <- locations_in_tweet[!(locations_in_tweet$word_loc_min %in% c(Inf,-Inf)),]
    locations_in_tweet <- locations_in_tweet[!(locations_in_tweet$word_loc_max %in% c(Inf,-Inf)),]

    #### Other word types
    # Create dataframes indicating locations of (1) crash words and (2) prepositions
    # in tweets
    crash_word_locations <- lapply(event_words, phrase_locate, text_i) %>% bind_rows
    preposition_word_locations <- lapply(prepositions_all, phrase_locate, text_i) %>% bind_rows

    #### Distance Closest Event Word
    if(nrow(crash_word_locations) > 0){

      crash_word_locations_vec <- c(crash_word_locations$word_loc_min, crash_word_locations$word_loc_max) %>% as.vector() %>% unique()

      locations_in_tweet$dist_closest_event_word <- lapply(1:nrow(locations_in_tweet), function(i){
        locations_in_tweet_i <- locations_in_tweet[i,]

        word_loc_min_dist <- min(abs(locations_in_tweet_i$word_loc_min - crash_word_locations_vec))
        word_loc_max_dist <- min(abs(locations_in_tweet_i$word_loc_max - crash_word_locations_vec))

        out <- min(c(word_loc_min_dist, word_loc_max_dist))
        return(out)
      }) %>%
        unlist()

    }


    # ** 4.3 Restrict Locations/Landmarks to Consider --------------------------
    if(!quiet_debug) message("Section - 4.3")
    ## Subset
    locations_in_tweet <- locations_in_tweet %>%
      landmark_road_overlap() %>% # if goes after fuzzy, keep roads before this?
      exact_fuzzy_overlap() %>%
      phase_overlap() %>% # do this before fuzzy... (no? why?)
      exact_fuzzy_startendsame()

    locations_in_tweet_original <- locations_in_tweet
    # TODO: same line is above, but think makes more sense to grab "origina" locations here.

    ## Keep, despite ignoring general and roads/areas restriction
    landmarks_in_tweet <- locations_in_tweet[locations_in_tweet$location_type %in% "landmark",]
    if(nrow(landmarks_in_tweet) > 0){
      vars_for_alws_keep <- names(landmarks_in_tweet)[grepl("crashword_prepos_tier_", names(landmarks_in_tweet))]
      tokeep_01 <- landmarks_in_tweet[,vars_for_alws_keep] %>% rowSums() %>% as.vector()
      locations_in_tweet_alw_keep <- landmarks_in_tweet[tokeep_01 %in% 1,]
      if(nrow(locations_in_tweet_alw_keep) >= 1){
        landmark_gazetteer_alw_keep <- landmark_gazetteer[landmark_gazetteer$name %in% locations_in_tweet_alw_keep$matched_words_correct_spelling,]
      } else{
        landmark_gazetteer_alw_keep <- NULL
      }
    } else{
      landmark_gazetteer_alw_keep <- NULL
      locations_in_tweet_alw_keep <- data.frame(NULL)
    }

    landmark_match <- landmark_match[landmark_match$matched_words_correct_spelling %in% locations_in_tweet$matched_words_correct_spelling,]

    ## Restricts gazetteer by type
    # Do before restrict by general, as here checks for, within
    # a general landmark, is there specific within our type_list?

    if(!is.null(type_list) & nrow(landmark_match) > 0){


      landmark_gazetteer <- remove_gaz_by_type(landmark_match,
                                               landmark_gazetteer,
                                               type_list)
    }

    landmark_match <- landmark_match[landmark_match$matched_words_correct_spelling %in% locations_in_tweet$matched_words_correct_spelling,]

    # ** 4.4 Restrict by roads, neighborhood and tier 1 landmarks --------------
    if(!quiet_debug) message("Section - 4.4")

    landmark_match <- landmark_match[landmark_match$matched_words_correct_spelling %in% locations_in_tweet$matched_words_correct_spelling,]

    #### 1. Prep Shapefiles
    # Do again, after restricting locations (eg, if area and road overlap, choose road -- eg, langata rd)
    road_match     <- locations_in_tweet[locations_in_tweet$location_type %in% "road",]
    area_match     <- locations_in_tweet[locations_in_tweet$location_type %in% "area",]

    ## Road shapefile
    if(nrow(road_match) > 0){
      road_match_sp <- roads[roads$name %in% road_match$matched_words_correct_spelling,]
    } else{
      road_match_sp <- NULL # create variable; lateer functions check if null
    }

    ## Areas shapefile
    if(nrow(area_match) > 0){
      area_match_sp <- areas[areas$name %in% area_match$matched_words_correct_spelling,]
    } else{
      area_match_sp <- NULL
    }

    #### 2. Restrict
    ## Roads
    if(nrow(road_match) > 0 & nrow(landmark_match) > 0){
      land_road_restrict <- restrict_landmarks_by_location(landmark_match,
                                                           landmark_gazetteer,
                                                           road_match_sp)
      landmark_match     <- land_road_restrict$landmark_match
      landmark_gazetteer <- land_road_restrict$landmark_gazetteer
    }

    ## Areas
    if(nrow(area_match) > 0 & nrow(landmark_match) > 0){
      area_road_restrict <- restrict_landmarks_by_location(landmark_match,
                                                           landmark_gazetteer,
                                                           area_match_sp)
      landmark_match     <- area_road_restrict$landmark_match
      landmark_gazetteer <- area_road_restrict$landmark_gazetteer
    }

    ## Tier 1 Landmarks
    if(nrow(landmark_match) > 0 & T){
      landmark_gazetteer <- restrict_gaz_tier1_landmarks(landmark_match,
                                                         landmark_gazetteer)
    }

    ## Update locations_in_tweet with new landmark dataframe
    locations_in_tweet <- locations_in_tweet %>%
      dplyr::filter((.data$location_type %in% "road") |
                      (.data$location_type %in% "area") |
                      ((.data$location_type %in% "landmark") &
                         (.data$matched_words_correct_spelling %in% landmark_match$matched_words_correct_spelling)))


    # ** 4.5 Restrict Gazetteer ------------------------------------------------
    if(!quiet_debug) message("Section - 4.5")
    if(nrow(landmark_match) > 0){

      # If name has both general and specific, preference specific; if all general,
      # keep all general. Only changes gazetteer

      # If name has both general and specific, preference specific; if all general,
      # keep all general. Only changes gazetteer.
      # We do a similar step before; however, here we allow a less strict measure
      # of what we define as specific.

      landmark_gazetteer <- landmark_gazetteer %>%
        pref_specific(landmark_match) %>%
        pref_type_with_gen_landmarks(landmark_match,
                                     type_list) %>%
        pref_orig_name_with_gen_landmarks(landmark_match) %>%
        pref_specific(landmark_match,
                      cluster_thresh = 0.51)


    }

    # ** 4.5 Preference specific -----------------------------------------------

    #if(!quiet_debug) message("Section - 4.5")

    #if(nrow(landmark_match) > 0){
    #  landmark_gazetteer <- pref_specific(landmark_gazetteer,
    #                                      landmark_match)
    #
    #}

    # ** 4.6 Preference types --------------------------------------------------
    # NOTE: below two steps don't get applies to "always keep list", but OK
    # as steps are integrated later too? Below just accounts for general
    # Only restrict gazetteer
    #if(!quiet_debug) message("Section - 4.6")

    #if(nrow(landmark_match) > 0){
    #  landmark_gazetteer <- pref_type_with_gen_landmarks(landmark_gazetteer,
    #                                                     landmark_match,
    #                                                     type_list)
    #}

    # ** 4.7 Preference original name over parallel landmark -------------------
    # Only restrict gazetteer
    #if(!quiet_debug) message("Section - 4.7")

    #if(nrow(landmark_match) > 0){
    #  landmark_gazetteer <- pref_orig_name_with_gen_landmarks(landmark_gazetteer,
    #                                                          landmark_match)
    #}

    # ** 4.5 Preference specific with less strict conditions -------------------


    #if(!quiet_debug) message("Section - 4.8")

    #if(nrow(landmark_match) > 0){
    #  landmark_gazetteer <- pref_specific(landmark_gazetteer,
    #                                      landmark_match,
    #                                      cluster_thresh = 0.51)
    #}

    # ** 4.8 Remove general landmarks ------------------------------------------
    if(!quiet_debug) message("Section - 4.8")

    if(nrow(landmark_match) > 0){
      rm_gen_out <- remove_general_landmarks(landmark_match,
                                             landmark_gazetteer)
      landmark_match     <- rm_gen_out$landmark_match
      landmark_gazetteer <- rm_gen_out$landmark_gazetteer
    }

    ## Update locations_in_tweet with new landmark dataframe
    # Keep all roads
    locations_in_tweet <- locations_in_tweet %>%
      dplyr::filter((.data$location_type %in% "road") |
                      (.data$location_type %in% "area") |
                      ((.data$location_type %in% "landmark") &
                         (.data$matched_words_correct_spelling %in% landmark_match$matched_words_correct_spelling)))

    # ** 4.9 Add always keep back in -------------------------------------------
    if(!quiet_debug) message("Section - 4.9")

    locations_in_tweet <- bind_rows(locations_in_tweet, locations_in_tweet_alw_keep) %>% unique()

    # If name in alw_keep gazetteer still exists in main gazetteer, remove from alw_keep
    landmark_gazetteer_alw_keep <- landmark_gazetteer_alw_keep[!(landmark_gazetteer_alw_keep$name %in%
                                                                   landmark_gazetteer$name),]

    landmark_gazetteer <- list(landmark_gazetteer,
                               landmark_gazetteer_alw_keep) %>%
      purrr::discard(is.null) %>%
      do.call(what = "rbind")

    # 5. Find Intersections ----------------------------------------------------
    if(!quiet_debug) message("Section - 5")

    road_intersections <- extract_intersections(locations_in_tweet, roads, crs_distance)

    # 6. Add Variables to Location Dataframes ----------------------------------
    if(!quiet_debug) message("Section - 6")
    # Add variables indicating the following:
    #   1. [Crash word] [tier x prepositon] [landmark]
    #   2. [Crash word] [other words] [prepositon] [landmark]
    #   3. Preposition before crashword

    # Loop through preposition tiers
    for(i in 1:length(prepositions_list)){

      prepositions <- prepositions_list[[i]]

      # Consider event words as tier 1 [eg, accident X]
      #if(i %in% 1){
      #  prepositions <- c(prepositions, event_words)
      #}

      #### locations_in_tweet dataframe
      locations_in_tweet[[paste0("crashword_prepos_tier_", i)]] <-
        search_crashword_prepos(text_i, locations_in_tweet$matched_words_text_spelling, event_words, prepositions)

      locations_in_tweet[[paste0("crashword_other_prepos_tier_", i)]] <-
        search_crashword_other_prepos(text_i, locations_in_tweet$matched_words_text_spelling, event_words, prepositions)

      locations_in_tweet[[paste0("prepos_before_crashword_tier_", i)]] <-
        search_prep_loc(text_i, locations_in_tweet$matched_words_text_spelling, prepositions)

      #### road_intersections dataframe
      if(nrow(road_intersections) > 0){

        road_intersections[[paste0("crashword_prepos_tier_", i)]] <-
          search_crashword_prepos(text_i, road_intersections$road_tweet_spelling_1, event_words, prepositions) |
          search_crashword_prepos(text_i, road_intersections$road_tweet_spelling_2, event_words, prepositions)

        road_intersections[[paste0("crashword_other_prepos_tier_", i)]] <-
          search_crashword_other_prepos(text_i, road_intersections$road_tweet_spelling_1, event_words, prepositions) |
          search_crashword_other_prepos(text_i, road_intersections$road_tweet_spelling_2, event_words, prepositions)

        road_intersections[[paste0("prepos_before_crashword_tier_", i)]] <-
          search_prep_loc(text_i, road_intersections$road_tweet_spelling_1, prepositions) |
          search_prep_loc(text_i, road_intersections$road_tweet_spelling_2, prepositions)

      }
    }

    # 7. Determine Event Location ----------------------------------------------
    if(!quiet_debug) message("Section - 7")

    # ** 7.1 Prep Location Dataframes ---------------------------------------------
    if(!quiet_debug) message("Section - 7.1")
    locations_in_tweet <- locations_in_tweet %>% unique

    areas_final <- locations_in_tweet[locations_in_tweet$location_type %in% "areas",] %>% unique
    roads_final <- locations_in_tweet[locations_in_tweet$location_type %in% "road",] %>% unique
    landmarks_final <- locations_in_tweet[locations_in_tweet$location_type %in% "landmark",] %>% unique
    road_intersections_final <- road_intersections

    # ** 7.2 Landmark Decision Process --------------------------------------------
    if(!quiet_debug) message("Section - 7.2")
    ## Null output

    loc_searched <- FALSE

    if(nrow(landmarks_final) > 0){

      # **** 7.2.1 Preposition Search: Landmark then Intersection ---------------------
      if(!quiet_debug) message("Section - 7.2.1")

      ## Determine order: prep_i then prep_pattern or vice versa?
      prep_check_order_vec <- c()

      if(prep_check_order %in% "prep_then_pattern"){

        for(prep_i in 1:length(prepositions_list)){
          for(prep_pattern in c("crashword_prepos_tier_",
                                "crashword_other_prepos_tier_",
                                "prepos_before_crashword_tier_")){

            prep_check_order_vec <- c(prep_check_order_vec,
                                      paste0(prep_pattern, prep_i))

          }
        }

      } else{

        for(prep_pattern in c("crashword_prepos_tier_",
                              "crashword_other_prepos_tier_",
                              "prepos_before_crashword_tier_")){
          for(prep_i in 1:length(prepositions_list)){

            prep_check_order_vec <- c(prep_check_order_vec,
                                      paste0(prep_pattern, prep_i))

          }
        }

      }

      for(prep_pattern_i in prep_check_order_vec){

        # 1. [crashword] [preposition] [location]
        if(TRUE %in% landmarks_final[[prep_pattern_i]] & !loc_searched){

          # df_out <<- landmarks_final[landmarks_final[[prep_pattern_i]] %in% TRUE,]
          # how_determined_text <<- paste0("crashword_tier_",prep_pattern_i,"preposition_landmark")
          # landmark_gazetteer <<- landmark_gazetteer
          # roads <<- roads
          # roads_final <<- roads_final
          # type_list <<- type_list
          # crs_distance <<- crs_distance
          # text_i <<- text_i

          df_out <- determine_location_from_landmark(
            landmarks_final[landmarks_final[[prep_pattern_i]] %in% TRUE,],
            paste0("crashword_tier_",prep_pattern_i,"preposition_landmark"),
            landmark_gazetteer,
            roads,
            roads_final,
            type_list,
            crs_distance,
            text_i)

          loc_searched <- TRUE
        }

        ## 2. Do regardless of whether intersection word?
        if(TRUE %in% road_intersections_final[[paste0(prep_pattern, prep_i)]] & !loc_searched){

          df_out <- determine_location_from_intersection(
            road_intersections_final[road_intersections_final[[paste0(prep_pattern, prep_i)]] %in% TRUE,],
            paste0("crashword_tier_",prep_i,"_preposition_intersection"))

          loc_searched <- TRUE
        }

      }


    }

    # **** 7.2.2 Intersection Search ------------------------------------------------
    if(!quiet_debug) message("Section - 7.2.2")
    #### If there is an intersection word and more than one intersection
    if(grepl(junction_words_regex, text_i) & nrow(road_intersections_final) > 0 & !loc_searched){

      df_out <- determine_location_from_intersection(
        road_intersections_final %>% as.data.frame(),
        "intersection_word")

      loc_searched <- TRUE

    }

    #### If there is only one intersection
    if(nrow(road_intersections_final) %in% 1 & !loc_searched){

      df_out <- determine_location_from_intersection(
        road_intersections_final %>% as.data.frame(),
        "one_intersection")

      loc_searched <- TRUE
    }

    # **** 7.2.3 Ambiguous Pattern -------------------------------------------
    if(!quiet_debug) message("Section - 7.2.3")
    if((nrow(landmarks_final) > 0) & !loc_searched){

      df_out <- determine_location_from_landmark(
        landmarks_final,
        "landmark_ambiguous_pattern",
        landmark_gazetteer,
        roads,
        roads_final,
        type_list,
        crs_distance,
        text_i)

      loc_searched <- TRUE

    }

    # **** 7.2.4 Output Cleaning and Checks ---------------------------------------
    if(!quiet_debug) message("Section 7.2.4")

    #### Spatially define
    if(!is.na(df_out$lat[1])){

      df_out_sp <- df_out %>% distinct()

      df_out_sp <- st_as_sf(df_out_sp,
                            coords = c("lon", "lat"),
                            crs = crs_distance)

      # coordinates(df_out_sp) <- ~lon+lat
      # crs(df_out_sp) <- CRS(crs_distance)

      #### If dataframe more than one row, collapse to one row
      df_out$id <- 1

      df_out <- df_out %>%
        group_by(.data$id) %>%
        summarise_all(function(x) x %>% unique %>% paste(collapse = ";")) %>%
        dplyr::rename(lon_all = .data$lon,
                      lat_all = .data$lat) %>%
        dplyr::select(-"id") %>%
        st_drop_geometry()

      #### Dominant Cluster
      df_out_sp <- extract_dominant_cluster(df_out_sp)

      if(nrow(df_out_sp) > 0){

        coords <- df_out_sp %>%
          st_union() %>%
          st_centroid() %>%
          st_as_sf() %>%
          dplyr::rename(geometry = .data$x) %>%
          dplyr::select("geometry") %>%
          bind_cols(df_out)

        # coords <- gCentroid(df_out_sp)
        # coords$id <- 1 # dummy variable so if spatial dataframe
        # coords@data <- df_out %>% as.data.frame()

        df_out <- coords

        #### Add distance to mentioned road
        if(!is.null(road_match_sp) > 0){
          df_out$dist_mentioned_road_m <- st_distance(road_match_agg_sp, df_out) %>% as.numeric()
        }

        # If no dominant cluster
      } else{
        df_out <- df_out %>%
          dplyr::mutate(lon = NA,
                        lat = NA,
                        no_dominant_cluster = T)
      }

    }

    if((nrow(roads_final) > 0 | nrow(areas_final) > 0) & !loc_searched){

      # ** 7.3 Road and Area ---------------------------------------------------
      if(!quiet_debug) message("Section 7.3")

      # If don't intersect, ignore
      if(nrow(roads_final) > 0 & nrow(areas_final) > 0 & !loc_searched){

        df_out_candidate <- st_intersection(roads_final, areas_final)

        if(!is.null(df_out_candidate)){

          df_out <- df_out_candidate %>%
            dplyr::mutate(id = 1) %>%
            group_by(.data$id) %>%
            dplyr::summarise(geometry = st_union(.data$geometry)) %>%
            ungroup()

          #df_out_candidate$id <- 1
          #df_out <- raster::aggregate(df_out_candidate, by="id")

          # If extent is small, make point
          df_out <- make_point_small_extent(df_out)

          df_out$how_determined_location <- "road_area_intersection"

          loc_searched <- TRUE
        }

      }

      # ** 7.4 Road Decision Process ------------------------------------------------
      if(!quiet_debug) message("Section 7.4")
      # If no landmark, output road only if one road

      if((nrow(roads_final) %in% 1) & !loc_searched){

        # Create spatial dataframe of road
        roads_final_sp <- merge(roads, roads_final, by.x="name", by.y="matched_words_correct_spelling", all.x=F)

        # Only use if none of road segments is ambiguous
        if(!(TRUE %in% roads_final_sp$ambiguous_road) & !loc_searched){

          # Use road if df_out is blank
          df_out <- roads_final_sp %>%
            dplyr::mutate(id = 1) %>%
            group_by(.data$id) %>%
            dplyr::summarise(geometry = st_union(.data$geometry)) %>%
            ungroup()

          #roads_final_sp$id <- 1
          #df_out <- raster::aggregate(roads_final_sp, by="id")

          # If extent is small, make point
          df_out <- make_point_small_extent(df_out)

          df_out$how_determined_location <- "one_road"

          loc_searched <- TRUE

        }
      }

      # ** 7.5 Area Decision Process -------------------------------------------
      if(!quiet_debug) message("Section 7.5")

      if(nrow(areas_final) %in% 1 & !loc_searched){

        # Create spatial dataframe of road
        areas_final_sp <- merge(areas, areas_final, by.x="name", by.y="matched_words_correct_spelling", all.x=F)

        # Use road if df_out is blank
        df_out <- areas_final_sp %>%
          dplyr::mutate(id = 1) %>%
          group_by(.data$id) %>%
          dplyr::summarise(geometry = st_union(.data$geometry)) %>%
          ungroup()

        #areas_final_sp$id <- 1
        #df_out <- raster::aggregate(areas_final_sp, by="id")

        # If extent is small, make point
        df_out <- make_point_small_extent(df_out)

        df_out$how_determined_location <- "one_area"

        loc_searched <- TRUE

      }

    }

  }

  # 8. Add Variables to Output -----------------------------------------------
  if(!quiet_debug) message("Section - 8")

  # 1. Add all location types found
  # 2. Add tweet
  if("landmark" %in% locations_in_tweet_original$location_type){
    df_out$landmarks_all_text_spelling <- locations_in_tweet_original$matched_words_text_spelling[locations_in_tweet_original$location_type %in% "landmark"] %>% unique %>% paste(collapse=";")
    df_out$landmarks_all_correct_spelling <- locations_in_tweet_original$matched_words_correct_spelling[locations_in_tweet_original$location_type %in% "landmark"] %>% unique %>% paste(collapse=";")

    landmark_gazetteer_orig <- landmark_gazetteer_orig %>%
      add_latlon_vars_to_sf()

    df_out$landmarks_all_location <- landmark_gazetteer_orig[landmark_gazetteer_orig$name %in% locations_in_tweet_original$matched_words_text_spelling[locations_in_tweet_original$location_type %in% "landmark"],] %>%
      as.data.frame() %>%
      dplyr::mutate(location = paste0(.data$name,",",.data$lat,",",.data$lon)) %>%
      pull("location") %>%
      unique %>%
      paste(collapse=";")
  }

  if("road" %in% locations_in_tweet_original$location_type){
    df_out$roads_all_text_spelling <- locations_in_tweet_original$matched_words_text_spelling[locations_in_tweet_original$location_type %in% "road"] %>% unique %>% paste(collapse=";")
    df_out$roads_all_correct_spelling <- locations_in_tweet_original$matched_words_correct_spelling[locations_in_tweet_original$location_type %in% "road"] %>% unique %>% paste(collapse=";")
  }

  if("neighborhood" %in% locations_in_tweet_original$location_type){
    df_out$neighborhoods_all_tweet_spelling <- locations_in_tweet_original$matched_words_text_spelling[locations_in_tweet_original$location_type %in% "neighborhood"] %>% unique %>% paste(collapse=";")
    df_out$neighborhoods_all_correct_spelling <- locations_in_tweet_original$matched_words_correct_spelling[locations_in_tweet_original$location_type %in% "neighborhood"] %>% unique %>% paste(collapse=";")
  }

  if(exists("road_intersections")){
    if(nrow(road_intersections) >= 1){

      rd_inter_df <- st_transform(road_intersections, crs_out) %>%
        sf_to_df() %>%
        dplyr::mutate(intersection_all_text_spelling = paste0(.data$road_tweet_spelling_1,",", .data$road_tweet_spelling_2),
                      intersection_all_correct_spelling = paste0(.data$road_correct_spelling_1,",", .data$road_correct_spelling_2),
                      intersection_all_location = paste0(.data$intersection_all_correct_spelling, ",",.data$lat,",",.data$lon))

      df_out$intersection_all_text_spelling <- rd_inter_df$intersection_all_text_spelling %>% unique %>% paste(collapse=";")
      df_out$intersection_all_correct_spelling <- rd_inter_df$intersection_all_correct_spelling %>% unique %>% paste(collapse=";")
      df_out$intersection_all_location <- rd_inter_df$intersection_all_location %>% unique %>% paste(collapse=";")

    }
  }


  df_out$text <- text_i
  if(!is.null(df_out$dist_closest_event_word)) df_out$dist_closest_event_word <- as.character(df_out$dist_closest_event_word)



  return(df_out)
}






