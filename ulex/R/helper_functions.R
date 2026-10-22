

##### ******************************************************************** #####
# OTHER ------------------------------------------------------------------------
# Check if a dataframe has zero observations
#
# @noRd
# @export
nrow_0 <- function(df){
  return(nrow(df) %in% 0)
}

# Make N-Gram dataframe in chunks
#
# @noRd
# @export
make_gram_df_chunks <- function(df, chunk_size, FUN, quiet){
  # The function for making the gram dataframes can be memory intensive.
  # Here, we wrap around the function and make it into chunks.
  starts <- seq(from = 1, to = nrow(df), by=chunk_size)

  make_gram_i <- function(start_i, df, chunk_size){
    if(!quiet) message(paste0(start_i, " / ", nrow(df)))

    end_i <- min((start_i + chunk_size - 1), nrow(df))
    df_i <- df[start_i:end_i,]
    ngram_df_i <- FUN(df_i)
    return(ngram_df_i)
  }

  gram_df_all <- lapply(starts, make_gram_i, df, chunk_size) %>%
    do.call(what = "rbind")

  return(gram_df_all)
}

# Convert sf object to dataframe with lat and lon as variables
#
# @noRd
# @export
sf_to_df <- function(data_sf,
                     lon_name = "lon",
                     lat_name = "lat"){
  # Convert sf dataframe to dataframe

  coord_df <- data_sf %>%
    st_coordinates() %>%
    as.data.frame()

  names(coord_df) <- c(lon_name, lat_name)

  data_df <- data_sf %>%
    st_drop_geometry() %>%
    bind_cols(coord_df)

  return(data_df)
}

# Add latitude and longitude variables to sf object
#
# @noRd
# @export
add_latlon_vars_to_sf <- function(data_sf,
                                  lon_name = "lon",
                                  lat_name = "lat"){
  # Add lat/lon variables to sf object

  if( !("lon" %in% names(data_sf)) ){

    coord_df <- data_sf %>%
      st_centroid() %>%
      st_coordinates() %>%
      as.data.frame()

    # coord_df <- data_sf %>%
    #   st_coordinates() %>%
    #   as.data.frame() %>%
    #   head(1)

    names(coord_df) <- c(lon_name, lat_name)

    data_sf <- data_sf %>%
      bind_cols(coord_df)

  }

  return(data_sf)

}

# Drop units
#
# @noRd
# @export
drop_units <- function(x) {
  # https://github.com/r-quantities/units/issues/68#issuecomment-341697927
  class(x) <- setdiff(class(x), "units")
  attr(x, "units") <- NULL
  x
}

# Extract dominant spatial cluster
#
# @noRd
# @export
extract_dominant_cluster_all <- function(landmarks,
                                         close_thresh_km = .9, #0.75,
                                         cluster_thresh = 0.624,
                                         N_loc_limit = 100,
                                         collapse_specific_coords = F,
                                         return_general_landmarks = "none",
                                         quiet = T){

  landmarks$general_specific <- NULL

  # Determine general/specific - - - - - - - - - - - - - - - - - - - - - - - - -
  ## For each unique name, determine an upper bound (maximum possible distance)
  # between coordinates
  landmarks_df <- landmarks %>%
    sf_to_df() %>%
    dplyr::group_by(.data$name) %>%
    dplyr::mutate(N_name = n(),
                  lat_min = min(.data$lat),
                  lat_max = max(.data$lat),
                  lon_min = min(.data$lon),
                  lon_max = max(.data$lon)) %>%
    dplyr::ungroup()

  landmarks_df$max_dist_km <- geodist(x = landmarks_df[,c("lon_min", "lat_min")],
                                      y = landmarks_df[,c("lon_max", "lat_max")],
                                      paired = T) / 1000

  ## Split into dataframes where all the landmarks are close versus far
  landmarks_df_close <- landmarks_df[landmarks_df$max_dist_km <= close_thresh_km,]
  landmarks_df_far <- landmarks_df[landmarks_df$max_dist_km > close_thresh_km,]

  ## Among far, determine whether number of landmarks is above/below threshold
  # for number of landmarks to consider
  landmarks_df_far_many <- landmarks_df_far[landmarks_df_far$N_name > N_loc_limit,]
  landmarks_df_far_few <- landmarks_df_far[landmarks_df_far$N_name <= N_loc_limit,]

  ## Spatiall define & define general/specific
  if(nrow(landmarks_df_close) > 0){

    landmarks_df_close <- landmarks_df_close %>%
      st_as_sf(coords = c("lon", "lat"),
               crs = st_crs(landmarks)) %>%
      dplyr::mutate(general_specific = "specific")

  } else{
    landmarks_df_close <- NULL
  }

  if(nrow(landmarks_df_far_many) > 0){
    landmarks_df_far_many <- landmarks_df_far_many %>%
      st_as_sf(coords = c("lon", "lat"),
               crs = st_crs(landmarks)) %>%
      dplyr::mutate(general_specific = "general")

  } else{
    landmarks_df_far_many <- NULL
  }

  if(return_general_landmarks %in% "none") landmarks_df_far_many <- NULL

  if(nrow(landmarks_df_far_few) > 0){

    landmarks_df_far_few <- landmarks_df_far_few %>%
      st_as_sf(coords = c("lon", "lat"),
               crs = st_crs(landmarks))

    landmarks_df_far_few <- lapply(as.character(unique(landmarks_df_far_few$name)), function(name){

      out <- extract_dominant_cluster(landmarks_df_far_few[landmarks_df_far_few$name %in% name,],
                                      close_thresh_km =close_thresh_km,
                                      cluster_thresh = cluster_thresh,
                                      N_loc_limit = N_loc_limit,
                                      collapse_specific_coords = collapse_specific_coords,
                                      return_general_landmarks = return_general_landmarks)



      if(nrow(out) == 0) out <- NULL
      return(out)
    }) %>%
      purrr::discard(is.null) %>%
      bind_rows()

  } else{
    landmarks_df_far_few <- NULL
  }

  ## Append
  landmarks_gs <- list(landmarks_df_close,
                       landmarks_df_far_many,
                       landmarks_df_far_few) %>%
    purrr::discard(is.null) %>%
    do.call(what="rbind")

  # Format Output - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if(collapse_specific_coords %in% T){

    landmarks_gs_df <- landmarks_gs %>%
      as.data.frame()

    landmarks_gs_df_g <- landmarks_gs_df %>%
      dplyr::filter(.data$general_specific %in% "general")

    make_unique_string <- function(x){
      x %>%
        str_split(";") %>%
        unlist() %>%
        unique() %>%
        paste(collapse = ";")
    }

    landmarks_gs_df_s <- landmarks_gs_df %>%
      dplyr::filter(.data$general_specific %in% "specific") %>%
      group_by(.data$name) %>%

      # ideally would do summarise then summarise_at, but can't. So mutate, then
      # take the first value in each group
      mutate_at(vars(-"lat", -"lon", -"name"), make_unique_string) %>%
      dplyr::mutate(lat = mean(.data$lat),
                    lon = mean(.data$lon)) %>%
      #summarise_all(. %>% head(1)) %>%

      ungroup() %>%
      distinct(.data$name, .keep_all = T)

    for(var in names(landmarks_gs_df_s)[!(names(landmarks_gs_df_s) %in% c("lat", "lon"))]) landmarks_gs_df_s[[var]] <- landmarks_gs_df_s[[var]] %>% as.character()
    for(var in names(landmarks_gs_df_g)[!(names(landmarks_gs_df_g) %in% c("lat", "lon"))]) landmarks_gs_df_g[[var]] <- landmarks_gs_df_g[[var]] %>% as.character()

    landmarks_gs <- bind_rows(landmarks_gs_df_g, landmarks_gs_df_s)

    landmarks_gs <- landmarks_gs %>%
      st_as_sf(coords = c("lon", "lat"),
               crs = st_crs(landmarks))

    # coordinates(landmarks_gs) <- ~lon+lat
    # crs(landmarks_gs) <- CRS(as.character(landmarks@proj4string))
  }

  return(landmarks_gs)
}

# Extract dominant spatial cluster
#
# @noRd
# @export
extract_dominant_cluster <- function(sdf,
                                     close_thresh_km = 0.9, #0.75,
                                     cluster_thresh = 0.624,
                                     N_loc_limit = 100,
                                     collapse_specific_coords = F,
                                     return_general_landmarks = "none"){
  # DESCRIPTION: Finds a dominant spatial cluster in a set of points
  # ARGS:
  # sdf: Spatial points dataframe. Assumed to be projected.
  # close_thresh_km: distance threshold (in kilometers) at which points
  #                  are considered close
  # cluster_thresh: Cluster threshold, which defines the proportion of points
  #                 that must be close by to be considered a dominant
  #                 spatial cluster.
  # N_loc_limit: If points aren't close, only attempts to find a dominant
  #              spatial cluster if the number of points is less than this
  #              threshold. If the number of points is above this threshold,
  #              we run into computational efficiency issues in calculating
  #              distance matrix and just assume unlikely to find a dominant
  #              cluster.
  # collapse_specific_coords: If TRUE, just returns the centroid coordinates of the
  #                    dominant spatial cluster. If FALSE, returns all
  #                    the original spatial dataframe but subsetted to the
  #                    observations in the dominant spatial cluster. If no
  #                    dominant spatial cluster is found, a NULL dataframe
  #                    is returned.
  # return_general_landmarks: The options are:
  #                           "none:" If no dominant cluster found, returns
  #                                   blank dataframe.
  #                           "only_if_all_general": Adds a "general_specific" varaible.
  #                                                  If a dominanc cluster is found,
  #                                                  only returns the "specific"
  #                                                  locations (part of the cluster)
  #                                                  and drops the "general" locations
  #                                                  (not part of cluster). If no dominant
  #                                                  cluster found, returns all original
  #                                                  locations as "general"
  #                           "all:" Adds a "general_specific" varaible. If a
  #                                  dominant cluster is found, returns all
  #                                  locations but indicates which locations are
  #                                  "specific" (part of dominant cluster) and
  #                                  "general" (not part of cluster). If no dominant
  #                                  cluster found, returns all original locations
  #                                  as "general"


  # Default output if not cluster is found
  sdf_out <- data.frame(NULL)
  cluster_exists <- F # default

  # 1. Fast all close check
  sdf_extent <- extent(sdf)

  max_dist_km <- as.numeric(geodist(x = c(sdf_extent@xmin,
                                          sdf_extent@ymin),
                                    y = c(sdf_extent@xmax,
                                          sdf_extent@ymax))) / 1000

  # If everything is close...
  if(max_dist_km < close_thresh_km){

    sdf_out <- sdf
    sdf_out <- sdf_out %>%
      dplyr::mutate(general_specific = "specific")

    # If locations aren't close and not too many locations...
  } else if ( (max_dist_km > close_thresh_km) & (nrow(sdf) <= N_loc_limit)){

    ## Distance matrix
    #sdf_dist_mat <- as.numeric(st_distance(sdf)) / 1000
    sdf_dist_mat_m <- st_distance(sdf) %>% drop_units()
    sdf_dist_mat <- sdf_dist_mat_m / 1000
    # Check if cluster exists
    sdf_dist_mat_list <- sdf_dist_mat %>% as.vector()
    cluster_exists <- mean(sdf_dist_mat_list <= close_thresh_km) >= cluster_thresh


    # If cluster exists, extract coordinates of dominant cluster
    if(cluster_exists){
      # Extract columns where more than X % are close together
      close_thresh_km_closeTF <- sdf_dist_mat <= close_thresh_km
      in_dominant_cluster <- (colSums(close_thresh_km_closeTF) / nrow(close_thresh_km_closeTF) >= cluster_thresh)

      if(return_general_landmarks %in% c("none", "only_if_all_general")){
        sdf <- sdf[in_dominant_cluster,]
        sdf$general_specific <- "specific"
      }

      if(return_general_landmarks %in% "all"){
        sdf$general_specific <- ""
        sdf$general_specific[in_dominant_cluster] <- "specific"
        sdf$general_specific[!in_dominant_cluster] <- "general"
      }

      sdf_out <- sdf
    } else{

      if(return_general_landmarks %in% c("only_if_all_general", "all")){
        sdf_out <- sdf
        sdf_out$general_specific <- "general"
      }


    }

    # If above threshold
  } else{

    if(return_general_landmarks %in% c("only_if_all_general", "all")){
      sdf_out <- sdf
      sdf_out$general_specific <- "general"
    }


  }

  ## Whether to collapse coordinates
  # If return_coord_only = T, only collapse "specific" coordinates
  if(collapse_specific_coords & nrow(sdf_out) >= 1){

    sdf_out_specific <- sdf_out[sdf_out$general_specific %in% "specific",]
    sdf_out_general  <- sdf_out[sdf_out$general_specific %in% "general",]

    #### Collapse Specific
    if(nrow(sdf_out_specific) >= 1){
      sdf_out_specific_centroid <- sdf_out_specific %>%
        st_union() %>%
        st_centroid() %>%
        st_as_sf() %>%
        dplyr::rename(geometry = .data$x)
      sdf_out_specific_centroid$temp_var <- 1 # dummy var so becomes spatial dataframe

      for(var in names(sdf_out)){
        sdf_out_specific_centroid[[var]] <- sdf_out_specific[[var]] %>% str_split(";") %>% unlist() %>% unique %>% paste(collapse=";")
      }

      sdf_out_specific_centroid$temp_var <- NULL

      sdf_out_specific <- sdf_out_specific_centroid
    }

    #### Append
    if((nrow(sdf_out_specific) >= 1) & (nrow(sdf_out_general) >= 1)){
      sdf_out <- list(sdf_out_specific, sdf_out_general) %>% do.call(what="rbind")
    } else if ((nrow(sdf_out_specific) >= 1) & (nrow(sdf_out_general) >= 0)){
      sdf_out <- sdf_out_specific
    } else if ((nrow(sdf_out_specific) >= 0) & (nrow(sdf_out_general) >= 1)){
      sdf_out <- sdf_out_general
    }

  }

  return(sdf_out)
}

# Restrict landmarks by location
#
# @noRd
# @export
restrict_landmarks_by_location <- function(landmark_match,
                                           landmark_gazetteer,
                                           sdf,
                                           dist_thresh = 500){ # CHANGE BACK TO 500

  # Subset landmark locations by other locations (eg, roads / areas)

  ## Spatiall set landmarks
  #landmark_match_sp <- merge(landmark_gazetteer, landmark_match,
  #                           by.x = "name",
  #                           by.y = "matched_words_correct_spelling",
  #                           all.x=F)
  landmark_match_sp <- landmark_gazetteer[landmark_gazetteer$name %in%
                                            landmark_match$matched_words_correct_spelling,]

  ## Spatiall prep sdf
  sdf <- sdf %>%
    dplyr::mutate(id = 1) %>%
    group_by(.data$id) %>%
    dplyr::summarise(geometry = st_union(.data$geometry)) %>%
    ungroup()

  dist_road <- st_distance(landmark_match_sp, sdf) %>% as.numeric()

  uids_to_remove <- landmark_match_sp$uid[dist_road > dist_thresh]

  ## If not removing everything, then subset
  if(length(uids_to_remove) < nrow(landmark_match_sp)){
    landmark_gazetteer <- landmark_gazetteer[!(landmark_gazetteer$uid %in% uids_to_remove),]

    landmark_match_sp <- landmark_match_sp[!(landmark_match_sp$uid %in% uids_to_remove),]
    landmark_match <- landmark_match[landmark_match$matched_words_correct_spelling %in% landmark_match_sp$name,]
  }

  return(list(landmark_match = landmark_match,
              landmark_gazetteer = landmark_gazetteer))
}

# Restrict to tier 1 landmarks
#
# @noRd
# @export
restrict_gaz_tier1_landmarks <- function(landmark_match,
                                         landmark_gazetteer){

  # For each landmark, determines whether it is close to the location of a
  # tier 1 landmark (excluding itself). Restrict to close loctions. If no
  # locations close, return the original set of locations (no restriction).
  # Only changes the gazetteer; doesn't eliminate landmarks. In short,
  # finds where overlap.

  landmark_gazetteer_gs <- merge(landmark_gazetteer,
                                 landmark_match %>% distinct(.data$matched_words_correct_spelling, .keep_all=T),
                                 by.x = "name",
                                 by.y = "matched_words_correct_spelling",
                                 all.x = F)

  uids_original <- landmark_gazetteer_gs$uid

  tier1_locs <- landmark_gazetteer_gs[landmark_gazetteer_gs$prepos_before_crashword_tier_1 %in% T,]

  if(length(unique(tier1_locs$name)) >= 2){ # doesn't make sense if just 1

    ## Loop through landmark names
    lndmrk_gaz_new <- lapply(unique(landmark_gazetteer_gs$name), function(name){
      ## Restrict to ith landmark
      landmark_gazetteer_gs_i <- landmark_gazetteer_gs[landmark_gazetteer_gs$name %in% name,]

      ## From tiered list, remove if same name as ith landmark
      tier1_locs_i <- tier1_locs[!(tier1_locs$name %in% landmark_gazetteer_gs_i$name),]
      if(nrow(tier1_locs_i) >= 1){
        #tier1_locs_i <- gBuffer(tier1_locs_i, width = 1000, byid=F)

        tier1_locs_i <- tier1_locs_i %>%
          st_union() %>%
          st_buffer(dist = 1000) %>%
          st_as_sf() %>%
          dplyr::rename(geometry = .data$x)
        tier1_locs_i$id <- 1

        landmark_gazetteer_gs_i_cand <- landmark_gazetteer_gs_i[as.vector(st_intersects(landmark_gazetteer_gs_i, tier1_locs_i, sparse = F)),]
        if(nrow(landmark_gazetteer_gs_i_cand) > 0){
          landmark_gazetteer_gs_i <- landmark_gazetteer_gs_i_cand
        }
      }

      return(landmark_gazetteer_gs_i)
    }) %>%
      do.call(what = "rbind")

    uids_to_remove <- setdiff(uids_original, lndmrk_gaz_new$uid)

    landmark_gazetteer <- landmark_gazetteer[!(landmark_gazetteer$uid %in% uids_to_remove),]

  }

  return(landmark_gazetteer)
}

# If Location has a small extent, make a point
#
# @noRd
# @export
make_point_small_extent <- function(sdf,
                                    dist_tresh = 500){

  sdf_extent <- extent(sdf)

  max_extent_dist <- as.numeric(geodist(x = c(sdf_extent@xmin,
                                              sdf_extent@ymin),
                                        y = c(sdf_extent@xmax,
                                              sdf_extent@ymax)))

  if(max_extent_dist < dist_tresh){
    #sdf <- sdf %>% gCentroid()

    sdf <- sdf %>%
      st_union() %>%
      st_centroid() %>%
      st_as_sf() %>%
      dplyr::rename(geometry = .data$x)

    sdf$id <- 1 # dummy variable to make spatial dataframe
  }

  return(sdf)
}


##### ******************************************************************** #####
# SEARCHING FOR LOCATIONS ------------------------------------------------------

# Check if phrase is in sentence with an exact match
#
# @noRd
# @export
phrase_in_sentence_exact <- function(sentence,
                                     phrase_list){

  # Description: Determines which words or phrases are in a sentence using an exact match
  # sentence: sentence to examine
  # phrase_list: list of phrases to check if in sentence

  # Grabs landmarks in sentence, regardless of word boundaries (faster than checking with boundaries)
  locations_candidates <- sentence %>%
    str_extract_all(fixed(phrase_list)) %>%
    unlist() %>%
    unique

  if(length(locations_candidates) > 0){

    # Add word boundaries
    locations_candidates <- paste0("\\b", locations_candidates, "\\b")

    locations_df <- sentence %>%
      str_extract_all(locations_candidates) %>%
      unlist() %>%
      unique %>%
      as.data.frame %>%
      dplyr::rename(matched_words_text_spelling = ".") %>%
      dplyr::mutate(matched_words_correct_spelling = .data$matched_words_text_spelling) %>%
      dplyr::mutate(exact_match = T)

  } else{
    locations_df <- data.frame(NULL)
  }

  return(locations_df)
}

# Check if phrase is in sentence using a fuzzy match, for one element
#
# @noRd
# @export
phrase_in_sentence_fuzzy_i <- function(sentence,
                                       phrase_list,
                                       fuzzy_match_landmark.min.word.length,
                                       fuzzy_match_landmark.dist,
                                       fuzzy_match_ngram_max,
                                       first_letters_same,
                                       last_letters_same,
                                       remove_words){
  # Description: Determine if a word or phrase (ie, ngram) is in a sentence using
  # a fuzzy match
  # sentence: sentence to examine
  # phrase_list: list of phrases to check if in sentence
  # fuzzy_match_landmark.min.word.length: excludes phrases below this number of characters
  # fuzzy_match_landmark.dist: maximum levenstein distance to allow phrases to match
  # fuzzy_match_ngram_max: number of ngrams to check if in sentence (equal or less than max words in phrase in phrase_list)
  # first_letters_same: only keep matches where first letters are the same
  # last_letters_same: only keep matches where last letters are the same

  #### Tweet preparation
  # Remove stop words (replace with other words to preserve fact that words are
  # between other words)

  remove_words_regex <- paste(paste0("\\b",remove_words,"\\b"), collapse="|")

  sentence <- sentence %>% str_replace_all(remove_words_regex, "thisisafillerwordpleaseignoremore") %>% str_squish

  #### Checks
  if(length(fuzzy_match_landmark.min.word.length) != length(fuzzy_match_landmark.dist)){
    stop("fuzzy_match_landmark.min.word.length and fuzzy_match_landmark.dist must be the same length")
  }

  phrase_list <- unique(phrase_list)

  #### Extract n-grams from sentence
  if(str_count(sentence, '\\w+') > 1){
    sentence_words <- ngram::ngram_asweka(sentence, min=1, max=fuzzy_match_ngram_max)
  } else{
    sentence_words <- sentence
  }

  ## Limit n-grams
  # Only consider n-grams that have more than X characters
  # Don't include certain words (eg, "road") in count
  sentence_words_nchar <- nchar(sentence_words)

  for(word_dont_consider in c("road", "rd", "street", "st", "avenue", "ave")){
    sentence_words_nchar[grepl(paste0("\\b", word_dont_consider, "\\b"), sentence_words)] <- sentence_words_nchar[grepl(paste0("\\b", word_dont_consider, "\\b"), sentence_words)] - nchar(word_dont_consider)
  }

  sentence_words <- sentence_words[sentence_words_nchar >= fuzzy_match_landmark.min.word.length]

  # Only consider n-grams that don't start with a stop word
  stopwords_regex <- paste0("^", stopwords() , "\\b") %>% paste(collapse="|")
  sentence_words <- sentence_words[!grepl(stopwords_regex, sentence_words)]

  sentence_words <- sentence_words[!grepl("thisisafillerwordpleaseignoremore",sentence_words)]

  #### Limit phrase_list to speed up matching
  # If say that first and last letters must be same, restrict to phrases where
  # this must be the case
  if(first_letters_same){
    first_letters_sentence <- sentence_words %>% str_sub(1,1)
    phrase_list <- phrase_list[str_sub(phrase_list,1,1) %in% first_letters_sentence]
  }

  if(last_letters_same){
    last_letters_sentence <- sentence_words %>% str_sub(-1,-1) %>% unique()

    # Allow words to be plural. So if user wrote "towers" and correct word is
    # "tower", we remove the "s" and keep "r" as an allowed last letter. For
    # below code, if no "s", will give same letter as above.
    last_letters_sentence_ignore_s <- sentence_words %>% str_replace_all("s$", "") %>% str_sub(-1,-1) %>% unique()

    last_letters_sentence <- c(last_letters_sentence,
                               last_letters_sentence_ignore_s) %>%
      unique()

    phrase_list <- phrase_list[str_sub(phrase_list,-1,-1) %in% last_letters_sentence]
  }

  # Levenstein Distance
  phrase_match_loc <- amatch(sentence_words, phrase_list, maxDist = fuzzy_match_landmark.dist, method="lv")

  # Check if any matches found
  if(sum(!is.na(phrase_match_loc)) > 0){

    # Locations Dataframe
    locations_df <- data.frame(matched_words_text_spelling = sentence_words[!is.na(phrase_match_loc)],
                               matched_words_correct_spelling = phrase_list[phrase_match_loc[!is.na(phrase_match_loc)]],
                               exact_match = F)

    # Restrict to ones where first letter is the same
    if(first_letters_same == TRUE){

      first_letter_tweet_spelling <- locations_df$matched_words_text_spelling %>% str_sub(1,1)
      first_letter_correct_spelling <- locations_df$matched_words_correct_spelling %>% str_sub(1,1)

      locations_df <- locations_df[first_letter_tweet_spelling %in% first_letter_correct_spelling,]

    }

    # Restrict to ones where last letter is the same
    if(last_letters_same == TRUE){

      last_letter_tweet_spelling <- locations_df$matched_words_text_spelling %>% str_sub(-1,-1)
      last_letter_correct_spelling <- locations_df$matched_words_correct_spelling %>% str_sub(-1,-1)

      locations_df <- locations_df[last_letter_tweet_spelling %in% last_letter_correct_spelling,]

    }

    # Format
    locations_df <- locations_df %>%
      dplyr::mutate(matched_words_text_spelling   = .data$matched_words_text_spelling %>% as.character(),
                    matched_words_correct_spelling = .data$matched_words_correct_spelling %>% as.character())

  } else{
    locations_df <- data.frame(NULL)
  }

  return(locations_df)
}

# Check if phrase is in sentence using a fuzzy match
#
# @noRd
# @export
phrase_in_sentence_fuzzy <- function(text_i,
                                     landmark_list,
                                     fuzzy_match_landmark.min.word.length,
                                     fuzzy_match_landmark.dist,
                                     fuzzy_match_ngram_max,
                                     first_letters_same,
                                     last_letters_same,
                                     remove_words){

  # Implements phrase_in_sentence_fuzzy_i, looping through different
  # values of fuzzy_match_landmark.min.word.length and fuzzy_match_landmark.dist

  df <- lapply(1:length(fuzzy_match_landmark.min.word.length), function(i){
    df_i <- phrase_in_sentence_fuzzy_i(text_i,
                                       landmark_list,
                                       fuzzy_match_landmark.min.word.length[i],
                                       fuzzy_match_landmark.dist[i],
                                       fuzzy_match_ngram_max,
                                       first_letters_same,
                                       last_letters_same,
                                       remove_words)
    return(df_i)
  }) %>%
    bind_rows %>%
    unique

  if(nrow(df) %in% 0){
    df <- data.frame(matched_words_text_spelling = character(),
                     matched_words_correct_spelling = character(),
                     exact_match = logical(),
                     stringsAsFactors = F)
  }

  return(df)
}

# Determine index of phrase in sentence
#
# @noRd
# @export
phrase_locate <- function(phrase, sentence){
  # Description: Takes a word or phrase and a sentence as input and returns the
  # start and end word location of the phrase within the sentence. For example,
  # if phrase is "garden city" and sentence is "crash near garden city involving
  # pedestrian", the function will return a dataframe with three variables:
  # word = "garden city", word_loc_min = 3, word_loc_min = 4.
  # phrase: phrase or word
  # sentence: sentence to search in

  # Check if phrase in sentence
  if(grepl(paste0("\\b",phrase,"\\b"), sentence)){

    # Sentence Word and Character Position
    sentence_words_loc <- strsplit(sentence," ") %>%
      as.data.frame #%>%
    #dplyr::rename(word = names(.)[1])
    names(sentence_words_loc)[1] <- "word"

    sentence_words_loc$word <- as.character(sentence_words_loc$word)
    sentence_words_loc$word_number <- 1:nrow(sentence_words_loc)
    sentence_words_loc$word_length <- nchar(sentence_words_loc$word)
    sentence_words_loc$word_char_start <- lapply(1:nrow(sentence_words_loc),
                                                 tweet_word_start_character,
                                                 sentence_words_loc) %>%
      unlist()

    phrase_char_start_end <- str_locate_all(sentence, phrase)[[1]]

    phrase_location_df <- lapply(1:nrow(phrase_char_start_end),
                                 function(i){
                                   df <- sentence_words_loc[sentence_words_loc$word_char_start >= phrase_char_start_end[i,][1] & sentence_words_loc$word_char_start <= phrase_char_start_end[i,][2],]
                                   df_out <- data.frame(word_loc_min = min(df$word_number),
                                                        word_loc_max = max(df$word_number))
                                   return(df_out)
                                 }) %>% bind_rows

    phrase_location_df$word <- phrase
  } else{
    warning("phrase is not in sentence")
    phrase_location_df <- data.frame(NULL)
  }

  if(nrow(phrase_location_df) %in% 0){
    phrase_location_df <- data.frame(word_loc_max = integer())
  }

  return(phrase_location_df)
}

# Determine starting character of word
#
# @noRd
# @export
tweet_word_start_character <- function(i, tweet_words_loc){
  # Supports: phrase_locate function
  if(i == 1){
    return(1)
  } else{
    return(sum(tweet_words_loc$word_length[1:(i-1)]) + i)
  }
}

# Extract location after words
#
# @noRd
# @export
extract_locations_after_words <- function(word_loc,
                                          text,
                                          landmarks){
  # DESCRIPTION: Searches for location references after words (typically
  # prepositions). Allows for partially matchine names.
  # ARGS:
  # word_loc: Index of the word (eg, preposition) in the text. For example,
  #           "accident near garden city", if the word for "word_loc" is "near",
  #           the index would be 2.
  # text: Text to search for locations
  # landmark_gazetteer: Spatial dataframe of landmarks with a "name" variable.

  ## Default - blank spatial polygons dataframe. Make sure has a variable
  # that typical output would include
  landmarks_out <- data.frame(NULL)

  # 1. Conditions to check for word ------------------------------------------
  # Check for conditions based on the next word after the propsotion. If one
  # of the following conditions exists, we don't consider words after the
  # preposition
  next_word <- word(text, word_loc+1)

  next_word_none <- is.na(next_word)
  next_word_ignoreword <- next_word %in% c(tm::stopwords("en"), "exit", "top", "bottom", "scene")
  next_word_short <- nchar(next_word) < 3

  if(!next_word_none & !next_word_ignoreword & !next_word_short){

    # 2. Grab gazeteer words after preposition -------------------------------

    ## Start with full gazeteer
    landmarks_subset <- landmarks

    ## Loop through words after preposition
    for(i in 1:10){

      word_i <- word(text, word_loc+i)
      if(is.na(word_i)) break

      # If first word after preposition, the gazetteer word must start with that word.
      # Restrict words in gazetteer, creating a temporary dataframe
      if(i == 1) landmarks_subset_candidate <- landmarks_subset[grepl(paste0("^", word_i, "\\b"),   landmarks_subset$name), ]
      if(i > 1)  landmarks_subset_candidate <- landmarks_subset[grepl(paste0("\\b", word_i, "\\b"), landmarks_subset$name), ]

      if(i == 1){
        landmarks_subset <- landmarks_subset_candidate
      }

      if(nrow(landmarks_subset_candidate) > 0){
        landmarks_subset <- landmarks_subset_candidate
      } else{
        break
      }

    }

    if(nrow(landmarks_subset) > 0){

      # 3. Subset selected words -----------------------------------------------
      # Keep landmarks with shortest word length. For example, if landmark
      # with fewest words has 2 words, we only keep landmarks with 2 words
      min_words <- min(str_count(landmarks_subset$name, "\\S+"))
      landmarks_subset <- landmarks_subset[str_count(landmarks_subset$name, "\\S+") %in% min_words,]

      # 4. Check for dominant cluster ------------------------------------------
      landmarks_subset <- extract_dominant_cluster(landmarks_subset)

      # 5. Format Output -------------------------------------------------------
      if(nrow(landmarks_subset) >= 1){

        landmarks_subset <- landmarks_subset %>%
          dplyr::rename(matched_words_correct_spelling = .data$name) %>%
          dplyr::mutate(exact_match = FALSE,
                        location_type = "landmark")

        ## Add tweet spelling
        # max_word_length <- landmarks_subset$matched_words_correct_spelling %>% str_count("\\S+") %>% max()

        landmarks_subset <- landmarks_subset %>%
          dplyr::mutate(matched_words_text_spelling = word(text,
                                                            word_loc + 1,
                                                            word_loc + i - 1))

        landmarks_out <- landmarks_subset %>%
          st_drop_geometry()
      }

    }
  } else{
    landmarks_out <- NULL
  }

  return(landmarks_out)
}

##### ******************************************************************** #####
# SUBSET LOCATIONS -------------------------------------------------------------

# Remove landmark entry by type of landmark
#
# @noRd
# @export
remove_gaz_by_type <- function(landmark_match,
                               landmark_gazetteer,
                               type_list){
  # For each landmark entry, grabs the entries in the gazetteer and restricts
  # gazetteer entry by type. If nrow=0, then keep original gazetteer entries
  # (no subsetting); if nrow>0, restrict to ones with type and checks if
  # new entry grouping should be considered general/specific

  type_regex <- type_list %>% unlist %>% as.vector() %>% paste(collapse = "|")

  # For each name, restrict to types. If nrow=0, use original; if nrow>0,
  # re-check dominan cluster and use that for gaz entry
  gaz_to_add <- lapply(unique(landmark_match$matched_words_correct_spelling), function(name){
    gaz_i <- landmark_gazetteer[landmark_gazetteer$name %in% name,]
    gaz_type_i <- gaz_i[grepl(type_regex, gaz_i$type),]

    if(nrow(gaz_type_i) > 0){

      gaz_i <- extract_dominant_cluster(gaz_type_i,
                                        return_general_landmarks = "all")
    }
    return(gaz_i)
  }) %>%
    do.call(what = "rbind")

  ### Replace entries
  # Remove exisitng
  landmark_gazetteer_rm <- landmark_gazetteer[!(landmark_gazetteer$name %in% landmark_match$matched_words_correct_spelling),]

  # Add new
  landmark_gazetteer <- list(landmark_gazetteer, gaz_to_add) %>% do.call(what = "rbind")

  return(landmark_gazetteer)
}

# Remove general landmarks
#
# @noRd
# @export
remove_general_landmarks <- function(landmark_match,
                                     landmark_gazetteer){

  # General landmarks are those with multiple names, are not close to each
  # other and there is no dominant cluster. These are more likely to have
  # spurious names (names not relevant for a location). These will only help
  # pinpoint a location if a road is mentioned. Consequently, we throw these
  # out if a road is not mentioned. If a road is mentioned, we restrict to ones
  # that are close to a road.

  # Note that a landmark with the same name can be both general and specific if
  # there is a dominant cluster: the landmarks in the dominant cluster are
  # specific, while the ones not are general.

  # Remove general landmarks EXCEPT from type list, then if still general, remove.

  # Remove general landmarks from:
  # (1) Landmark matched list
  # (2) Gazeteer

  landmark_gazetteer_gs <- merge(landmark_gazetteer,
                                 landmark_match %>% distinct(.data$matched_words_correct_spelling, .keep_all=T),
                                 by.x = "name",
                                 by.y = "matched_words_correct_spelling",
                                 all.x = F)

  # Pull out ones already considered general [TODO: Need to do?]
  #landmark_gazetteer_gs <- landmark_gazetteer_gs[landmark_gazetteer_gs$general_specific %in% "general",]
  landmark_gazetteer_gs <- extract_dominant_cluster_all(landmark_gazetteer_gs,
                                                        return_general_landmarks = "all")

  landmark_gazetteer_gs <- landmark_gazetteer_gs[landmark_gazetteer_gs$general_specific %in% "general",]

  # Re-determine general/specific
  if(nrow(landmark_gazetteer_gs) > 0){
    #landmark_gazetteer_gs <- extract_dominant_cluster_all(landmark_gazetteer_gs,
    #                                                      return_general_landmarks = "all")
    #landmark_gazetteer_gs <- landmark_gazetteer_gs[landmark_gazetteer_gs$general_specific %in% "general",]

    uids_to_remove <- landmark_gazetteer_gs$uid

    if(nrow(landmark_gazetteer_gs) > 0){

      # Remove from gazetteer
      landmark_gazetteer <- landmark_gazetteer[!(landmark_gazetteer$uid %in% uids_to_remove),]

      # Remove from landmarks
      landmark_match <- landmark_match[landmark_match$matched_words_correct_spelling %in%
                                         landmark_gazetteer$name,]

    }
  }
  return(list(landmark_match = landmark_match,
              landmark_gazetteer = landmark_gazetteer))
}

# Resolving name overlaps with landmarks, roads, and areas
#
# @noRd
# @export
landmark_road_overlap <- function(locations_in_tweet){
  # Landmark intersects with road OR area, choose road

  # If road name intersects with landmark name, remove landmark
  # airtel msa rd: landmark: "airtel mesa", road: "msa rd"

  if( (TRUE %in% (locations_in_tweet$location_type %in% c("landmark", "area") )) & (TRUE %in% (locations_in_tweet$location_type %in% "road")) ){

    # Road locations
    locations_in_tweet_roads <- locations_in_tweet[locations_in_tweet$location_type %in% "road",]
    road_locations <- lapply(1:nrow(locations_in_tweet_roads), function(i){
      road_locs <- locations_in_tweet_roads$word_loc_min[i]:locations_in_tweet_roads$word_loc_max[i]
      return(road_locs)
    }) %>% unlist

    locations_keep <- lapply(1:nrow(locations_in_tweet), function(i){

      keep <- TRUE

      if(locations_in_tweet$location_type[i] %in% c("landmark", "area") ){
        if(TRUE %in% (locations_in_tweet$word_loc_min[i]:locations_in_tweet$word_loc_max[i] %in% road_locations)){
          keep <- FALSE
        }
      }

      return(keep)
    }) %>% unlist

    locations_in_tweet <- locations_in_tweet[locations_keep,] %>% unique

  }

  return(locations_in_tweet)

}

# Preference exact matches over fuzzy matches
#
# @noRd
# @export
exact_fuzzy_overlap <- function(locations_in_tweet){
  # If exact intersects with fuzzy, choose exact
  if((TRUE %in% locations_in_tweet$exact_match) & (FALSE %in% locations_in_tweet$exact_match)){

    # Road locations
    locations_in_tweet_exactTRUE <- locations_in_tweet[locations_in_tweet$exact_match %in% TRUE,]
    exactTRUE_locations <- lapply(1:nrow(locations_in_tweet_exactTRUE), function(i){
      exactTRUE_locs <- locations_in_tweet_exactTRUE$word_loc_min[i]:locations_in_tweet_exactTRUE$word_loc_max[i]
      return(exactTRUE_locs)
    }) %>% unlist

    locations_keep <- lapply(1:nrow(locations_in_tweet), function(i){

      keep <- TRUE

      if(locations_in_tweet$exact_match[i] %in% FALSE){
        if(TRUE %in% (locations_in_tweet$word_loc_min[i]:locations_in_tweet$word_loc_max[i] %in% exactTRUE_locations)){
          keep <- FALSE
        }
      }

      return(keep)
    }) %>% unlist

    locations_in_tweet <- locations_in_tweet[locations_keep,] %>% unique

  }

  return(locations_in_tweet)
}

# Check for phrase overlaps
#
# @noRd
# @export
phase_overlap <- function(locations_in_tweet){
  # Phase within Phrase
  # If phrase within another phrase, choose longer one (pick "garden city mall" over "garden city").
  # If phrases are same word length, keep both
  locations_remove <- lapply(1:nrow(locations_in_tweet), function(i){
    phrase_in_longer_phrase <- ((locations_in_tweet[i,]$word_loc_min >= locations_in_tweet[-i,]$word_loc_min) &
                                  (locations_in_tweet[i,]$word_loc_max <= locations_in_tweet[-i,]$word_loc_max))

    same_start_end <- ((locations_in_tweet[i,]$word_loc_min == locations_in_tweet[-i,]$word_loc_min) &
                         (locations_in_tweet[i,]$word_loc_max == locations_in_tweet[-i,]$word_loc_max))

    phrase_in_longer_phrase[same_start_end] <- FALSE

    return(TRUE %in% phrase_in_longer_phrase)
  }) %>% unlist

  locations_in_tweet <- locations_in_tweet[!locations_remove,] %>% unique

  return(locations_in_tweet)
}

# Preference exact over fuzzy matches
#
# @noRd
# @export
exact_fuzzy_startendsame <- function(locations_in_tweet){

  # Same Start/End, choose correctly spelled
  # If phrase has same start/end location, only keep ones that are correctly spelled
  locations_keep <- lapply(1:nrow(locations_in_tweet), function(i){

    keep_location <- TRUE

    # Grab all landmarks that start/end same location
    same_start_end <- ((locations_in_tweet[i,]$word_loc_min == locations_in_tweet$word_loc_min) &
                         (locations_in_tweet[i,]$word_loc_max == locations_in_tweet$word_loc_max))

    # Check if one of landmarks with same start/end is spelled correctly;
    # only subset if that is the case
    if(TRUE %in% locations_in_tweet$exact_match[same_start_end]){
      keep_location <- locations_in_tweet$exact_match[i]
    }

    return(keep_location)

  }) %>% unlist

  locations_in_tweet <- locations_in_tweet[locations_keep,] %>% unique

  return(locations_in_tweet)
}

# Resolve name clashes between landmarks and roads
#
# @noRd
# @export
landmark_road_samename <- function(locations_in_tweet){
  # If same name both road and landmark, drop landmark
  if(("landmark" %in% locations_in_tweet$location_type) & ("road" %in% locations_in_tweet$location_type)){

    drop_if_landmark <- locations_in_tweet$matched_words_text_spelling %in%
      locations_in_tweet$matched_words_text_spelling[locations_in_tweet$location_type %in% "road"]

    locations_in_tweet <- locations_in_tweet[!(locations_in_tweet$location_type %in% "landmark" & drop_if_landmark),]
  }

  return(locations_in_tweet)
}

# Extract intersections
#
# @noRd
# @export
extract_intersections <- function(locations_in_tweet,
                                  roads,
                                  crs_distance,
                                  point_dist_thresh = 1000, # Larger than 500; add 50 in buffer, for ex
                                  road_buffer = 50){
  # DESCRIPTION: Finds intersections between roads and returns if points of
  # intersections are close together (ie, returns nothing if two roads
  # intersect in multiple places far apart)
  # ARGS:
  # locations_in_tweet: Dataframe of locations in tweet
  # roads: Roads shapefile
  # point_dist_thresh: Threshold at which intersection points must be close
  #                    together. (meters / projected units)
  # road_buffer: Width to buffer roads before determining intersections (default
  #              0). Allows cases where roads are close but technically no
  #              point of intersections. (meters / projected units)

  # Find Road Intersection
  # Iterates through all 2-road combinations and checks for intersections. Uses
  # the intersection if intersection points are close together.

  locations_in_tweet_roads <- locations_in_tweet[locations_in_tweet$location_type %in% "road",]

  if(nrow(locations_in_tweet_roads) >= 2){

    road_combn <- combn(nrow(locations_in_tweet_roads), 2)

    #### Define Function
    extract_road_intersection <- function(i){

      intersection_point <- data.frame(NULL)

      locations_in_tweet_roads_i <- locations_in_tweet_roads[road_combn[,i],]

      road_1 <- roads[tolower(roads$name) %in% locations_in_tweet_roads_i$matched_words_correct_spelling[1],]
      road_2 <- roads[tolower(roads$name) %in% locations_in_tweet_roads_i$matched_words_correct_spelling[2],]

      if(road_buffer > 0){
        road_1 <- st_buffer(road_1, dist=road_buffer)
        road_2 <- st_buffer(road_2, dist=road_buffer)
      }

      intersection_points <- st_intersection(road_1, road_2)

      # Check to make sure intersection points are close together. Roads could
      # intersect in multiple places, where then location is ambiguous
      if(!is.null(intersection_points)){

        # If maximum distance between points are close together, then use
        #intersection_points_extent <- extent(intersection_points)
        #max_points_dist <- sqrt((intersection_points_extent@xmin - intersection_points_extent@xmax)^2 + (intersection_points_extent@ymin - intersection_points_extent@ymax)^2)
        max_points_dist <- intersection_points %>%
          st_convex_hull() %>% # force everything to polygon
          st_make_valid() %>%
          st_union() %>% # one observation
          st_coordinates() %>%
          as.data.frame() %>%
          st_as_sf(coords = c("X", "Y"),
                   crs = crs_distance) %>%
          st_distance() %>%
          max() %>%
          as.numeric()

        if(max_points_dist < point_dist_thresh){

          intersection_point <- intersection_points %>%
            st_convex_hull() %>%
            st_make_valid() %>%
            st_union() %>%
            st_centroid() %>%
            st_as_sf() %>%
            dplyr::rename(geometry = .data$x) %>%
            st_coordinates() %>%
            as.data.frame() %>%
            dplyr::rename(lon = .data$X) %>%
            dplyr::rename(lat = .data$Y) %>%
            dplyr::mutate(road_correct_spelling_1 = locations_in_tweet_roads_i$matched_words_correct_spelling[1],
                          road_tweet_spelling_1 = locations_in_tweet_roads_i$matched_words_text_spelling[1],
                          road_correct_spelling_2 = locations_in_tweet_roads_i$matched_words_correct_spelling[2],
                          road_tweet_spelling_2 = locations_in_tweet_roads_i$matched_words_text_spelling[2])

          # head(intersection_point)
          #
          # intersection_point <- gCentroid(intersection_points)@coords %>%
          #   as.data.frame %>%
          #   dplyr::rename(lon = x) %>%
          #   dplyr::rename(lat = y) %>%
          #   dplyr::mutate(road_correct_spelling_1 = locations_in_tweet_roads_i$matched_words_correct_spelling[1],
          #                 road_tweet_spelling_1 = locations_in_tweet_roads_i$matched_words_text_spelling[1],
          #                 road_correct_spelling_2 = locations_in_tweet_roads_i$matched_words_correct_spelling[2],
          #                 road_tweet_spelling_2 = locations_in_tweet_roads_i$matched_words_text_spelling[2])
        }
      }

      return(intersection_point)
    }

    #### Implement Function; Grab Intersections
    road_intersections <- lapply(1:ncol(road_combn), extract_road_intersection) %>% bind_rows

    ## Spatially define
    if(nrow(road_intersections) > 0){

      road_intersections <- road_intersections %>%
        st_as_sf(coords = c("lon", "lat"),
                 crs = st_crs(roads))

      # coordinates(road_intersections) <- ~lon+lat
      # crs(road_intersections) <- CRS(as.character(roads@proj4string))
    }

    #### Add variable to "locations_in_tweet" if road is part of intersection
    # TODO

  } else{
    # Other parts check nrow(road_intersections), so make blank dataframe
    road_intersections <- data.frame(NULL)
  }

  return(road_intersections)

}

# Preference specific landmarks over general landmarks
#
# @noRd
# @export
pref_specific <- function(landmark_gazetteer,
                          landmark_match,
                          cluster_thresh = 0.624){

  # If landmark name has both general and specific, only keep specific; if only
  # has one type (general or specific), keep all. Only affects gazetteer, not
  # landmark

  landmark_gazetteer_gs <- merge(landmark_gazetteer,
                                 landmark_match %>% distinct(.data$matched_words_correct_spelling, .keep_all=T),
                                 by.x = "name",
                                 by.y = "matched_words_correct_spelling",
                                 all.x = F)

  landmark_gazetteer_gs <- extract_dominant_cluster_all(landmark_gazetteer_gs,
                                                        return_general_landmarks = "all",
                                                        cluster_thresh = cluster_thresh)
  landmark_gazetteer_gs <- landmark_gazetteer_gs %>%
    st_drop_geometry()

  uids_orig <- landmark_gazetteer_gs$uid

  gaz_new <- lapply(unique(landmark_gazetteer_gs$name), function(name){
    landmark_gazetteer_gs_i <- landmark_gazetteer_gs[landmark_gazetteer_gs$name %in% name,]

    # If 2 types (so both general and specific), restrict to specific. We don't
    # change if all general or all specific (ie, one type)
    N_gs_types <- landmark_gazetteer_gs_i$general_specific %>% unique %>% length()
    if(N_gs_types %in% 2){
      landmark_gazetteer_gs_i <- landmark_gazetteer_gs_i[landmark_gazetteer_gs_i$general_specific %in% "specific",]
    }

    return(landmark_gazetteer_gs_i)
  }) %>%
    bind_rows()

  uids_to_remove <- setdiff(uids_orig, gaz_new$uid)

  landmark_gazetteer <- landmark_gazetteer[!(landmark_gazetteer$uid %in% uids_to_remove),]

  return(landmark_gazetteer)
}

# Preference original names over augmented names
#
# @noRd
# @export
pref_orig_name_with_gen_landmarks <- function(landmark_gazetteer,
                                              landmark_match){
  # Preferences the original name versus parallel landmark versions of names in
  # cases of a general landmark. For example, if name found in tweet is "nairobi school",
  # and "nairobi school" is a general landmark, where possible restricts name
  # to cases where "nairobi school" was the original landmark name (not a
  # parallel landmark).

  landmark_gazetteer_gs <- merge(landmark_gazetteer,
                                 landmark_match %>% distinct(.data$matched_words_correct_spelling, .keep_all=T),
                                 by.x = "name",
                                 by.y = "matched_words_correct_spelling",
                                 all.x = F)

  # Pull out ones already considered general
  #landmark_gazetteer_gs <- landmark_gazetteer_gs[landmark_gazetteer_gs$general_specific %in% "general",]
  landmark_gazetteer_gs <- extract_dominant_cluster_all(landmark_gazetteer_gs,
                                                        return_general_landmarks = "all")
  landmark_gazetteer_gs <- landmark_gazetteer_gs[landmark_gazetteer_gs$general_specific %in% "general",]

  # Re-determine general/specific
  if(nrow(landmark_gazetteer_gs) > 0){
    #landmark_gazetteer_gs <- extract_dominant_cluster_all(landmark_gazetteer_gs,
    #                                                      return_general_landmarks = "all")
    #landmark_gazetteer_gs <- landmark_gazetteer_gs[landmark_gazetteer_gs$general_specific %in% "general",]

    landmark_gazetteer_gs <- landmark_gazetteer_gs %>%
      st_drop_geometry()

    uids_orig <- landmark_gazetteer_gs$uid

    # If there are general landmarks...
    if(nrow(landmark_gazetteer_gs) > 0){

      gaz_new <- lapply(unique(landmark_gazetteer_gs$name), function(name){

        landmark_gazetteer_gs_i <- landmark_gazetteer_gs[landmark_gazetteer_gs$name %in% name,]
        landmark_gazetteer_gs_i_temp <- landmark_gazetteer_gs_i[landmark_gazetteer_gs_i$name == landmark_gazetteer_gs_i$name_original,]
        if(nrow(landmark_gazetteer_gs_i_temp) > 0) landmark_gazetteer_gs_i <- landmark_gazetteer_gs_i_temp

        return(landmark_gazetteer_gs_i)
      }) %>%
        bind_rows()

      uids_to_remove <- setdiff(uids_orig, gaz_new$uid)

      landmark_gazetteer <- landmark_gazetteer[!(landmark_gazetteer$uid %in% uids_to_remove),]
    }
  }

  return(landmark_gazetteer)
}

# Preference original name over augment names
#
# @noRd
# @export
pref_type_with_gen_landmarks <- function(landmark_gazetteer,
                                         landmark_match,
                                         type_list){
  # Preferences the original name versus parallel landmark versions of names in
  # cases of a general landmark. For example, if name found in tweet is "nairobi school",
  # and "nairobi school" is a general landmark, where possible restricts name
  # to cases where "nairobi school" was the original landmark name (not a
  # parallel landmark).

  landmark_gazetteer_gs <- merge(landmark_gazetteer,
                                 landmark_match %>% distinct(.data$matched_words_correct_spelling, .keep_all=T),
                                 by.x = "name",
                                 by.y = "matched_words_correct_spelling",
                                 all.x = F)

  # Pull out ones already considered general [TODO: Need to do?]
  #landmark_gazetteer_gs <- landmark_gazetteer_gs[landmark_gazetteer_gs$general_specific %in% "general",]
  landmark_gazetteer_gs <- extract_dominant_cluster_all(landmark_gazetteer_gs,
                                                        return_general_landmarks = "all")
  landmark_gazetteer_gs <- landmark_gazetteer_gs[landmark_gazetteer_gs$general_specific %in% "general",]

  # Re-determine general/specific
  if(nrow(landmark_gazetteer_gs) > 0){
    #landmark_gazetteer_gs <- extract_dominant_cluster_all(landmark_gazetteer_gs,
    #                                                      return_general_landmarks = "all")
    #landmark_gazetteer_gs <- landmark_gazetteer_gs[landmark_gazetteer_gs$general_specific %in% "general",]

    landmark_gazetteer_gs <- landmark_gazetteer_gs %>%
      st_drop_geometry()

    uids_orig <- landmark_gazetteer_gs$uid

    # If there are general landmarks...
    if(nrow(landmark_gazetteer_gs) > 0){

      gaz_new <- lapply(unique(landmark_gazetteer_gs$name), function(name){

        df_gaz_i <- landmark_gazetteer_gs[landmark_gazetteer_gs$name %in% name,]

        if(length(type_list) > 0){

          for(type_tier_i in type_list){
            type_tier_i <- type_tier_i %>% paste(collapse="|")

            if(TRUE %in% grepl(type_tier_i, df_gaz_i$type)){
              df_gaz_i <- df_gaz_i[grepl(type_tier_i, df_gaz_i$type),]
            }

          }
        }

        return(df_gaz_i)
      }) %>%
        bind_rows()

      uids_to_remove <- setdiff(uids_orig, gaz_new$uid)

      landmark_gazetteer <- landmark_gazetteer[!(landmark_gazetteer$uid %in% uids_to_remove),]
    }
  }

  return(landmark_gazetteer)
}

##### ******************************************************************** #####
# ADD REGEX VARIABLES TO LOCATION DATA -----------------------------------------

# Search for text pattern
#
# @noRd
# @export
search_crashword_prepos <- function(text,
                                    location_words,
                                    crash_words,
                                    prepositions){
  # For each location determine whether, [crash_word] [preposition] [location_word]
  # pattern exists.

  crash_words_regex  <- paste(paste0("\\b",crash_words,"\\b"),collapse="|")
  prepositions_regex <- paste(paste0("\\b",prepositions,"\\b"),collapse="|")

  crashword_prepos_TF <- lapply(location_words, function(location){
    regex_expression <- paste0(paste0("(",crash_words_regex,")."),
                               paste0("(",prepositions_regex,")."),
                               location)
    fits_pattern <- grepl(regex_expression, text)
    return(fits_pattern)
  }) %>% unlist

  return(crashword_prepos_TF)
}

# Search for text pattern
#
# @noRd
# @export
search_crashword_other_prepos <- function(text,
                                          location_words,
                                          crash_words,
                                          prepositions,
                                          N_other_words=3){

  crash_words_regex  <- paste(paste0("\\b",crash_words,"\\b"),collapse="|")
  prepositions_regex <- paste(paste0("\\b",prepositions,"\\b"),collapse="|")

  TF <- lapply(location_words, function(location){

    regex_expression <- paste0(paste0("(",crash_words_regex,")."),
                               "(.*).",
                               paste0("(",prepositions_regex,")."),
                               location)
    fits_pattern <- grepl(regex_expression, text)

    ## Number of words in between
    if(fits_pattern){
      # remove all text after location and before crashword [MIGHT FAIL IF >2 CRASHWORDS]
      otherwords_numwords <- text %>%
        str_replace_all(paste0("(",location,").*"),"\\1") %>%
        str_replace_all(paste0(".*(",crash_words_regex,")"),"\\1") %>%
        str_replace_all(regex_expression,"\\2") %>%
        str_count("\\S+")

      if(otherwords_numwords > N_other_words){
        fits_pattern <- F
      }
    }

    return(fits_pattern)
  }) %>% unlist

  return(TF)
}

# Check if preposition is before a location word
#
# @noRd
# @export
search_prep_loc <- function(text, location_words, prepositions){

  # Preposition before location word

  prepositions_regex <- paste(paste0("\\b",prepositions,"\\b"),collapse="|")

  TF <- lapply(location_words, function(location){
    regex_expression <- paste0(paste0("(",prepositions_regex,")."),
                               location)
    fits_pattern <- grepl(regex_expression, text)
    return(fits_pattern)
  }) %>% unlist

  return(TF)
}

##### ******************************************************************** #####
# DETERMINE EVENT LOCATION -----------------------------------------------------

# Choose between multiple landmarks
#
# @noRd
# @export
choose_between_multiple_landmarks <- function(df_out,
                                              roads,
                                              roads_final,
                                              crs_distance){
  #### Function for strategy of dealing with multiple landmarks (different names)

  # 1. Restrict landmarks based on preposition tiers

  # 2. If road mentioned, restrict to landmarks near road
  if(nrow(roads_final) > 0){
    df_out_sp <- df_out

    df_out_sp <- df_out_sp %>%
      st_as_sf(coords = c("lon", "lat"),
               crs = crs_distance)

    # coordinates(df_out_sp) <- ~lon+lat
    # crs(df_out_sp) <- CRS(crs_distance)

    roads_in_tweet <- roads[roads$name %in% roads_final$matched_words_correct_spelling,]
    roads_in_tweet$id <- 1
    roads_in_tweet <- roads_in_tweet %>%
      group_by(.data$id) %>%
      dplyr::summarise(geometry = st_union(.data$geometry)) %>%
      ungroup()
    #roads_in_tweet <- raster::aggregate(roads_in_tweet, by="id")
    df_out_sp$distance_road <- as.numeric(st_distance(roads_in_tweet, df_out_sp))
    df_out_sp <- df_out_sp[df_out_sp$distance_road < 500,]

    if(nrow(df_out_sp) > 0){
      df_out <- df_out[df_out$matched_words_correct_spelling %in% df_out_sp$matched_words_correct_spelling,]
      df_out$how_determined_location <- paste(df_out$how_determined_location, "multiple_landmarks_restrict_to_near_roads", sep=";")
    } else{
      df_out$how_determined_location <- paste(df_out$how_determined_location, "multiple_landmarks_tried_restrict_to_near_roads_but_none_near_road", sep=";")
    }
  }

  # 3. Use landmark closest to crash word.
  # if there are two words, one with distance -2 and one with distance 2, which.min()
  # will just give the first one. Consequently, use another approach to grab both.
  # Only use if a crash word exists in tweet.
  if(is.null(df_out$dist_closest_event_word) %in% FALSE){
    landmark_closest_crashword <- df_out$matched_words_correct_spelling[abs(df_out$dist_closest_event_word) %in% min(abs(df_out$dist_closest_event_word))] %>% unique()
    df_out <- df_out[df_out$matched_words_correct_spelling %in% landmark_closest_crashword,]
    df_out$how_determined_location <- paste(df_out$how_determined_location, "multiple_landmarks_choose_closest_crashword", sep=";")
  }

  return(df_out)
}

# Choose between landmarks with the same name
#
# @noRd
# @export
choose_between_landmark_same_name <- function(df_out,
                                              roads,
                                              roads_final,
                                              type_list,
                                              crs_distance){
  #### Function for strategy of dealing with landmarks with same name, diff loc

  # 1. If multiple landmarks with same name and a road name, restrict to
  #    ones close to any road that is mentioned
  if(nrow(roads_final) >= 1){
    # Spatial dataframe of landmark candidates
    df_out_sp <- df_out

    df_out_sp <- df_out_sp %>%
      st_as_sf(coords = c("lon", "lat"),
               crs = crs_distance)

    # coordinates(df_out_sp) <- ~lon+lat
    # crs(df_out_sp) <- CRS(crs_distance)

    # Road shapefile of roads in tweet
    roads_in_tweet <- roads[roads$name %in% roads_final$matched_words_correct_spelling,]
    roads_in_tweet$id <- 1
    #roads_in_tweet <- raster::aggregate(roads_in_tweet, by="id")
    roads_in_tweet <- roads_in_tweet %>%
      group_by(.data$id) %>%
      dplyr::summarise(geometry = st_union(.data$geometry)) %>%
      ungroup()

    df_out$distance_road_in_tweet <- as.numeric(st_distance(df_out_sp, roads_in_tweet))

    if(TRUE %in% (df_out$distance_road_in_tweet < 500)){
      df_out <- df_out[df_out$distance_road_in_tweet < 500,]
      df_out$how_determined_location <- paste(df_out$how_determined_location, "restrict_landmarks_close_to_road", sep=";")
    } else{
      df_out$how_determined_location <- paste(df_out$how_determined_location, "tried_restricting_landmarks_close_to_road_but_none_close", sep=";")
    }

  }

  # 2. If multiple landmarks with same name, restrict by type
  if(length(type_list) > 0){

    for(type_tier_i in type_list){
      type_tier_i <- type_tier_i %>% paste(collapse="|")

      if(TRUE %in% grepl(type_tier_i, df_out$type)){
        df_out <- df_out[grepl(type_tier_i, df_out$type),]
        df_out$how_determined_location <- paste(df_out$how_determined_location, paste0("restrict_by_type_", type_tier_i), sep=";")
      }

    }
  }

  # 2. If multiple landmarks with same name, use one if bus station
  #if(TRUE %in% grepl("bus_station|transit_station|stage_added", df_out$type)){
  #  df_out <- df_out[grepl("bus_station|transit_station|stage_added", df_out$type),]
  #  df_out$how_determined_location <- paste(df_out$how_determined_location, "choose_bus_station", sep=";")
  #}

  # 3. If multiple landmarks with same name, use one if mall
  #if(TRUE %in% grepl("shopping_mall", df_out$type)){
  #  df_out <- df_out[grepl("shopping_mall", df_out$type),]
  #  df_out$how_determined_location <- paste(df_out$how_determined_location, "choose_shopping_mall", sep=";")
  #}

  # 3. If multiple landmarks with same name, use one with more types
  nrow_before <- nrow(df_out)
  df_out <- df_out[stri_count(df_out$type, fixed = ";") %in% max(stri_count(df_out$type, fixed = ";")),]
  nrow_after <- nrow(df_out)
  if(nrow_before > nrow_after) df_out$how_determined_location <- paste(df_out$how_determined_location, "choose_landmark_more_types", sep=";")

  return(df_out)
}

# Snap location to road
#
# @noRd
# @export
snap_landmark_to_road <- function(df_out,
                                  roads,
                                  roads_final,
                                  crs_distance){

  #df_out_sp <- df_out

  df_out_sp <- df_out %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = crs_distance)

  # coordinates(df_out_sp) <- ~lon+lat
  # crs(df_out_sp) <- CRS(crs_distance)

  roads_i <- roads[roads$name %in% roads_final$matched_words_correct_spelling,]

  if( min(as.numeric(st_distance(df_out_sp, st_union(roads_i)))) < 500){

    #crs_utm <- get_utm_epsg(df_out_sp[1,])

    roads_pts_i <- roads_i %>%
      st_coordinates() %>%
      as.data.frame() %>%
      st_as_sf(coords = c("X", "Y"),
               crs = crs_distance)

    snap_point_to_points <- function(point_sf, points_sf){

      dist_vec <- st_distance(point_sf, points_sf) %>% as.numeric()

      if(min(dist_vec) < 500){
        min_pt_index <- dist_vec %>% which.min()
        point_sf$geometry <- points_sf$geometry[min_pt_index]
      }

      return(point_sf)
    }

    df_out_sp <- map_df(1:nrow(df_out_sp), function(i){
      snap_point_to_points(df_out_sp[i,],
                           roads_pts_i)
    })

    # min_pt_index <- st_distance(df_out_sp, roads_pts_i) %>% which.min()
    # df_out_sp$geometry <- roads_pts_i$geometry[min_pt_index]

    df_out_sp_snap <- df_out_sp

    df_out_sp_snap$how_determined_location <- paste(df_out_sp_snap$how_determined_location, "snapped_to_road", sep=";")

    df_out_sp_snap <- df_out_sp_snap %>%
      dplyr::select(-c("lon", "lat")) %>%
      add_latlon_vars_to_sf()

    #df_out_sp_snap <- df_out_sp_snap %>%
    #  st_drop_geometry()
  } else{
    df_out_sp_snap <- df_out
    df_out_sp_snap$how_determined_location <- paste(df_out_sp_snap$how_determined_location, "tried_to_snapped_to_road_but_road_too_far", sep=";")
  }

  return(df_out_sp_snap)

}

# Find other landmarks that might be near road
#
# @noRd
# @export
find_landmark_similar_name_close_to_road <- function(df_out,
                                                     roads,
                                                     roads_final,
                                                     landmark_gazetteer,
                                                     crs_distance,
                                                     text_i,
                                                     direction = "next_words"){


  null_or_nrow0_returnNULL <- function(x){

    out <- x
    if(!is.null(x)){
      if(nrow(x) %in% 0) out <- NULL
    }

    return(out)
  }

  # Find other landmarks with similar name as landmarks in df_out that might
  # be near the road. Here, we start with the landmark names in df_out. If
  # they are far (more than 500 meters) from the mentioned road, this might
  # suggest that we have the incorrect landmark; the correct location is
  # probably near the mentioned road. Consequently, we broaden our landmark
  # search. Note that this step is different from the previous search
  # as it doesn't imply the landmark came before a preposition. This involves:
  # 1. Check whether landmark names are part of *any* part of gazetteer entries
  # 2. Of above landmarks, checks whether they are within 100 meters of mentioned road
  # 3. If more than one landmark found, check if all close together
  # 4. If the above conditions don't hold, we stay with the original landmarks

  df_out_sp <- df_out

  df_out_sp <- df_out_sp %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = crs_distance)

  # coordinates(df_out_sp) <- ~lon+lat
  # crs(df_out_sp) <- CRS(crs_distance)

  roads_i <- roads[roads$name %in% roads_final$matched_words_correct_spelling,]

  if( min(as.numeric(st_distance(df_out_sp, st_union(roads_i) ))) >= 500){

    #regex_search <- paste0("^", unique(df_out$matched_words_correct_spelling), "\\b") %>% paste(collapse="|")
    regex_search <- paste0("\\b", unique(df_out$matched_words_correct_spelling), "\\b") %>% paste(collapse="|")

    landmark_gazetteer_subset <- landmark_gazetteer[grepl(regex_search, landmark_gazetteer$name),]

    roads_i$id <- 1
    #roads_i <- aggregate(roads_i, by="id")
    roads_i <- roads_i %>%
      group_by(.data$id) %>%
      dplyr::summarise(geometry = st_union(.data$geometry)) %>%
      ungroup()

    landmark_gazetteer_subset$distance_road <- as.numeric(st_distance(landmark_gazetteer_subset, roads_i, byid=T))
    #landmark_gazetteer_subset$distance_road <- as.numeric(gDistance(landmark_gazetteer_subset, roads_i, byid=T))

    landmark_gazetteer_subset <- landmark_gazetteer_subset[landmark_gazetteer_subset$distance_road <= 100,]

    #landmark_gazetteer_subset$name[5] <- "laboratory and allied ltd"

    ##### If multiple close by, use one where mentioned word is at start of landmark
    # If none start a landmark, don't subset.
    # E.g., "accident at dbt mombasa rd" , following found: "dbt center" and "moneygram at dbt"; choose "dbt center"
    #regex_search_startstring <- paste0("^", unique(df_out$matched_words_correct_spelling), "\\b") %>% paste(collapse="|")

    #### NEW
    if(nrow(landmark_gazetteer_subset) > 0){
      #text_words <- text_i %>% words()
      text_words <- text_i %>% strsplit(split = " ") %>% unlist()

      if(direction %in% "next_words"){
        next_word_i <- df_out$word_loc_max[1] + 1
      } else{
        next_word_i <- df_out$word_loc_max[1] - 1
      }

      next_word <- text_words[next_word_i]
      if(length(next_word) %in% 0) next_word <- NA # if next_word_i = 0, for ex
      next_word_regex <- paste0("\\b", next_word, "\\b")

      landmark_gazetteer_subset_TEMP <- landmark_gazetteer_subset # initialize for while loop

      # landmark_gazetteer_subset_TEMP <<- landmark_gazetteer_subset_TEMP
      # text_words <<- text_words
      # next_word_i <<- next_word_i
      # next_word <<- next_word

      while((nrow(landmark_gazetteer_subset_TEMP) >= 1) & !is.na(next_word)){
        landmark_gazetteer_subset_TEMP <- landmark_gazetteer_subset[grepl(next_word_regex,landmark_gazetteer_subset$name),]
        if(nrow(landmark_gazetteer_subset_TEMP) >= 1) landmark_gazetteer_subset <- landmark_gazetteer_subset_TEMP


        if(direction %in% "next_words"){
          next_word_i <- next_word_i + 1
        } else{
          next_word_i <- next_word_i - 1
        }

        next_word <- text_words[next_word_i]

        if(length(next_word) %in% 0){
          next_word <- NA
        } else if (is.na(next_word)){
          next_word <- NA
        } else{
          next_word_regex <- paste0("\\b", next_word, "\\b")
        }

      }

      ## Check for dominant cluster
      dom_cluster <- extract_dominant_cluster(landmark_gazetteer_subset,
                                              collapse_specific_coords = T,
                                              return_general_landmarks = "none")
      dom_cluster <- null_or_nrow0_returnNULL(dom_cluster) # TODO: check so can remove

      # If not, restrict to where START with name....

      ## If there is no dominant cluster, restrict to where starts with landmark
      # name and check again
      if(is.null(dom_cluster)){
        regex_search_startstring <- paste0("^", unique(df_out$matched_words_correct_spelling), "\\b") %>% paste(collapse="|")
        landmark_gazetteer_subset <- landmark_gazetteer_subset[grepl(regex_search_startstring, landmark_gazetteer_subset$name),]

        if(nrow(landmark_gazetteer_subset) > 0){
          dom_cluster <- extract_dominant_cluster(landmark_gazetteer_subset,
                                                  collapse_specific_coords = T,
                                                  return_general_landmarks = "none")
          dom_cluster <- null_or_nrow0_returnNULL(dom_cluster) # TODO: check so can remove
        }
      }

      ## If there is a dominant cluster ...
      if(!is.null(dom_cluster)){

        coords <- st_centroid(dom_cluster)

        df_out$lat <- st_coordinates(coords)[[2]]
        df_out$lon <- st_coordinates(coords)[[1]]
        df_out$matched_words_correct_spelling <- paste(c(unique(df_out$matched_words_correct_spelling),unique(landmark_gazetteer_subset$name)), collapse=";")
        df_out$how_determined_location <- paste(df_out$how_determined_location, "broadended_landmark_search_found_landmarks_similar_name_near_road", sep=";")

      }
    }

  }

  return(df_out)

}

# Determine location from landmark
#
# @noRd
# @export
determine_location_from_landmark <- function(df_out,
                                             how_determined_text = "",
                                             landmark_gazetteer,
                                             roads,
                                             roads_final,
                                             type_list,
                                             crs_distance,
                                             text_i){

  df_out <- merge(df_out, landmark_gazetteer, by.x="matched_words_correct_spelling", by.y="name", all.x=T, all.y=F)

  df_out$how_determined_location <- how_determined_text

  df_out <- df_out %>%
    st_as_sf() %>%
    add_latlon_vars_to_sf()

  if(length(unique(df_out$matched_words_correct_spelling)) > 1) df_out <- choose_between_multiple_landmarks(df_out, roads, roads_final, crs_distance)
  if(nrow(df_out) > 1) df_out <- choose_between_landmark_same_name(df_out, roads, roads_final, type_list, crs_distance)
  if(nrow(roads_final) %in% 1) df_out <- find_landmark_similar_name_close_to_road(df_out, roads, roads_final, landmark_gazetteer, crs_distance, text_i, direction = "next_words")
  if(nrow(roads_final) %in% 1) df_out <- find_landmark_similar_name_close_to_road(df_out, roads, roads_final, landmark_gazetteer, crs_distance, text_i, direction = "previous_words")
  if(nrow(roads_final) %in% 1) df_out <- snap_landmark_to_road(df_out, roads, roads_final, crs_distance)

  df_out$type <- "landmark"

  if(is.null(df_out$dist_closest_event_word[1])){
    df_out$dist_closest_event_word <- NA
  }

  df_out <- df_out %>%
    st_as_sf() %>%
    add_latlon_vars_to_sf() %>%
    dplyr::select("lon", "lat", "matched_words_correct_spelling",
                  "matched_words_text_spelling", "type", "how_determined_location",
                  "dist_closest_event_word")

  return(df_out)
}

# Determine location from intersection
#
# @noRd
# @export
determine_location_from_intersection <- function(df_out,
                                                 how_determined_text = ""){

  df_out <- df_out %>%
    st_as_sf() %>%
    sf_to_df()

  df_out$matched_words_correct_spelling <- paste(df_out$road_correct_spelling_1, df_out$road_correct_spelling_2, sep=",")
  df_out$matched_words_text_spelling <- paste(df_out$road_tweet_spelling_1, df_out$road_tweet_spelling_2, sep=",")

  #df_out <- subset(df_out, select=c(lon, lat, matched_words_correct_spelling, matched_words_text_spelling))
  df_out <- df_out %>%
    add_latlon_vars_to_sf() %>%
    dplyr::select("lon", "lat", "matched_words_correct_spelling", "matched_words_text_spelling")
  df_out$type <- "intersection"
  df_out$how_determined_location <- how_determined_text

  return(df_out)
}






