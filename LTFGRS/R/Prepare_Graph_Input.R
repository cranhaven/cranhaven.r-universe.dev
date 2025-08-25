utils::globalVariables("cip")
utils::globalVariables("all_combinations")
utils::globalVariables("children")
utils::globalVariables("comb")
utils::globalVariables("distances")
utils::globalVariables("from")
utils::globalVariables("nchildren")
utils::globalVariables("parent_id")
utils::globalVariables("ph")
utils::globalVariables("to")
utils::globalVariables("event_age")
utils::globalVariables("cip_pred")
utils::globalVariables("thr")
utils::globalVariables("attrs")
utils::globalVariables("data")

#' Attempts to convert the list entry input format to a long format
#'
#' @param family a tibble with two entries, family id and personal id. personal id should end in "_role", if a role column is not present.
#' @param threshs thresholds, with a personal id (without role) as well as the lower and upper thresholds
#' @param personal_id_col column name that holds the personal id
#' @param role_col column name that holds the role
#'
#' @return returns a format similar to \code{prepare_thresholds}, which is used by \code{estimate_liability}
#'
#' @examples
#' family <- data.frame(
#' fid = c(1, 1, 1, 1),
#' pid = c(1, 2, 3, 4),
#' role = c("o", "m", "f", "pgf")
#' )
#'
#' threshs <- data.frame(
#'   pid = c(1, 2, 3, 4),
#'   lower = c(-Inf, -Inf, 0.8, 0.7),
#'   upper = c(0.8, 0.8, 0.8, 0.7)
#' )
#'
#' convert_format(family, threshs)
#'
#' @export
convert_format = function(family, threshs, personal_id_col = "pid", role_col = NULL) {
  # standardising input -----------------------------------------------------

  #are there any list columns in family (list entry format)?
  which_list_columns = which(sapply(family, is.list))
  if (length(which_list_columns) > 0) {
    #updating fam to get rid of list columns
    family = tidyr::unnest(family, cols = names(which_list_columns))

    ###  checking if role is present in ID or separate column
    # if "_" is present, a role will be there too.
    if (any(stringr::str_detect(family[[personal_id_col]], "_"))) { #if true, extract role
      #split pid_role in two:
      family[[role_col]] = strsplit(family[[personal_id_col]], "_(?=[^_]+$)", perl = TRUE) %>% sapply(., function(x) x[2])
      family[[personal_id_col]] = strsplit(family[[personal_id_col]], "_(?=[^_]+$)", perl = TRUE) %>% sapply(., function(x) x[1])

      warning("We've tried converting from list entries to a long format internally. If you see this print, please run prepare_thresholds and use the .tbl input going forward! \n")
    } else {
      if (is.null(role_col)) stop("Please provide family roles for each family member. e.g. father(f), mother(m), siblings (s1-s9), etc.")
      stop("We weren't able to convert data input automatically. Please use prepare_thresholds and use the .tbl input!\n")
    }
  }
  .tbl = left_join(family, threshs, by = personal_id_col)
  return(.tbl)
}

#' Calculate (personalised) thresholds based on CIPs.
#'
#' This function prepares input for \code{estimate_liability} by calculating thresholds based on stratified cumulative incidence proportions (CIPs) with options for interpolation for ages between CIP values. Given a tibble with families and family members and (stratified) CIPs, personalised thresholds will be calculated for each individual present in \code{.tbl}. An individual may be in multiple families, but only once in the same family.
#'
#' @param .tbl Tibble with  family and personal id columns, as well as CIP_merge_columns and status.
#' @param CIP Tibble with population representative cumulative incidence proportions. CIP must contain columns from \code{CIP_merge_columns} and \code{cIP_cip_col}.
#' @param age_col Name of column with age at the end of follow-up or age at diagnosis for cases.
#' @param CIP_merge_columns The columns the CIPs are subset by, e.g. CIPs by birth_year, sex.
#' @param CIP_cip_col Name of column with CIP values.
#' @param Kpop Takes either "useMax" to use the maximum value in the CIP strata as population prevalence, or a tibble with population prevalence values based on other information. If a tibble is provided, it must contain columns from \code{.tbl} and a column named "K_pop" with population prevalence values. Defaults to "UseMax".
#' @param status_col Column that contains the status of each family member. Coded as 0 or FALSE (control) and 1 or TRUE (case).
#' @param lower_equal_upper Should the upper and lower threshold be the same for cases? Can be used if CIPs are detailed, e.g. stratified by birth year and sex.
#' @param fid_col Column that contains the family ID.
#' @param personal_id_col Column that contains the personal ID.
#' @param personal_thr Should thresholds be based on stratified CIPs or population prevalence?
#' @param interpolation Type of interpolation, defaults to NULL.
#' @param bst.params List of parameters to pass on to xgboost. See xgboost documentation for details.
#' @param min_CIP_value Minimum cip value to allow. Too low values may lead to numerical instabilities.
#' @param xgboost_itr Number of iterations to run xgboost for.
#'
#'
#' @importFrom stats qnorm predict
#' @importFrom dplyr all_of mutate select %>% left_join group_by ungroup arrange across
#'
#' @return Tibble with (personlised) thresholds for each family member (lower & upper), the calculated cumulative incidence proportion for each individual (K_i), and population prevalence within an individuals CIP strata (K_pop; max value in stratum). The threshold and other potentially relevant information can be added to the family graphs with \code{familywise_attach_attributes}.
#'
#' @examples
#' tbl = data.frame(
#' fid = c(1, 1, 1, 1),
#' pid = c(1, 2, 3, 4),
#' role = c("o", "m", "f", "pgf"),
#' sex = c(1, 0, 1, 1),
#' status = c(0, 0, 1, 1),
#' age = c(22, 42, 48, 78),
#' birth_year = 2023 - c(22, 42, 48, 78),
#' aoo = c(NA, NA, 43, 45))
#'
#' cip = data.frame(
#' age = c(22, 42, 43, 45, 48, 78),
#' birth_year = c(2001, 1981, 1975, 1945, 1975, 1945),
#' sex = c(1, 0, 1, 1, 1, 1),
#' cip = c(0.1, 0.2, 0.3, 0.3, 0.3, 0.4))
#'
#' prepare_thresholds(.tbl = tbl, CIP = cip, age_col = "age", interpolation = NA)
#'
#' @export
prepare_thresholds = function(.tbl,
                              CIP,
                              age_col,
                              CIP_merge_columns = c("sex", "birth_year", "age"),
                              CIP_cip_col = "cip",
                              Kpop = "useMax",
                              status_col = "status",
                              lower_equal_upper = FALSE,
                              personal_thr = FALSE,
                              fid_col = "fid",
                              personal_id_col = "pid",
                              interpolation = NULL,
                              bst.params = list(
                                max_depth = 10,
                                base_score = 0,
                                nthread = 4,
                                min_child_weight = 10
                                ),
                              min_CIP_value = 1e-5,
                              xgboost_itr = 30
) {

# Checking input data -----------------------------------------------------
  # check if CIP_merge_columns are in both .tbl and CIP:
  if (any(!(CIP_merge_columns %in% colnames(.tbl)))) {
    stop(paste0("prepare_thresholds: The following columns are not present in the provided .tbl: ", paste(setdiff(CIP_merge_columns, colnames(.tbl)), collapse = ", ")))
  }

  if (any(!(CIP_merge_columns %in% colnames(CIP)))) {
    stop(paste0("prepare_thresholds: The following columns are not present in the provided CIP: ", paste(setdiff(CIP_merge_columns, colnames(CIP)), collapse = ", ")))
  }

  # checking whether the non-age columns used for merging in .tbl are present in CIP
  # this allows CIP to only hold relevant stratification info.
  # interpolation can be done on the age column.
  overlap_test_tbl = sapply(setdiff(CIP_merge_columns, age_col), function(x) {
    any(!(.tbl[[x]] %in% CIP[[x]]))
  })
  if ( any(overlap_test_tbl) ) {
    warning(paste0("prepare_thresholds: The following CIP_merge_columns are not completely overlapping in CIP and .tbl:",
                   paste(setdiff(CIP_merge_columns, age_col)[overlap_test_tbl], collapse = ", ")))
  }


# -------------------------------------------------------------------------


  # TODO:
  # ADD linear interpolation

  # interpolation with xgboost or merge on raw values?
  if (!is.na(interpolation) && interpolation != "xgboost") stop("Invalid choice of interpolation method. Must be NA or xgboost.")

  # interpolate CIP values based on xgb
  if (is.na(interpolation)) {

    # merge on raw values

    # Merging CIPs and assigning thresholds -----------------------------------
    .tbl = .tbl %>%
      dplyr::left_join(CIP, by = CIP_merge_columns) %>%
      dplyr::mutate(thr = qnorm(!!as.symbol(CIP_cip_col), lower.tail = FALSE),
                    lower = ifelse(!!as.symbol(status_col) == 1, thr, -Inf),
                    upper = ifelse(!!as.symbol(status_col) == 1,
                                   ifelse(lower_equal_upper, thr, Inf),
                                   thr))


    if (any(is.na(.tbl$lower)) | any(is.na(.tbl$upper))) {
      warning(paste0("There are ", sum(is.na(select(.tbl, lower, upper))), " NA values in the upper and lower thresholds. \n Do the age and age of onset values match the ages given in the CIPs?"))
    }

    if (any(.tbl[[age_col]] < 0)) {
      warning("prepare_thresholds: Some ages are negative. This may be due to the end of follow-up happening before the birth of an individual. Setting their thresholds to be uninformative. Please check the input data.")
      .tbl = .tbl %>%
        mutate(lower = ifelse(!!as.symbol(age_col) < 0, -Inf, lower),
               upper = ifelse(!!as.symbol(age_col) < 0, Inf, upper))
    }

    # returning formatted input -----------------------------------------------
    return(.tbl)


  } else if (interpolation == "xgboost") {


    # extract cip values
    y = CIP[[CIP_cip_col]]

    # force the remaining values in cur_cip to be a matrix
    X = as.matrix(select(CIP, all_of(c(CIP_merge_columns, age_col)), -!!as.symbol(CIP_cip_col)))
    # train xgboost
    xgb = xgboost::xgboost(X, y, nrounds = xgboost_itr, params = bst.params)


    # get the predicted CIP value based on the merge columns.
    .tbl$cip_pred = .tbl %>%
      select(all_of(c(CIP_merge_columns, age_col))) %>%
      as.matrix() %>%
      predict(xgb,.) %>%
      pmax(min_CIP_value)

    if (is.character(Kpop) && Kpop == "useMax") {
      .tbl = select(CIP, all_of(setdiff(CIP_merge_columns, age_col)), !!as.symbol(CIP_cip_col)) %>%
        group_by(across(all_of(all_of(setdiff(CIP_merge_columns, age_col))))) %>%
        summarise(K_pop = max(!!as.symbol(CIP_cip_col))) %>%
        ungroup() %>%
        left_join(.tbl, ., by = setdiff(CIP_merge_columns, age_col))


      .tbl = .tbl %>%
        group_by(across(all_of(setdiff(CIP_merge_columns, age_col)))) %>%
        arrange(!!as.symbol(age_col)) %>%
        mutate(cip_pred = cummax(cip_pred)) %>%
        ungroup() %>%
        mutate(thr = ifelse(rep(personal_thr, n()),
                            qnorm(cip_pred, lower.tail = FALSE),
                            qnorm(K_pop, lower.tail = FALSE)),
               lower = ifelse(!!as.symbol(status_col), thr, -Inf),
               upper = ifelse(!!as.symbol(status_col),
                              ifelse(lower_equal_upper, thr, Inf),
                              thr)) %>%
        rename(K_i = cip_pred)

    } else if (any(class(Kpop) %in% c("data.frame", "tibble", "matrix", "data.table", "tbl_df", "tbl"))) {

      if ( !("K_pop" %in% colnames(Kpop)) ) {
        stop("prepare_thresholds: Kpop must contain a column named 'K_pop' with population prevalence values for columns also present in .tbl.")
      }

      .tbl = left_join(.tbl, Kpop) %>%
        group_by(across(all_of(setdiff(CIP_merge_columns, age_col)))) %>%
        arrange(!!as.symbol(age_col)) %>%
        mutate(cip_pred = cummax(cip_pred)) %>%
        ungroup() %>%
        mutate(thr = ifelse(rep(personal_thr, n()),
                            qnorm(cip_pred, lower.tail = FALSE),
                            qnorm(K_pop, lower.tail = FALSE)),
               lower = ifelse(!!as.symbol(status_col), thr, -Inf),
               upper = ifelse(!!as.symbol(status_col),
                              ifelse(lower_equal_upper, thr, Inf),
                              thr)) %>%
        rename(K_i = cip_pred)

    } else {
      stop("prepare_thresholds: Kpop must be either 'useMax' or a tibble with columns matching the desired strata in .tbl and a column named 'K_pop' with population prevalence values.")
    }

    if (any(.tbl[[age_col]] < 0)) {
      warning("prepare_thresholds: Some ages are negative. This may be due to the end of follow-up happening before the birth of an individual. Setting their thresholds to be uninformative. Please check the input data.")
      .tbl = .tbl %>%
        mutate(lower = ifelse(!!as.symbol(age_col) < 0, -Inf, lower),
               upper = ifelse(!!as.symbol(age_col) < 0, Inf, upper))
    }

    if (any(is.na(.tbl$lower)) | any(is.na(.tbl$upper))) {
      warning(paste0("There are ", sum(is.na(select(.tbl, lower, upper))), " NA values in the upper and lower thresholds. \n Do the age and age of onset values match the ages given in the CIPs?"))
    }

    return(.tbl)

  } else {
    stop("unsupported interpolation method. Please use xgboost or NA.")
  }
}



#' Construct graph from register information
#'
#' \code{prepare_graph} constructs a graph based on mother, father, and offspring links.
#'
#' @param .tbl tibble with columns icol, fcol, mcol. Additional columns will be attributes in the constructed graph.
#' @param icol column name of column with proband ids.
#' @param fcol column name of column with father ids.
#' @param mcol column name of column with mother ids.
#' @param node_attributes tibble with icol and any additional information, such as sex, lower threshold, and upper threshold. Used to assign attributes to each node in the graph, e.g. lower and upper thresholds to individuals in the graph.
#' @param missingID_patterns string of missing values in the ID columns. Multiple values can be used, but must be separated by "|". Defaults to "^0$". OBS: "0" is NOT enough, since it relies on regex.
#'
#' @return An igraph object. A (directed) graph object based on the links provided in .tbl, potentially with provided attributes stored for each node.
#'
#' @importFrom dplyr %>% rename relocate mutate filter group_by summarise select bind_rows pull
#'
#' @examples
#' fam <- data.frame(
#'   id = c("pid", "mom", "dad", "pgf"),
#'   dadcol = c("dad", 0, "pgf", 0),
#'   momcol = c("mom", 0, 0, 0))
#'
#' thresholds <- data.frame(
#'   id = c("pid", "mom", "dad", "pgf"),
#'   lower = c(-Inf, -Inf, 0.8, 0.7),
#'   upper = c(0.8, 0.8, 0.8, 0.7))
#'
#' prepare_graph(fam, icol = "id", fcol = "dadcol", mcol = "momcol", node_attributes = thresholds)
#'
#' @export
prepare_graph = function(.tbl, icol, fcol, mcol, node_attributes = NA, missingID_patterns = "^0$") {

  # helper boolean to check if node_attributes is provided, since an offered node_attributes may have NA values in one or more
  # entries, hence the check on class of the object. NAs are logical and will lead to a FALSE.
  attachAttributes = any(class(node_attributes) %in% c("data.frame", "tibble", "matrix", "data.table", "tbl_df", "tbl"))

  # formatting .tbl from trio info to graph compatible input
  prep = .tbl %>%
    # making from column
    tidyr::pivot_longer(cols = c(!!as.symbol(fcol), !!as.symbol(mcol)),
                        values_to = "from") %>%
    # renaming id to "to"
    rename(to = !!as.symbol(icol)) %>%
    # reloacting to and from columns to first two columns
    select(from, to) %>% # directed graph -> order is important!
    # replacing "0"s with NA to ease later computation and reflect true data set
    # with missing / unknown links
    mutate(to = ifelse(str_detect(to, missingID_patterns), NA, to),
           from = ifelse(str_detect(from, missingID_patterns), NA, from))

  # remove connections with unknown links, i.e. only known links / edges
  parent_links = prep %>%
    filter(!is.na(to), !is.na(from)) %>%
    # ensure from and to are character vectors
    mutate(from = as.character(from),
           to = as.character(to))

  # we need to add a direct link between (full) siblings
  sibling_links = .tbl %>%
    select(!!as.symbol(icol), !!as.symbol(fcol), !!as.symbol(mcol)) %>%
    filter(str_detect(!!as.symbol(fcol), missingID_patterns, negate = TRUE),
           str_detect(!!as.symbol(mcol), missingID_patterns, negate = TRUE)) %>%
    mutate(parent_id = purrr::map2_chr(
      .x = !!as.symbol(fcol),
      .y = !!as.symbol(mcol),
      ~ paste0(sort(c(.x, .y)), collapse = "_")
    )) %>%
    group_by(parent_id) %>%
    summarise(children = list(!!as.symbol(icol))) %>%
    mutate(nchildren = sapply(children, length)) %>%
    filter(nchildren > 1) %>%
    mutate(all_combinations = purrr::map(.x = children, ~ get_all_combs(.x))) %>%
    # avoiding many duplicate rows by selecting only columns to unnest
    select(all_combinations) %>%
    tidyr::unnest(cols = c(all_combinations)) %>%
    mutate(ph = str_split(all_combinations, "_"),
           from = sapply(ph, function(x) x[1]),
           to = sapply(ph, function(x) x[2])) %>%
    select(from, to)

  # combining sibling and parent links
  if (nrow(sibling_links) > 0) {
    graph_input = bind_rows(parent_links, sibling_links)
  } else {
    graph_input = parent_links
  }

  # extract unique list of ids of individuals in graph input; graph_input has only 2 columns.
  present_ids = unlist(graph_input) %>% unique()

  # if node_attributes is provided, we will attach the node information to the graph.
  if (attachAttributes) {
    graph = igraph::graph_from_data_frame(d = graph_input,
                                          #use unique id list to attach threshold info
                                          vertices = filter(node_attributes, !!as.symbol(icol) %in% present_ids))
  } else {
    graph = igraph::graph_from_data_frame(d = graph_input)
  }

  # isolating potential solo nodes: where to or from column is NA
  # extracting only node names
  solo = prep %>%
    mutate(comb = purrr::map2_chr(.x = from,
                                  .y = to,
                                  ~ paste0(sort(c(.x, .y)), collapse = "_"))) %>%
    filter(str_detect(comb, "_", negate = TRUE)) %>%
    pull(comb)

  # all linked nodes; no NAs in to or from,
  # just a list of node names
  duos = graph_input %>%
    filter(!is.na(from) & !is.na(to)) %>%
    select(from, to) %>% unlist() %>% unique()

  # which nodes appear only in solo nodes, but not in duo nodes?
  # meaning they have no links
  solo_points = setdiff(solo, as.character(duos))
  # only run the below code if solo_points has any solo points to add.
  if (length(solo_points) > 0) {
    if (attachAttributes) {
      graph = igraph::add.vertices(graph, nv = length(solo_points), name = solo_points, attr = filter(node_attributes, !!as.symbol(icol) %in% solo_points))
    } else {
      graph = igraph::add.vertices(graph, nv = length(solo_points), name = solo_points)
    }
  }
  return(graph)
}




# -------------------------------------------------------------------------


# mindist and mode are inherited directly from make_neighborhood_graph
# the other parameters are used for selecting individuals and formatting

#' Automatically identify family members of degree n
#'
#' This function identifies individuals ndegree-steps away from the proband in the population graph.
#'
#' @param pop_graph Population graph from prepare_graph()
#' @param ndegree Number of steps away from proband to include
#' @param proband_vec Vector of proband ids to create family graphs for. Must be strings.
#' @param fid Column name of proband ids in the output.
#' @param fam_graph_col Column name of family graphs in the output.
#' @param mindist Minimum distance from proband to exclude in the graph (experimental, untested), defaults to 0, passed directly to make_neighborhood_graph.
#' @param mode Type of distance measure in the graph (experimental, untested), defaults to "all", passed directly to make_neighborhood_graph.
#'
#' @returns Tibble with two columns, family ids (fid) and family graphs (fam_graph_col).
#' @export
#'
#' @examples
#' # See Vignettes.
get_family_graphs = function(pop_graph, ndegree, proband_vec, fid = "fid", fam_graph_col = "fam_graph", mindist = 0, mode = "all") {
  ## TODO: mindist > 0, will get_covmat still work?

  # later computations with igraph seem to internally convert ids to strings
  # we will enforce string format for IDs here.
  if (typeof(proband_vec) != "character") {
    warning("get_family_graphs: proband_vec is not a character vector. Converting to character.")
    proband_vec = as.character(proband_vec)
  }


  tibble(
    !!as.symbol(fid) := proband_vec,
    !!as.symbol(fam_graph_col) := igraph::make_neighborhood_graph(pop_graph, order = ndegree, mindist = mindist, nodes = proband_vec, mode = mode)
  )
}



# tbl with start, end, and event as columns
# status_column is created and returned
# aod (age of diagnosis) is created and returned
# we assume event is the age of diagnosis, and NOT other events, such as death, emigration or other censoring events.
# start, end, event must be date columns.


#' Calculate age of diagnosis, age at end of follow up, and status
#'
#' @param tbl tibble with start, end, and event as columns
#' @param start start of follow up, typically birth date, must be a date column
#' @param end end of follow up, must be a date column
#' @param event event of interest, typically date of diagnosis, must be a date column
#' @param status_col column name of status column to be created. Defaults to "status".
#' @param aod_col column name of age of diagnosis column to be created. Defaults to "aod".
#' @param age_eof_col column name of age at end of follow-up column to be created. Defaults to "age_eof".
#'
#' @returns tibble with added status, age of diagnosis, and age at end of follow-up
#' @export
#'
#' @importFrom lubridate interval is.Date time_length
#' @importFrom dplyr %>% mutate
#' @importFrom rlang sym
#'
#' @examples
#' # See vignettes.
get_onset_time = function(tbl, start, end, event,
                          status_col = "status",
                          aod_col = "aod",
                          age_eof_col = "age") {
  # checks on date format is moved to familywise_censoring

  start_sym = sym(start)
  end_sym = sym(end)
  event_sym = sym(event)

  tbl %>%
    mutate(
      # status, censoring events (diagnosis) that happen outside of start and end dates
      !!sym(status_col) := ifelse(!is.na(!!event_sym) & (!!event_sym >= !!start_sym) & (!!event_sym <= !!end_sym), 1L, 0L),
      # age of diagnosis - NA if control
      !!sym(aod_col) := ifelse(
        !!sym(status_col) == 1L,
        as.numeric(difftime(!!event_sym, !!start_sym, units = "days")) / 365.25,
        NA_real_),
      # age at end of follow-up (as given)
      # start = birth date, returns age att diagnosis or eof
      !!sym(age_eof_col) := pmin( #which happens first?
        # aod from earlier
        !!sym(aod_col),
        # calculating age at end of follow up
        as.numeric(difftime(!!end_sym, !!start_sym, units = "days")) / 365.25,
        na.rm = TRUE
      )
    )

}


# Note, since no filtering is done on individuals, only censoring of onset times, it is possible for some individuals to have
# a negative age. This is due to the end of follow up happening before the birth of someone
# e.g. a parent (proband) is diagnosed in their teens, then gets a child later in life.
# The age of the child would then be the time interval between the time of diagnosis of the parent
# and the birth of the child ( the "start" is birth date, hence a negative time interval)

#' Censor onset times in a family based on a proband's end of follow-up.
#'
#' This function censors onset times for family members based on the proband's end of follow-up. This is done to prevent using future events to base predictions on.
#'
#' @param tbl tibble with info on family members, censoring events based on cur_proband in proband_id_col, must contain start, end, and event as columns
#' @param proband_id_col column name of proband ids within family
#' @param cur_proband current proband id
#' @param start start of follow up, typically birth date, must be a date column
#' @param end end of follow up, must be a date column
#' @param event event of interest, typically date of diagnosis, must be a date column
#' @param status_col column name of status column to be created. Defaults to "status.
#' @param aod_col column name of age of diagnosis (aod) column to be created. Defaults to "aod".
#' @param age_eof_col column name of age at end of follow-up (eof) column to be created. Defaults to "age_eof".
#'
#' @returns tibble with updated end times, status, age of diagnosis, and age at end of follow-up for a family, such that proband's end time is used as the end time for all family members. This prevents
#'  using future events to based predictions on.
#' @export
#'
#' @importFrom rlang sym
#' @importFrom dplyr %>% mutate filter pull
#'
#' @examples
#' # See Vignettes.
censor_family_onsets = function(tbl, proband_id_col, cur_proband, start, end, event,
                                status_col = "status",
                                aod_col = "aod",
                                age_eof_col = "age") {
  # TODO: This function may return negative ages. Should these people be removed? Negative ages are mostly due to the end of follow up happening before the birth of someone.
  # if event, start, or end is not a date column, throw error

  # checks on date format is moved to familywise_censoring

  # only doing these once
  start_sym = sym(start)
  end_sym = sym(end)
  event_sym = sym(event)
  proband_id_sym = sym(proband_id_col)

  # extracting proband's end of follow up time to use for the entire family
  proband_eof = tbl %>%
    filter(!!proband_id_sym == cur_proband) %>%
    pull(!!end_sym)

  # is only one proband eof identified?
  if (length(proband_eof) == 0) {
    stop(paste0("censor_family_onsets: proband '", cur_proband, "' not found in the provided tbl."))
  } else if (length(proband_eof) > 1) {
    stop(paste0("censor_family_onsets: proband '", cur_proband, "' has multiple end of follow-up dates. Please check the input data."))
  }

  tbl %>%
    mutate(
      # updating end time to be proband's end time if it is before current end time
      # this avoids using events after proband's eof (typically diagnosis, death, other censoring)
      !!end_sym := pmin(!!end_sym, proband_eof)
    ) %>%
    # get status, age of diagnosis, age at end of follow up, etc:
    get_onset_time(
      start = start,
      end = end,
      event = event,
      status_col = status_col,
      aod_col = aod_col,
      age_eof_col = age_eof_col
    )
}


#' Attach attributes to a family graphs
#'
#' This function attaches attributes to family graphs, such as lower and upper thresholds, for each family member. This allows for a user-friendly way to attach personalised thresholds and other per-family specific attributes to the family graphs.
#'
#' @param cur_fam_graph An igraph object (neighbourhood graph around a proband) with family members up to degree n.
#' @param cur_proband Current proband id (center of the neighbourhood graph).
#' @param fid Column name of family id.
#' @param attr_tbl Tibble with family id and attributes for each family member.
#' @param attr_names Names of attributes to be assigned to each node (family member) in the graph.
#' @param censor_proband_thrs Should proband's upper and lower thresholds be made uninformative? Defaults to TRUE. Used to exclude proband's information for prediction.
#'
#' @returns igraph object (neighbourhood graph around a proband) with updated attributes for each node in the graph.
#'
#' @export
attach_attributes = function(cur_fam_graph, cur_proband, fid, attr_tbl, attr_names, censor_proband_thrs = TRUE) {
  # get node names
  graph_vertex_names = igraph::vertex_attr(cur_fam_graph)$name

  # which nodes are present in thresholds?
  to_keep_indx = which(graph_vertex_names %in% attr_tbl[[fid]])
  # get names of present nodes
  to_keep = graph_vertex_names[to_keep_indx]

  # order attributes after graph
  attr_tbl_matched = attr_tbl %>% slice(match(to_keep, !!as.symbol(fid)))

  # any attr_names columns not in attr_tbl?
  if (any(!(attr_names %in% colnames(attr_tbl_matched)))) {
    warning(paste0("attach_attributes: Not all attributes are present in attr_tbl!\n",
                    " Missing attributes: ", paste(setdiff(attr_names, colnames(attr_tbl_matched)), collapse = ", ")))
  }

  # only include columns of attr_tbl that are in attr_names
  for (attr in attr_names) {
    cur_fam_graph <- igraph::set_vertex_attr(graph = cur_fam_graph, name = attr, value = attr_tbl_matched[[attr]])
  }

  # censor proband thresholds if requested
  if (censor_proband_thrs) {
    for (attr in str_subset(attr_names, "lower|upper")) {
      cur_fam_graph = igraph::set_vertex_attr(
        graph = cur_fam_graph,
        index = cur_proband,
        name = attr,
        value = case_when(
          str_detect(attr, "lower") ~ -Inf,
          str_detect(attr, "upper") ~ Inf,
          TRUE ~ NA
        )
      )
    }
  }

  return(cur_fam_graph)
}



#' Wrapper to attach attributes to family graphs
#'
#' This function can attach attributes to family graphs, such as lower and upper thresholds, for each family member. This allows for personalised thresholds and other per-family specific attributes.
#' This function wraps around attach_attributes to ease the process of attaching attributes to family graphs in the standard format.
#'
#' @param family_graphs tibble with family ids and family graphs
#' @param fam_attr tibble with attributes for each family member
#' @param fam_graph_col column name of family graphs in family_graphs. defailts to "fam_graph"
#' @param attached_fam_graph_col column name of the updated family graphs with attached attributes. defaults to "masked_fam_graph".
#' @param fid column name of family id. Typically contains the name of the proband that a family graph is centred on. defaults to "fid".
#' @param pid personal identifier for each individual in a family. Allows for multiple instances of the same individual across families. Defaults to "pid".
#' @param cols_to_attach columns to attach to the family graphs from fam_attr, typically lower and upper thresholds. Mixture input also requires K_i and K_pop.
#' @param censor_proband_thrs Should proband's upper and lower thresholds be made uninformative? Defaults to TRUE. Used to exclude proband's information for prediction.
#'
#' @returns tibble with family ids and an updated family graph with attached attributes. If lower and upper thresholds are specified, the input is ready for estimate_liability().
#'
#' @export
#'
#' @examples
#' # See Vignettes.
familywise_attach_attributes = function(family_graphs,
                                        fam_attr,
                                        fam_graph_col = "fam_graph",
                                        attached_fam_graph_col = "masked_fam_graph",
                                        fid = "fid",
                                        pid = "pid",
                                        cols_to_attach = c("lower", "upper"),
                                        censor_proband_thrs = TRUE) {
  fam_attr %>%
    tidyr::nest(attrs = -fid) %>%
    left_join(family_graphs, ., by = fid) %>%
    mutate(
      !!as.symbol(attached_fam_graph_col) := purrr::pmap(
        .l = list(!!as.symbol(fid), !!as.symbol(fam_graph_col), attrs),
        ~ attach_attributes(
          cur_fam_graph = ..2,
          attr_tbl = ..3, cur_proband = ..1,
          fid = pid, attr_names = cols_to_attach,
          censor_proband_thrs = censor_proband_thrs))) %>%
    select(-attrs, -!!as.symbol(fam_graph_col))
}




#' Censor Family Onsets for Multiple Families
#'
#' This fucntion is a wrapper around \code{censor_family_onsets}. This functions accepts a tibble with family graphs from \code{get_family_graphs}. It censors the onset times for each individual in the family graph based on the proband's end of follow-up.
#' Returns a formatted output.
#'
#' @param family_graphs Tibble with fid and family graphs columns.
#' @param tbl Tibble with information on each considered individual.
#' @param start Column name of start of follow up, typically date of birth.
#' @param end Column name of the personalised end of follow up.
#' @param event Column name of the event.
#' @param status_col Column name of the status (to be created). Defaults to "status".
#' @param aod_col Column name of the age of diagnosis (to be created). Defaults to "aod".
#' @param age_eof_col Column name of the age at the end of follow up (to be created). Defaults to "age_eof".
#' @param fam_graph_col Column name of family graphs in the 'family_graphs' object. Defaults to "fam_graph".
#' @param fid Family id, typically the name of the proband that a family graph is centred on. Defaults to "fid".
#' @param pid Personal identifier for each individual. Allows for multiple instances of the same individual across families. Defaults to "pid".
#' @param merge_by Column names to merge by. If different names are used for family graphs and tbl, a named vector can be specified: setNames(c("id"), c("pid")). Note id is the column name in tbl and pid is the column name in family_graphs. The column names used should reference the personal identifier.
#'
#' @returns A tibble with family ids and updated status, age of diagnosis, and age at end of follow-up for each individual in the family based on the proband's end of follow-up.
#' @export
#'
#' @examples
#' # See Vignettes.
familywise_censoring = function(
    family_graphs,
    tbl,
    start,
    end,
    event,
    status_col = "status",
    aod_col = "aod",
    age_eof_col = "age",
    fam_graph_col = "fam_graph",
    fid = "fid",
    pid = "pid",
    merge_by = pid) {


# Checking input ----------------------------------------------------------


  # check merge_by length:
  if (length(merge_by) != 1) {
    warning("familywise_censoring: merge_by is tested with a single column name or a named vector of length.")
  }

  # are the values in merge_by present in tbl and family_graphs?

  # tbl
  if (any(!(merge_by %in% colnames(tbl)))) {
    stop(paste0("familywise_censoring: The following columns are not present in the provided tbl: ", paste(setdiff(merge_by, colnames(tbl)), collapse = ", ")))
  }
  # note: it does not make sense to check for presence in family_graphs
  # since that column is created in this function.

  if (fid == pid) {
    stop("familywise_censoring: fid and pid cannot be the same. Please provide different column names.")
  }

  # are all 'fid's present in tbl?
  if (any( !(family_graphs[[fid]] %in% tbl[[merge_by]]))) {
    stop(paste0("familywise_censoring: The following family ids are not present in the provided tbl: ", paste(setdiff(family_graphs[[fid]], tbl[[merge_by]]), collapse = ", ")))
  }
  # we cannot perform the reverse check, as we do not expect all values in tbl to be present in family graphs in the same way.
  # instead we will extract IDs from the family graph and check that they are present in tbl.

  family_graphs = family_graphs %>%
    mutate(
      !!as.symbol(pid) := purrr::map(.x = !!as.symbol(fam_graph_col), ~ igraph::V(.x)$name)) %>%
    select(-!!as.symbol(fam_graph_col)) %>%
    tidyr::unnest(!!as.symbol(pid))

  # checking if all pid are present in tbl
  if (typeof(family_graphs[[pid]]) != typeof(tbl[[merge_by]])) {
    stop(paste0("familywise_censoring: The following columns are not of the same type: '", pid, "' in family_graphs and '", unname(merge_by), "' in tbl."))
  }

  # check that all family graphs pid are present in tbl
  if (any(!(unique(family_graphs[[pid]]) %in% tbl[[merge_by]]))) {
    stop(paste0("familywise_censoring: The following ids are not present in the provided family graphs (ids within fam_graph_col of family_graph): ", paste(setdiff(unique(family_graphs[[pid]]), tbl[[merge_by]]), collapse = ", ")))
  }

  # checking date format of tbl
  if (!is.Date(tbl[[start]])) stop(paste0("get_onset_time: start column '", start ,"' must be in a date format."))
  if (!is.Date(tbl[[end]]))   stop(paste0("get_onset_time: end column '", end ,"' must be in a date format."))
  if (!is.Date(tbl[[event]])) stop(paste0("get_onset_time: event column '", event ,"' must be in a date format."))


# Performing proband censoring per family  --------------------------------


  family_graphs %>%
    left_join(tbl, by = merge_by) %>%
    tidyr::nest(data = -fid) %>%
    # performing per-family operations:
    mutate(data = purrr::map2(.x = fid, .y = data,
                              ~ censor_family_onsets(
                                tbl = .y,
                                proband_id_col = pid,
                                cur_proband = .x,
                                start = start,
                                end = end,
                                event = event,
                                status_col = status_col,
                                aod_col = aod_col,
                                age_eof_col = age_eof_col))) %>%
    tidyr::unnest(cols = c(data))
}
