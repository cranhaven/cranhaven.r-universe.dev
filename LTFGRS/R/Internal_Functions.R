utils::globalVariables("role")
utils::globalVariables("fid")
utils::globalVariables("indiv_ID")


#' Constructing age of onset (aoo)
#'
#' \code{construct_aoo} constructs the age of onset (aoo)
#' for a variable number of family members based on their
#' liability, disease status and current age.
#'
#' @param fam_mem A character vector holding all family members.
#' @param .tbl A tibble holding the liability as well as age and
#' disease status for the set of individuals in \code{fam_mem}.
#' @param pop_prev A positive number representing the population prevalence, i.e. the
#' overall prevalence in the population.
#' @param phen_name Either \code{NULL} or character vector holding the
#' phenotype name. Must be specified in the multi-trait case.
#' Defaults to \code{NULL}.
#'
#' @return A tibble holding all columns present in .tbl as well
#' as the age of onset or the current age
#' (depending on the disease status) for all individuals
#' given in \code{fam_mem}.
#'
#' @importFrom dplyr %>% rowwise select mutate bind_cols
#' @importFrom rlang :=
#' @noRd
construct_aoo <- function(fam_mem,.tbl, pop_prev, phen_name = NULL){

  # Removing the genetic component from the
  # set of family members, if it is present
  i_ind <- setdiff(fam_mem, c("g"))

  if(is.null(phen_name)){

    # Looping over all family members ind i_ind
    lapply(i_ind, function(j){

      # Selecting the liability, disease status and age for
      # individual j, in order to compute the age of onset.
      select(.tbl, c(tidyselect::matches(paste0("^",j,"$")), tidyselect::matches(paste0("^",j,"_[as].*$")))) %>%
        rowwise() %>%
        mutate(., !!as.symbol(paste0(j,"_aoo")) := ifelse(!!as.symbol(paste0(j,"_status")),
                                                          round(convert_liability_to_aoo(!!as.symbol(j), dist = "logistic", pop_prev = pop_prev, mid_point = 60, slope = 1/8)),
                                                          !!as.symbol(paste0(j,"_age")))) %>%
        select(., !!as.symbol(paste0(j,"_aoo")))
    }
    ) %>% do.call("bind_cols",.) %>% bind_cols(.tbl,.)

  }else{

    # Looping over all family members ind i_ind
    lapply(i_ind, function(j){

      # Selecting the liability, disease status and age for
      # individual j, in order to compute the age of onset.
      select(.tbl, tidyselect::starts_with(paste0(j, "_"))) %>%
        rowwise() %>%
        mutate(., !!as.symbol(paste0(j,"_", phen_name ,"_aoo")) := ifelse(!!as.symbol(paste0(j, "_", phen_name, "_status")),
                                                                          round(convert_liability_to_aoo(!!as.symbol(paste0(j, "_", phen_name)), dist = "logistic", pop_prev = pop_prev, mid_point = 60, slope = 1/8)),
                                                                          !!as.symbol(paste0(j,"_age")))) %>%
        select(., !!as.symbol(paste0(j,"_", phen_name ,"_aoo")))
    }
    ) %>% do.call("bind_cols",.) %>% bind_cols(.tbl,.)
  }
}


#' Computing thresholds
#'
#' \code{construct_thresholds} computes the upper and lower
#' thresholds for a variable number of family members based on their
#' disease status and current age or age of onset (depending on
#' the disease status).
#'
#' @param fam_mem A character vector holding all family members.
#' @param .tbl A tibble holding the family ID, disease status as well
#' as the age of onset or the current age
#' (depending on the disease status).
#' @param pop_prev A positive number representing the population prevalence, i.e. the
#' overall prevalence in the population.
#' @param phen_name Either \code{NULL} or character vector holding the
#' phenotype name. Must be specified in the multi-trait case.
#' Defaults to \code{NULL}.
#'
#' @return A tibble holding the personal identifier (PID) as well as
#' the lower and the upper threshold for all individuals
#' present in \code{fam_mem}.
#'
#' @importFrom dplyr %>% rowwise select mutate bind_rows ungroup
#' @noRd
construct_thresholds <- function(fam_mem, .tbl, pop_prev, phen_name = NULL){

  # Removing the genetic component from the
  # set of family members, if it is present
  i_ind <- setdiff(fam_mem, c("g"))

  if (!is.null(phen_name)) {

    # Looping over all family members ind i_ind
    lapply(i_ind, function(j){

      nbr <- which(i_ind == j)

      # Selecting the family ID, disease status and age/aoo for
      # individual j, in order to compute the thresholds.
      select(.tbl, c(fid,
                     tidyselect::matches(paste0(j, "_", phen_name, "_status")),
                     tidyselect::matches(paste0(j, "_", phen_name, "_aoo")))) %>%
        rowwise() %>%
        mutate(., indiv_ID = paste0(fid,"_", nbr),
               role = paste0(j),
               upper = convert_age_to_thresh(!!as.symbol(paste0(j, "_", phen_name, "_aoo")), dist = "logistic", pop_prev = pop_prev, mid_point = 60, slope = 1/8),
               lower = ifelse(!!as.symbol(paste0(j, "_", phen_name, "_status")),
                              convert_age_to_thresh(!!as.symbol(paste0(j, "_", phen_name, "_aoo")), dist = "logistic", pop_prev = pop_prev, mid_point = 60, slope = 1/8),
                              -Inf)) %>%
        rename(., !!as.symbol(paste0("lower_", phen_name)) := lower, !!as.symbol(paste0("upper_", phen_name)) := upper) %>%
        select(., fid, indiv_ID, role, starts_with("lower"), starts_with("upper")) %>%
        ungroup()

    }) %>% do.call("bind_rows",.)

  } else {

    # Looping over all family members ind i_ind
    lapply(i_ind, function(j){

      nbr <- which(i_ind == j)

      # Selecting the family ID, disease status and age/aoo for
      # individual i, in order to compute the thresholds.
      select(.tbl, c(fid,
                     tidyselect::matches(paste0("^",j,"_status$")),
                     tidyselect::matches(paste0("^",j,"_aoo$")))) %>%
        rowwise() %>%
        mutate(., indiv_ID = paste0(fid,"_", nbr),
               role = paste0(j),
               upper = convert_age_to_thresh(!!as.symbol(paste0(j,"_aoo")), dist = "logistic", pop_prev = pop_prev, mid_point = 60, slope = 1/8),
               lower = ifelse(!!as.symbol(paste0(j,"_status")),
                              convert_age_to_thresh(!!as.symbol(paste0(j,"_aoo")), dist = "logistic", pop_prev = pop_prev, mid_point = 60, slope = 1/8),
                              -Inf)) %>%
        select(., fid, indiv_ID, role, starts_with("lower"), starts_with("upper")) %>%
        ungroup()

    }) %>% do.call("bind_rows",.)
  }
}

#'
#' Add in missing roles for proband
#'
#' This function adds missing roles for a proband and fills lower threshold values with -Inf and upper threshold values with Inf.
#'
#' @param temp_tbl tibble to add missing proband roles to; originates from .tbl of estimate_liability.
#' @param role name of role column
#' @param cur_roles values of the role column
#' @param cur_fid current family ID being worked on
#' @param pid name of column with personal IDs
#' @param fid name of column with family IDs
#' @param phen_names vector of phenotype names as given in .tbl of estimate_liability. Defaults to NULL (which is single trait).
#'
#' @return The provided temp_tbl object is returned, but with the missing "g" and/or "o" roles added, where -Inf and Inf values
#' have been used to fill the lower and upper threshold values. If phen_names is provided, a pair of upper and lower values is
#' provided for each entry in phen_names.
#'
#' @importFrom dplyr filter pull tibble %>% bind_rows
#' @noRd

add_missing_roles_for_proband = function(temp_tbl, role, cur_roles, cur_fid, pid, fid, phen_names = NULL) {
  # role types to check for, centered on proband
  to_check_for = c("g", "o")

  # roles is already calculated; are they present?
  to_be_added = setdiff(to_check_for,   cur_roles)
  present     = intersect(to_check_for, cur_roles)

  # if some present, extract individual ID, if not, get family ID
  if (length(present) > 0 ) {
    i_pid = (temp_tbl %>% filter(!!as.symbol(role) == present) %>% pull(!!as.symbol(pid)))[1]
  } else {
    i_pid = pull(temp_tbl, !!as.symbol(fid))[1]
  }
  # suffixes of roles to be added
  id_suffixes = paste0("_",to_be_added) %>% stringr::str_replace_all(., "_o", "")

  if ( is.null(phen_names) ) { # single trait
    # construct tibble with desired roles
    tibble(
      !!as.symbol(fid) := pull(temp_tbl, !!as.symbol(fid))[1],
      !!as.symbol(pid)    := paste0(i_pid, id_suffixes),
      !!as.symbol(role)   := to_be_added,
      lower = rep(-Inf, length(to_be_added)),
      upper = rep( Inf, length(to_be_added))
    ) %>%
      bind_rows(., temp_tbl)

  } else { # multi trait
    # constructs id rows, then adds lower and upper thresholds from phen_names provided
    tibble(
      !!as.symbol(fid) := pull(temp_tbl, !!as.symbol(fid))[1],
      !!as.symbol(pid)    := paste0(i_pid, id_suffixes),
      !!as.symbol(role)   := to_be_added
    ) %>%
      bind_cols(
        tibble(!!!c(stats::setNames(rep(-Inf, length(phen_names)), paste0("lower_", phen_names)),
                    stats::setNames(rep( Inf, length(phen_names)), paste0("upper_", phen_names))))) %>%
      bind_rows(
        .,
        temp_tbl
      )
  }
}




#' Title Internal Function used to extact input needed for liability estimation
#'
#' @param .tbl .tbl input from estimate_liability
#' @param cur_fid current family ID being worked on
#' @param h2 heritability value from estimate_liability
#' @param fid name of family ID column
#' @param pid name of personal ID column
#' @param role name of role column
#' @param add_ind Whether the genetic liability be added. Default is TRUE.
#'
#' @returns list with two elements: tbl (tibble with all relevant information) and cov (covariance matrix) estimated through construct_covmat()
#'
#' @export
extract_estimation_info_tbl = function(.tbl, cur_fid, h2, fid, pid, role, add_ind = TRUE) {
  # extract all with current family ID.
  temp_tbl = filter(.tbl, !!as.symbol(fid) == cur_fid)

  # Extract the personal numbers and roles for all family members
  pids  <- pull(temp_tbl, !!as.symbol(pid))
  roles <- pull(temp_tbl, !!as.symbol(role))

  # Constructing the covariance matrix.
  cov_obj <- construct_covmat(fam_vec = roles, n_fam = NULL, add_ind = add_ind, h2 = h2)

  # check for whether covariance matrix is positive definite
  # correct if needed.
  cov_PD = correct_positive_definite_simplified(covmat = cov_obj)
  cov = cov_PD$covmat

  # adding missing roles (of either g or o)
  if (add_ind) {
    temp_tbl = add_missing_roles_for_proband(temp_tbl = temp_tbl,
                                             role = role,
                                             cur_roles = roles,
                                             cur_fid = cur_fid,
                                             pid = pid,
                                             fid = fid)

  }

  # Now that we have extracted all the relevant information, we
  # only need to order the observations before we can run
  # Gibbs sampler, as g and o need to be the first two observations.

  first_indx <- match(c("g","o"), pull(temp_tbl, !!as.symbol(role)))
  other_indx <- setdiff(1:length(pull(temp_tbl, !!as.symbol(role))), first_indx)
  temp_tbl <- temp_tbl[c(first_indx, other_indx),]
  return(list(tbl = temp_tbl, cov = cov))
}


#' Title Internal Function used to extact input needed from graph input for liability estimation
#'
#' @param cur_fam_graph neightbourhood graph of degree n around proband
#' @param cur_fid proband ID
#' @param h2 heritability value from estimate_liability
#' @param pid Name of column of personal ID
#' @param add_ind Whether the genetic liability be added. Default is TRUE.
#'
#' @returns list with two elements: tbl (tibble with all relevant information) and cov (covariance matrix) estimated through graph_based_covariance_construction()
#'
#' @export
#'
extract_estimation_info_graph = function(cur_fam_graph, cur_fid, h2, pid, add_ind = TRUE) {
  # extract current (local) family graph and
  # construct covariance and extract threshold information from graph.
  cov_obj = graph_based_covariance_construction(pid = pid,
                                                cur_proband_id = cur_fid,
                                                cur_family_graph = cur_fam_graph,
                                                h2 = h2, add_ind = add_ind)
  # cov and temp_tbl are ordered during construction

  # check whether covariance matrix is positive definite
  # correct if needed.
  cov_PD = correct_positive_definite_simplified(covmat = cov_obj$covmat)

  return(list(tbl = cov_obj$temp_tbl, cov = cov_PD$covmat))
}
