
shiny_result <- function(profile, df, CI = 0.95, min_n = 75, grouping = "pop", select = NULL, tilt = FALSE, ...){
  ## build fixes : start ##
  x0 <- NULL
  X0 <- NULL
  X1 <- NULL
  p_value <- NULL
  n_obs <- NULL
  logP <- NULL
  z_score <- NULL
  . <- NULL
  ## build fixes : end ##
  grouping <- match.arg(grouping, c("pop", "meta", "cluster"))
  if(!(grouping %in% names(df))) return(NULL)
  grouping_ <- quo(!!sym(grouping))
  ##
  df_meta <- df %>% select(-ends_with("data")) %>% select(starts_with(grouping)) %>% distinct()
  df <- df %>% select(starts_with(grouping))
  # non_list_cols <- which(purrr::map_chr(df, class) != "list")
  ## char_class <- function(x) class(x)[1] == "character"
  ## non_list_cols <- which(purrr::map_lgl(df, char_class))
  df <- df %>% group_by(!!grouping_) %>% slice(1) %>% ungroup() %>% unnest(cols = ends_with("data"))
  group_vars <- grep(paste0("^",grouping), names(df_meta), value = TRUE)
  ## browser()
  ## If profile is simulated it contains X0 - if not, remoove X0 and X1 from reference
  join_by <- c("locus", "x0")
  if("X0" %in% names(profile)) join_by <- c(join_by, "X0")
  else df <- df %>% filter(x0 == X0) %>% select(-X0, -X1)
  df_profile <- inner_join(df, profile, by = join_by)
  ## filter in the number of individuals in each population
  df_profile <- df_profile %>% filter(n >= (2*min_n)) %>%  ## min_n is mininum number of individuals, n is 2*individuals
    group_by_(.dots = group_vars)
  if(nrow(df_profile)==0) return(NULL)
  ##
  if(any(grepl("hulls", names(df_profile)))){
    hulls <- df_profile %>% select_(.dots = c(group_vars, grep("hull", names(.), value = TRUE))) %>% 
      distinct_(.dots = group_vars, .keep_all = TRUE)
  }
  ## Compute the scores per population/meta 
  scores <- scores_pop(profile = df_profile, CI = CI, tilt = tilt, ...)
  scores <- scores %>% ungroup() %>% 
    mutate(
      accept = (p_value > (1-CI)),
      n = n/2, 
      labs = paste0(.[[1]]," (",n,")"),
      labs = fct_reorder(labs, logP)
    ) %>% 
    arrange(z_score)
  if(is.null(select)) select <- names(scores)
  scores <- scores %>% select_(.dots = select)
  if(grouping == "pop") scores <- scores %>% left_join(df_meta, by = names(.)[1:2])
  scores <- scores %>% select(which(purrr::map_chr(., class) == "character"), everything())
  if(any(grepl("hulls", names(df_profile)))) scores <- scores %>% inner_join(hulls, by = group_vars)
  scores
}


#' Likelihood ratio tests for AIMs
#'
#' Computes the likelihood ratio test statistics for each population in a database of reference populations.
#' @name genogeo
#' @param profile The AIMs profile encoded as returned by the \code{profile_AA_x0} function.
#' @param df The database of reference populations as returned by the \code{pops_to_DB} function.
#' @param CI The confidence level used to reject or accept the various hypotheses (between 0 and 1).
#' @param min_n Minimum number of individuals in each database sample
#' @param grouping should \code{"pop"} (the default) or \code{"meta"} be used for aggregating the results.
#' Can also be \code{"cluster"} if this variable is defined in the input database.
#' @param tilt Should exponential titling be used to obtain more accurate $p$-values in the distribution's tail
#' (currently not implemented)
#' @param ... Further arguments that are passed to other functions
#' @return A tibble containing the $z$-scores, $p$-values etc for each population.
#' @export
#' @examples
#' df_ <- simulate_pops(pop_n = 20, aims_n = 50)
#' df_db <- pops_to_DB(df_)
#' profile <- random_AIMs_profile(df_db, keep_pop = TRUE)
#' profile$pop[1] # The true population
#' result <- genogeo(profile[,c("locus","x0")], df = df_db)

genogeo <- function(profile, df, CI = 0.95, min_n = 75, grouping = "pop", tilt = FALSE, ...){
  if(is.list(profile) && !is.data.frame(profile)) profile <- profile$profile_x0
  shiny_result(profile = profile, df = df, CI = CI, min_n = min_n, grouping = grouping, tilt = tilt, ...)
}

