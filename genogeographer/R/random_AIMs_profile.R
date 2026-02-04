## Simulate random profile from population
rand_profile <- function(df, grouping = "pop", population = NULL, n = FALSE, keep_pop = FALSE){ ## x: freq og pop=char
  ## build fixes : start ##
  meta <- NULL
  pop <- NULL
  x0 <- NULL
  X0 <- NULL
  x1 <- NULL
  locus <- NULL
  p <- NULL
  p0 <- NULL
  p1 <- NULL
  p2 <- NULL
  . <- NULL
  ## build fixes : end ##
  stopifnot(length(population) <= 1)
  grouping <- match.arg(grouping, c("pop", "meta"))
  grouping_ <- quo(!!sym(grouping))
  ## 
  df <- df %>% select(starts_with(grouping)) %>% 
    distinct(!!grouping_, .keep_all = TRUE) %>% unnest(cols = ends_with("data")) %>% 
    ungroup()
  if(grouping == "meta") df <- df %>% rename(pop = meta)
  ## 
  if(!is.null(population)){
    if(any(population %in% df$pop)) df <- filter_(df, .dots = paste0("pop == '", population,"'"))
    else cat(paste0("Population: '",population,"' is not in database with grouping '",grouping,"'"))
  } 
  else{
    pops_n <- df %>% select(pop, n) %>% distinct()
    if(!n) pops_n <- mutate(pops_n, n = 1)
    pops_n <- pops_n %>% sample_n(size = 1, replace = FALSE, weight = n) %>% select(pop)
    df <- right_join(df, pops_n, by = "pop")
  }
  ## We sample x0 ~ x1
  x0_profile <- df %>% filter(x0 == 0, X0 == 0) %>%
    mutate(p = x1/n) %>%
    select(pop, locus, p) %>%
    mutate(x0 = rbinom(n = nrow(.), size = 2, prob = p)) %>%
    select(pop, locus, x0)
  ## We sample x0 ~ x0|x0+x1
  # in each row px0_x contains the distribution of x0 | x0' + x1, where x0' is from x0_profile
  df <- inner_join(df, x0_profile, by = c("pop", "locus", "x0"))
  x0_x_profile <- df %>% filter(X0 == 0) %>%
    ## Now px0_x = c(P(x0 = 0 | x1 + x0), P(x0 = 1 | x1 + x0), P(x0 = 2 | x1 + x0))
    bind_cols(dist_x0_cond_x_(n = .$n, x = .$x0 + .$x1)) %>% 
    rowwise() %>% 
    mutate(X0 = sample(0:2, size = 1, prob = c(p0, p1, p2))) %>%
    select(pop, locus, x0, X0) %>% ungroup()
  if(keep_pop) return(x0_x_profile)
  x0_x_profile %>% select(-pop)
}

#' Simulate a random AIMs profile
#' 
#' Use the information from \code{pops_to_DB} to simulate a profile from a random or given population.
#' The sampling is done with respect to the null hypothesis, such that the total count is adjusted accordingly.
#' For further details see Tvedebrink et al (2018), Section 3.1 (Simulations).
#' 
#' @author Torben Tvedebrink \email{tvede@@math.aau.dk}
#' @param df Database of reference profiles as returned by \code{pops_to_DB}
#' @param grouping Simualte from \code{pop} (default) or \code{meta}.
#' @param population The population to sample from. If NULL chosen at random.
#' @param n Use numbers of samples as weights to choose the population randomly
#' @param keep_pop Keep information on population 
#' @export
random_AIMs_profile <- function(df, grouping = "pop", population = NULL, n = FALSE, keep_pop = FALSE){
  rand_profile(df = df, grouping = grouping, population = population, n = n, keep_pop = keep_pop)
}

