
#' Simulate random populations
#' 
#' 
#' @author Torben Tvedebrink \email{tvede@@math.aau.dk}
#' @param pop_n Number of populations to simulate
#' @param pop_names Their names. If NULL: The names are "pop_001" through "pop_pop_n"
#' @param pop_totals How many observations/sampled individuals per population. If one number this is used as parameter in a Poisson distribution
#' @param aims_n Number of AIMs
#' @param aims_names Their names. If NULL: The names are "rs_001" through "rs_aims_n"
#' @export

simulate_pops <- function(pop_n = 100, pop_names = NULL, pop_totals = NULL,
                          aims_n = 50, aims_names = NULL){
  freq <- NULL
  ## population names
  if(is.null(pop_names)) pop_names <- paste0("pop_", sprintf(paste0("%0",nchar(pop_n),"d"),1:pop_n))
  else pop_n <- length(pop_names)
  ## AIMs names
  if(is.null(aims_names)) aims_names <- paste0("rs_", sprintf(paste0("%0",nchar(aims_n),"d"),1:aims_n))
  else aims_n <- length(aims_names)
  ## Total counts per population
  if(is.null(pop_totals)) pop_totals <- 75 ## 75 per population
  if(length(pop_totals) == 1) pop_totals <- rpois(pop_n, pop_totals) ## Simulate
  ## Simulate allele frequencies
  populations <- tibble(
    pop = rep(pop_names, each = aims_n),
    population = rep(pop_names, each = aims_n),
    n = 2*rep(pop_totals, each = aims_n),
    lat = NA,
    lon = NA,
    locus = rep(aims_names, pop_n),
    freq = rbeta(n = pop_n*aims_n, shape1 = 10, shape2 = 10),
    x1 = rbinom(n = pop_n*aims_n, size = n, prob = freq)
  ) %>% select(-freq)
  populations
}

