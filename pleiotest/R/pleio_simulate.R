#' @title Create simulations
#' @description Example function to create simulations with no effects.
#' @param n_traits number of traits to simulate.
#' @param n_individuals number of individuals to simulate.
#' @param n_snp number of SNPs to simulate.
#' @param percentage_mv proportion of missing values. By default = 0.
#' @return a list with pheno and geno to test the pleioR function.
#' @examples
#' sim1 <- pleio_simulate(n_traits = 3, n_individuals = 1e4, n_snp = 1e3, percentage_mv = 0.1)
#' @author Original code by Fernando Aguate.
#' @export
pleio_simulate <- function(n_traits, n_individuals, n_snp, percentage_mv = 0){
  if (n_traits < 2) stop ('n_traits must be a number higher than 1')
  if (n_individuals < 5) stop ('n_individuals must be a number higher than 5')
  if (percentage_mv < 0 | percentage_mv > 1) stop ('percentage_mv must be a number between 0 and 1')

  y <- stats::rnorm(n_traits * n_individuals)
  x <- sapply(1:n_snp, function(i) stats::rbinom(n_individuals, 2, stats::runif(1, 0.01, .49)))
  rownames(x) <- paste0('ind', 1:n_individuals)
  colnames(x) <- paste0('snp', 1:ncol(x))
  trait <- rep(paste0('t', 1:n_traits), each = n_individuals)
  id <- paste0('ind', rep(1:n_individuals, n_traits))

  if (percentage_mv != 0){
    missing_values <- sample(1:length(y), round(length(y) * (1 - percentage_mv)))
    y <- y[-missing_values]
    trait <- trait[-missing_values]
    id <- id[-missing_values]
  }
  return(list(pheno = data.frame('id' = id, 'trait' = trait, 'y' = y),
              geno = x))
}
