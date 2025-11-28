#' Create a dataset to fit models with all possible families in distreg packages
#'
#' @details This function creates a 3-dimensional uniform distribution (with
#'   support from 0 to 1) which has a cross-correlation of 0.5. Then the first
#'   dimension is transformed into a specified distribution (argument
#'   \code{fam_name}) via Inverse Transform Sampling
#'   \url{https://en.wikipedia.org/wiki/Inverse_transform_sampling}. The other
#'   two dimensions are transformed into a normal distribution (norm2) and
#'   a binomial distribution (binomial1, for testing categorical explanatory
#'   covariates). This procedure ensures that there is a dependency structure of
#'   the transformed first distribution and the other two.
#' @param nrow Number of observations of the exported dataset.
#' @param seed The seed which should be used, for reproducibility.
#' @param fam_name The name of the distribution family to which the first
#'   dimension of the uniform distribution should be transformed to.
#' @import bamlss
#' @import gamlss.dist
#' @importFrom betareg betareg
#' @importFrom stats rmultinom qbeta qbinom qgamma runif qpois qnorm qlnorm
#' @examples
#' # Beta distributed random values
#' model_fam_data(nrow = 500, fam_name = "BE")
#' @return A data.frame with columns for differently distributed data.
#' @export

model_fam_data <- function(nrow = 500, seed = 1408, fam_name = "NO") {

  # Set seed
  set.seed(seed)

  ## Get three equi-correlated uniform distributions
  t2u <- function(x) ifelse(x<1, x^2, 2-(2-x)^2)/2
  u_data <- data.frame(u1 = runif(nrow),
                       u2 = runif(nrow),
                       u3 = runif(nrow))
  u_data <- with(u_data, data.frame(v1 = t2u(u1+u2),
                                    v2 = t2u(u1+u3),
                                    v3 = t2u(u2+u3)))

  ### BAMLSS Families
  if (is.bamlss(fam_name)) {

    ## Multinomial
    if (fam_name == "multinomial") {
      probs <- with(u_data, cbind(cbind(v2, v3) %*% c(0, 0),
                                  cbind(v2, v3) %*% rnorm(2),
                                  cbind(v2, v3) %*% rnorm(2),
                                  cbind(v2, v3) %*% rnorm(2)))
      probs <- exp(probs)
      choices <- t(apply(probs, 1, rmultinom, size = 1, n = 1))
      mult_data <- apply(choices, 1, FUN = function(x) which(x == 1))

      # Transformed vector
      tfvec <- factor(mult_data, labels = c("one", "two", "three", "four"))
    }

    ## Gaussian
    if (fam_name %in% c("gaussian", "gaussian2", "Gaussian"))
      tfvec <- qnorm(u_data$v1)

    ## Beta
    if (fam_name == "beta")
      tfvec <- qbeta(u_data$v1, 2, 5)

    ## Binomial
    if (fam_name == "binomial")
      tfvec <- qbinom(u_data$v1, 1, 0.8)

    ## Cnorm
    if (fam_name == "cnorm")
      tfvec <- cnorm_bamlss()$q(u_data$v1, par = list(mu = 5, sigma = 2))

    ## Gamma
    if (fam_name == "gamma")
      tfvec <- qgamma(u_data$v1, shape = 9, scale = 0.5)

    ## Gpareto
    if (fam_name == "gpareto")
      tfvec <- gpareto_bamlss()$q(u_data$v1, list(sigma = 1, xi = 0.4))

    ## Poisson
    if (fam_name == "poisson")
      tfvec <- qpois(u_data$v1, 3)

    if (fam_name == "glogis")
      tfvec <- sapply(u_data$v1, FUN = glogis_bamlss()$q,
                      par = list(mu = 1, sigma = 1, alpha = 1))

    if (fam_name == "lognormal")
      tfvec <- qlnorm(u_data$v1)

    if (!exists("tfvec"))
      stop("Family not yet implemented")
  }

  ### GAMLSS Families
  if (is.gamlss(fam_name)) {

    # Get quantile function
    q_raw_name <- paste0("q", fam_name)
    qfun <- function(p)
      return(do.call(get(force(q_raw_name)),
                     args = list(p = p)))
    tfvec <- sapply(u_data$v1, FUN = qfun) # use apply here because I'm not sure if all q functions are vectorized
  }

  ### Betareg Family
  if (is.betareg(fam_name)) {
    tfvec <- qbeta(u_data$v1, 2, 5)
  }

  ## Piece everything together
  data <- data.frame(tfvec = tfvec,
                     binomial1 = factor(qbinom(u_data$v2, 1, 0.5), labels = c("yes", "no")),
                     norm2 = qnorm(u_data$v3, 10, 15))
  colnames(data)[1] <- fam_name

  ## Return result
  return(data)

}
