#' Estimate a Congeneric Normal-Ogive Model
#'
#' `conogive` is used to estimate congeneric normal-ogive models
#'   (McDonald, R. P. (1997)).
#'
#' The `data` argument can be either a list containing the parameters of
#'   a normal-ogive model, or raw data. If actual data is passed to `data`,
#'   it is passed to `psych::polychoric` to estimate its polychoric correlation
#'   matrix and cutoffs. This is passed to `psych::fa` to do a barebones
#'   multivariate normal-ogive model. The `...` arguments are passed to
#'   `psych::fa`, which is called with `fm = "ml"` by default.
#'
#' Likert data should start with 1, not 0.
#'
#' @param data A data frame of observations or a named list with elements
#'    `lambda`, `sigma`, and `cuts`. See the details.
#' @param use Passed to `stats::cov`; defaults to `"complete.obs"`.
#' @param ... Passed to `psych::fa`, where `fm = "ml"` by default.
#' @export
#' @return An object of class `conogive`.
#' @examples
#' if(require("psychTools")) {
#'   extraversion = psychTools::bfi[c("E1", "E2", "E3", "E4", "E5")]
#'   extraversion[, "E1"] = 7 - extraversion[, "E1"] # Reverse-coded item.
#'   extraversion[, "E2"] = 7 - extraversion[, "E2"] # Reverse-coded item.
#'   fit = conogive(extraversion)
#' }
#' @references
#' McDonald, R. P. (1997). Normal-ogive multidimensional model. In W. J.
#' van der Linden & R. K. Hambleton (Eds.), Handbook of Modern Item Response
#' Theory (pp. 257–269). Springer.
#' \url{https://doi.org/10.1007/978-1-4757-2691-6_15}
#' Moss, J. (2020). Please avoid the standardized alpha and the ordinal alpha.
#' \url{https://psyarxiv.com/nvg5d}
conogive = function(data, use = "complete.obs", ...) {

  if(is.list(data)) {

    if(all(c("lambda", "sigma", "cuts") %in% names(data))) {
      checkmate::assertNumeric(data$lambda)
      k = length(data$lambda)
      checkmate::assertNumeric(data$sigma, len = k)
      cuts = massage_cuts(data$cuts, k)
      checkmate::assertList(cuts, len = k)

      lambda = standardize_lambda(data$lambda, data$sigma)
      sigma = standardize_sigma(data$lambda, data$sigma)
      rho = tcrossprod(lambda, lambda) + diag(sigma^2)

      object = list(rho = rho,
                    cuts = cuts,
                    lambda = lambda,
                    sigma = sigma,
                    xi_sample = xi_theoretical(cuts, rho),
                    n = Inf)

      class(object) = "conogive"
      return(object)
    }

  }

  args = list(...)
  if(is.null(args$fm)) args$fm = "ml"
  assertthat::assert_that(min(data, na.rm = TRUE) >= 1)
  poly = psych::polychoric(data)
  fa = do.call(what = psych::fa, args = c(list(r = poly$rho), args))
  lambda = stats::setNames(c(fa$loadings), colnames(data))
  sigma = c(sqrt(fa$uniquenesses))
  xi = xi_sample(y = data, cuts = poly$tau, use = use)

  object = list(rho = poly$rho, cuts = poly$tau, lambda = lambda, sigma = sigma,
                xi_sample = xi, n = nrow(data))

  class(object) = "conogive"
  object

}

#' Predict Method for Conogive Objects
#'
#' Predict the latent variable in a congeneric normal-ogive model using the
#' formula of ((arxiv ref.))
#'
#' @param object An object of class "`conogive`".
#' @param newdata An optional data frame with variables to predict with.
#'    The fitted values are used if omitted.
#' @param weights Weights to use; choose between optimal weights and equal
#'    weights.
#' @param ... Ignored.
#' @examples
#'  if(require("psychTools")) {
#'    extraversion = psychTools::bfi[c("E1", "E2", "E3", "E4", "E5")]
#'    extraversion[, "E1"] = 7 - extraversion[, "E1"] # Reverse-coded item.
#'    extraversion[, "E2"] = 7 - extraversion[, "E2"] # Reverse-coded item.
#'    object = conogive(extraversion)
#'    hist(predict(object, extraversion)) # Plot distribution of predictions.
#'  }
#' @export
predict.conogive = function(object, newdata, weights = c("optimal", "equal"),
                                                         ...) {

  weights = match.arg(weights)
  lambda = object$lambda
  sigma = object$sigma

  if(is.null(dim(newdata))) dim(newdata) = c(1, length(newdata))

  names = rownames(newdata)

  k = ncol(newdata)
  cuts = massage_cuts(object$cuts)
  mat = sapply(seq.int(k), function(i) x_hat(newdata[, i], cuts[[i]]))

  v = if (weights == "optimal") thurstone(lambda, sigma) else
    mean(lambda) / (k * mean(lambda)^2 + mean(sigma^2)) * rep(1, k)

  stats::setNames(c(mat %*% v), rownames(names))

}

#' Calculate the Ordinal Reliabiltiy
#'
#' The function `ordinal_r ` calculates the concrete ordinal reliability.
#'    The functions `theoretical_ordinal_r` and `theoretical_ordinal_alpha`
#'    calculates the theoretical ordinal reliability and alpha based on the
#'    polychoric correlation matrix.
#'
#' The population value of theoretical ordinal alpha equals the theoretical
#'    ordinal reliability when the underlying multivariate normal is parallel.
#'    The concrete ordinal reliability is the sqaured correlation between the
#'    true latent variable and the best linear predictor of the observed
#'    Likert-type data. See ((ref)) for definitions.
#'
#' @param object An object of class `conogive`.
#' @param xi How to calculate the Xi matrix. Option `"theoretical"` calculates
#'    the theoretical Xi matrix from `rho`, while  `"sample"` calculates the
#'    sample Xi matrix.
#' @param weights The weights used to calculate the ordinal reliability.
#'    Option `"optimal"` uses the optimal weights and `"equal"` the equal
#'    weights.
#' @export
#' @name reliability
#' @return The concrete ordinal reliability, theoretical ordinal reliability, or
#'    theoretical ordinal alpha.
#' @examples
#' if(require("psychTools")) {
#'   agreeableness = psychTools::bfi[c("A1", "A2", "A3", "A4", "A5")]
#'   agreeableness[, "A1"] = 7 - agreeableness[, "A1"] # Reverse-coded item.
#'   object = conogive(agreeableness)
#'   ordinal_r(object, weights = "equal") # 0.6394087
#'   theoretical_ordinal_alpha(object) # 0.7589922
#'   theoretical_ordinal_r(object, weights = "equal") # 0.7689878
#'   ordinal_r(object, weights = "optimal") # 0.6763742
#'   theoretical_ordinal_r(object) # 0.8101108
#' }

ordinal_r = function(object, xi = c("sample", "theoretical"),
                        weights = c("optimal", "equal")) {

  weights = match.arg(weights)
  xi = match.arg(xi)

  cuts = object$cuts
  rho = object$rho
  v = thurstone(object$lambda, object$sigma)
  xi = if(xi  == "sample") object$xi else xi_theoretical(cuts, rho)

  if(weights == "optimal") {
    c(crossprod(v, xi %*% v))
  } else {
    i = rep(1, nrow(rho))
    c(crossprod(i, xi %*% v))^2/sum(xi)
  }

}


#' @rdname reliability
#' @export
theoretical_ordinal_r = function(object,
                                 weights = c("optimal", "equal", "sigma")) {

  lambda = object$lambda
  sigma = object$sigma
  weights = match.arg(weights)
  k <- length(lambda)

  if(weights == "optimal") {
    a <- sum(lambda^2 / sigma^2)
  } else if(weights == "equal") {
    a <- k * mean(abs(lambda))^2 / mean(sigma^2)
  } else if(weights == "sigma") {
    a <- k * mean(abs(lambda) / sqrt(lambda^2 + sigma^2))^2 /
      mean(sigma^2 / (lambda^2 + sigma^2))
  }

  a / (a + 1)

}

#' @rdname reliability
#' @export
theoretical_ordinal_alpha  = function(object) {
  rho = object$rho
  k <- nrow(rho)
  k / (k - 1) * (1 - tr(rho) / sum(rho))
}
