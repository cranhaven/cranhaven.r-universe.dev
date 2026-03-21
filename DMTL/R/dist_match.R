#' Estimate Inverse Mapping
#'
#' This function estimates an inverse map \eqn{g} for a given set of knots
#' (input) and values (output) corresponding to a certain map \eqn{f} _i.e._,
#' given \eqn{x, y | f: x --> y}, `match_func()` estimates \eqn{g: y --> x}
#' using linear interpolation.
#'
#' @param knots Vector containing knots for the distribution estimate.
#' @param vals Vector containing distribution values corresponding to the knots.
#' @param new_vals Vector containing distribution values for which the knots
#' are unknown. If missing, `match_func()` simply returns the map function.
#' @param lims Vector providing the range of the knot values for mapping. If
#' missing, these values are estimated from the given knots.
#' @param get_func Flag for returning the map function if `new_vals` is
#' provided. If `TRUE`, `match_func()` returns a named list with two components-
#' `mapped` and `func` (mapped knots for `new_vals` and the mapping function,
#' respectively). Defaults to `FALSE`.
#'
#' @return
#' If `new_vals` is missing, a function performing interpolation
#' (linear or constant) of the given data points.
#'
#' If `get_func = FALSE`, a vector containing the matched knots that will
#' produce `new_vals` for the map \eqn{f}.
#'
#' If `get_func = TRUE`, a named list with two components- `mapped` and `func`
#' (mapped knots for `new_vals` and the mapping function, respectively).
#'
#'
#' @keywords inverse-map function-approximation
#' @export
#' @examples
#' set.seed(654321)
#' x <- rnorm(100, 1, 0.5)
#' F <- ecdf(x)
#' fval <- F(x)
#' map <- match_func(knots = x, vals = fval)
#'
#' x2 <- rnorm(20, 0.8, 0.5)
#' F2 <- ecdf(x2)
#' fval2 <- F2(x2)
#' matched <- match_func(knots = x, vals = fval, new_vals = fval2)
#'
#' ## Plot histograms...
#' opar <- par(mfrow = c(1, 3))
#' hist(x);    hist(x2);    hist(matched)
#' par(opar)              # Reset par
#'

## Dependency: stats, ks
## Dependency_own: lambda_functions
##
## Author: SR Dhruba, Dec 2020
################################################################################


## Define matching function...
match_func <- function(knots, vals, new_vals, lims, get_func = FALSE) {

  ## Limits for function inputs...
  if (missing(lims)) {  lims <- range(knots)  }

  ## Inverse CDF mapping...
  map <- stats::approxfun(x = vals, y = knots, yleft = lims[1], yright = lims[2], method = "linear", ties = "ordered", rule = 2)

  if (missing(new_vals))        # Return map function
    return( map )

  ## Matched values...
  new_knots <- confined(map(new_vals), lims = lims)

  if (get_func)                 # Return function & mapped values
    return( list("mapped" = new_knots, "func" = map) )

  new_knots                     # Return mapped values only

}


#' Distribution Matching for Source and Reference Datasets
#'
#' This function matches a source distribution to a given reference distribution
#' such that the data in the source space can effectively be transferred to the
#' reference space _i.e._ domain transfer _via_ distribution matching.
#'
#' @param src Vector containing the source data to be matched.
#' @param ref Vector containing the reference data to estimate the reference
#' distribution for matching.
#' @param src_cdf Vector containing source distribution values. If missing,
#' these values are estimated from the source data using `estimate_cdf()`.
#' @param ref_cdf Vector containing reference distribution values. If missing,
#' these values are estimated from the reference data using `estimate_cdf()`.
#' @param lims Vector providing the range of the knot values for mapping. If
#' missing, these values are estimated from the reference data.
#' @param density Flag for using kernel density estimates for matching instead
#' of histogram counts. Defaults to `False`.
#' @param samples Sample size for estimating distributions if `src_cdf` and/or
#' `ref_cdf` are missing. Defaults to `1e6`.
#' @param seed Seed for random number generator (for reproducible outcomes).
#' Defaults to `NULL`.
#'
#' @return A vector containing the matched values corresponding to `src`.
#'
#' @keywords distribution-matching domain-transfer
#' @export
#' @examples
#' set.seed(7531)
#' x1 <- rnorm(100, 0.2, 0.6)
#' x2 <- runif(200)
#' matched <- dist_match(src = x1, ref = x2, lims = c(0, 1))
#'
#' ## Plot histograms...
#' opar <- par(mfrow = c(1, 3))
#' hist(x1);    hist(x2);    hist(matched)
#' par(opar)              # Reset par
#'

## Dependency: stats, ks
## Dependency_own: lambda_functions, match_func
##
## Author: SR Dhruba, Dec 2020
################################################################################


## Distribution matching for two CDFs...
dist_match <- function(src, ref, src_cdf, ref_cdf, lims, density = FALSE, samples = 1e6, seed = NULL) {

  ## Get distributions...
  if (missing(ref_cdf))
    ref_cdf <- estimate_cdf(ref, bootstrap = TRUE, samples, density, binned = TRUE, grids = 1e3, unit_range = TRUE, seed)

  if (missing(src_cdf))
    src_cdf <- estimate_cdf(src, bootstrap = TRUE, samples, density, binned = TRUE, grids = 1e3, unit_range = TRUE, seed)


  ## Mapping parameters...
  if (density) {                        # Using kernel density
    kn_vals <- ref_cdf$eval.points;         fn_vals <- ref_cdf$estimate
    vals_to_match <- stats::predict(src_cdf, x = src)
  }

  else {                                # Using histogram
    kn_vals <- stats::knots(ref_cdf);       fn_vals <- ref_cdf(kn_vals)
    vals_to_match <- src_cdf(src)
  }


  ## Perform mapping...
  if (missing(lims)) {  lims <- range(ref)  }
  matched <- match_func(knots = kn_vals, vals = fn_vals, new_vals = vals_to_match, lims = lims)
  matched

}
