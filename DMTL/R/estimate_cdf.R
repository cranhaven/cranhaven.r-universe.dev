#' Estimate Cumulative Distribution
#'
#' This function estimates the values of the cumulative distribution function
#' (CDF) for a vector.
#' @param x Vector containing data.
#' @param bootstrap Flag for performing bootstrapping on `x` to get a better
#' estimate of the CDF. Defaults to `TRUE`.
#' @param samples Sample size for bootstrapping. Defaults to `1e6`. Ignored
#' when `bootstrap = FALSE`.
#' @param density Flag for calculating kernel density estimates (KDE) instead
#' of histogram counts. Depends on the `ks` package for density estimation.
#' Defaults to `FALSE`.
#' @param binned Flag for calculating binned KDE. Defaults to `TRUE`. Ignored
#' when `density = FALSE`.
#' @param grids Size parameter for the estimation grid when `density = TRUE`.
#' Used to calculate the grid sizes for KDE bandwidth estimation (`grids*10`),
#' and grid size KDE estimation (`bgridsize = grids` if `binned = TRUE` else
#' `gridsize = grids/10`). Defaults to `1e4`.
#' @param unit_range Flag for unity data range (_i.e._, data is normalized
#' between 0 and 1). Defaults to `FALSE`.
#' @param seed Seed for random number generator (for reproducible outcomes).
#' Defaults to `NULL`.
#' @param ... Other options relevant for distribution estimation.
#'
#' @return
#' If `density = FALSE`, a function of class `ecdf`, inheriting from the
#' `stepfun` class, and hence inheriting a `knots()` method.
#'
#' If `density = TRUE`, an object of class `kcde` which has the fields
#' `eval.points` and `estimate` necessary for calculating a map.
#'
#'
#' @keywords cumulative-distribution CDF cumulative-histogram kernel-density
#' kernel-density-estimate KDE kernel-CDF
#' @export
#' @examples
#' x <- runif(100)
#' x_hist_cdf <- estimate_cdf(x, samples = 1000, unit_range = TRUE)
#' x_kde_cdf <- estimate_cdf(x, density = TRUE, unit_range = TRUE)
#'

## Dependency: stats, ks
##
## Author: SR Dhruba, Dec 2020
################################################################################

estimate_cdf <- function(x, bootstrap = TRUE, samples = 1e6, density = FALSE, binned = TRUE, grids = 1e4,
                         unit_range = FALSE, seed = NULL, ...) {


    ## Bootstrapping & data range...
    set.seed(seed)                              # For reproducibility

    xx   <- if (bootstrap) sample(x, size = samples, replace = TRUE) else x
    lims <- if (unit_range) c(0, 1) else range(x)


    ## Calculate cumulative distribution...
    if (density) {                              # Use kernel density

        ## Estimate KDE bandwidth...
        bw <- ks::hscv(x, nstage = 2, binned = TRUE, bgridsize = grids * 10)

        x_cdf <- if (binned) {
            ## Binned estimate for faster computation...
            ks::kcde(xx, h = bw, binned = TRUE, bgridsize = grids, xmin = lims[1], xmax = lims[2], ...)
        } else {
            ## Continuous estimate for increased accuracy...
            ks::kcde(xx, h = bw, binned = FALSE, gridsize = grids / 10, xmin = lims[1], xmax = lims[2], ...)
        }
    }

    else {                                      # Use histogram
        x_cdf <- stats::ecdf(xx, ...)
    }

    x_cdf

}
