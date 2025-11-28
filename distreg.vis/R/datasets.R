#' Information about supported and not yet supported distribution families
#'
#' A dataset containing all of bamlss' exported and gamlss.dist families. This
#' is the backbone of the package; whether you can use a distributional family
#' or not depends on this dataset. Since 1.7.0 family \code{betareg} from the
#' \link{betareg} package is also supported.
#'
#' @details This \code{data.frame} object contains one row for each
#' distribution, and columns with the following content: \itemize{ \item
#' \code{dist_name}: Name of the distribution. \item \code{class}: Either
#' "bamlss" or "gamlss" detailing from which package the target distribution
#' comes from. \item \code{implemented}: Is this distribution generally usable
#' for \code{plot_dist()}, and was this usage already tested? \item
#' \code{moment_funs}: Are functions implemented with which to calculate the
#' moments of the distribution, given the parameters? This column is especially
#' relevant for \code{plot_moments()}, in which the predicted moments are
#' displayed. \item \code{type_limits}: Details the range the values from the
#' distribution can have. Can be "both_limits", "one_limit", "no_limit" and
#' "cat_limit" (for categorical distributions). \item \code{l_limit, u_limit}:
#' Integers detailing where the limits of the distributions lie. \item
#' \code{type}: Character string for the type of distribution. Can be
#' "Discrete", "Continuous", "Mixed" and "Categorical". }
#'
#' @examples
#' ## Find out which GAMLSS or BAMLSS families are supported
#'
#' dists_char <- dists[dists$moment_funs, c("dist_name", "class")]
#'
#' # GAMLSS families
#' dists_char[dists_char$class == "gamlss", "dist_name"]
#'
#' # BAMLSS families
#' dists_char[dists_char$class == "bamlss", "dist_name"]
"dists"
