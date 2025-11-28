#------------------------------------------------------------------------------#
#--------------------------------- skm::skm.r ---------------------------------#
#------------------------- author: gyang274@gmail.com -------------------------#
#------------------------------------------------------------------------------#

#--------+---------+---------+---------+---------+---------+---------+---------#
#234567890123456789012345678901234567890123456789012345678901234567890123456789#

#------------------------------------------------------------------------------#
#------------------------------------ main ------------------------------------#
#------------------------------------------------------------------------------#

# skm_mlp_cpp: solve skm with multiple runs in serial via Rcpp RcppArmadillo
# Rcpp::List skm_mlp_cpp(
#   const arma::mat& x, const arma::uword k,
#   const arma::uvec& s_must,
#   const arma::uword max_it, const arma::uword max_at
# )

# skmRpl_mlp_cpp: solve skm with multiple runs in serial via Rcpp RcppParallel
# Rcpp::List skmRpl_mlp_cpp(
#   const NumericMatrix x, const unsigned int k,
#   const IntegerVector s_must,
#   const unsigned int max_it, const unsigned int max_at,
#   const unsigned int skmRpl_GS = 100
# )

# skm_mls_cpp: solve skm with multiple runs in serial via Rcpp RcppArmadillo
# skm_mls_cpp improved selection efficiency by stratified sampling s_init w.r.t g
# Rcpp::List skm_mls_cpp(
#   const arma::mat& x, const arma::uword k, arma::uvec g,
#   const arma::uvec& s_must,
#   const arma::uword max_it, const arma::uword max_at
# )

#' skm_mls
#'
#' @description
#'
#'  a selective k-means problem solver - wrapper over skm_mls_cpp
#'
#' @details
#'
#'  a selective k-means problem is defined as finding a subset of k rows from
#'  a m x n matrix such that the sum of each column minimial is minimized.
#'
#'  skm_mls would take data.table (data.frame) as inputs, rather than a matrix,
#'  assume that a data.table of s - t - d(s, t) for all combination of s and t,
#'  choose k of s that minimizes sum(min(d(s, t) over selected k of s) over t).
#'
#' @param x data.table with s - t - d(s, t): s<source> - t<target> - d<distance>
#'  where s<source> and t<target> must characters and d<distance> must numeric.
#'  aware d<distance> is not necessary as an euclidean or any distance and even
#'  necessary as symmetric - d(s, t) can be unequal to d(t, s) - view d as such
#'  a measure of the cost of assigning one to the other!
#'
#' @param k number of centers
#'
#' @param s_colname s<source>
#'
#' @param t_colname t<target>
#'
#' @param d_colname d<distance> - view d as cost of assigning t into s.
#'  also modify the input data or build in the algorithm can solve problem with
#'  a different fixed cost on using each s as source - i prefer to moddify data
#'  so that the algorithm is clean and clear - i will show a how to in vignette
#'
#' @param w_colname w<weighting> - optional: when not null will optimize toward
#'  objective to minimize d = d * w such as weighted cost of assigning t into s
#'
#' @param s_must length <= k-1 s must in result: conditional optimizing.
#'
#' @param s_ggrp s_init will be stratified sampling from s w.r.t s_ggrp.
#'
#' @param max_it max number of iterations can run for optimizing result.
#'
#' @param max_at max number of attempts/repeats on running for optimial.
#'
#' @param auto_create_ggrp boolean indicator of whether auto creating the group
#'  structure using the first letter of s when s_ggrp is integer(0).
#'
#' @param extra_immaculatism boolean indicator of whether making extra runs for
#'  improving result consistency when multiple successive k is specified, e.g.,
#'  k = c(9L, 10L).
#'
#' @param extra_at an integer specifying the number of extra runs when argument
#'  extra_immaculatism is TRUE.
#'
#' @return data.table
#'
#'  o - objective - based on d_colname
#'
#'  w - weighting - based on w_colname
#'
#'  k - k<k-list> - based on k - input
#'
#'  s - s<source> - based on s_colname
#'
#'  d - weighed averge value of d_colname weighed by w_column when s are selected.
#'
#' @useDynLib skm
#'
#' @importFrom Rcpp sourceCpp
#'
#' @export
skm_mls <- function(
  x, k = 1L, s_colname = "s", t_colname = "t", d_colname = "d",
  w_colname = NULL, s_ggrp = integer(0L), s_must = integer(0L),
  max_it = 100L, max_at = 100L, auto_create_ggrp = TRUE,
  extra_immaculatism = TRUE, extra_at = 10L
) {

  #- check input list

  message("skm_mls: skm solver via skm_mls_cpp ...\n")

  .ptc <- proc.time()

  ## parallel processing? (inherit message from skm_mlp)
  # if ( goParallel ) {
  #
  #   message("skm_mlp: parallel processing is high risk:\n\t1. set skmRpl_GS (gain size) for protection.\n\t2. set setThreadOptions(numThreads = defaultNumThreads() / 2) for more protection.\n\t3. author test parallel on with max_at/skmRpl_GS equals 2 to 10, stable ok, performance gain slight.\n")
  #
  # }

  ## x must be data.table
  if ( ! all( class(x) == c("data.table", "data.frame") ) ) {

    warning("skm_mls: x should be a data.table.\n")

    x <- x %>% `class<-`(c("data.table", "data.frame"))

  }

  ##  k list must be sort and unique
  if ( !all(k == sort(unique(k))) ) {

    warning("skm_mls: k must be sort and all unique.\n")

    k <- sort(unique(k))

  }

  ## s_ggrp

  ## s_ggrp must a data.table contain all s - g mapping
  ## s_ggrp integer(0L) and auto_create_ggrp TRUE would
  ## create g = substr(s_name, 1, 1) as default strata.

  ## s_must
  if ( length(s_must) > length(unique(s_must)) ) {

    warning("skm_mls: s_must must be unique.\n")

    s_must <- unique(s_must)

  }

  #- create analytical dataset xdat
  eval(parse(text = paste0(
    'xdt <- x[ , .(',
      's = ', s_colname, ', ',
      't = ', t_colname, ', ',
      'd = ', 'as.numeric(', d_colname, ')', ', ',
      'w = ', ifelse(is.null(w_colname), 1.00, w_colname),
    ') ]'
  )))

  ## sum of weights from each s <souce> should be equal to 1
  # xdt <- xdt %>%
  #   dplyr::group_by(s) %>% dplyr::mutate(d = d * w / sum(w)) %>% dplyr::ungroup() %>%
  #   dplyr::select(-w) %>% `class<-`(c("data.table", "data.frame"))
  xdt <- xdt[ , c("d") := get("d") * get("w") / sum(get("w")), by = c("s")][ , c("w") := NULL ]

  ## xdt must have full combination of s and t
  ## TODO: impute maximum double into cell when not full combination
  ## sent warning message and impute cell rather than stops the call
  stopifnot( nrow(xdt) == length(unique(xdt[["s"]])) * length(unique(xdt[["t"]])) )

  ## dcast.data.table xdt long into wide
  xdt <- xdt %>% data.table::setorder(s, t) %>% data.table::dcast(s ~ t, value.var = "d")

  #- create input matrix xmt and name vector s_name and t_name from xdt

  s_name <- xdt[["s"]]

  ## set avoids the overhead of [.data.table in loops and is super fast
  # xdt[ , `:=`(s = NULL)]
  data.table::set(xdt, i = NULL, j = 1L, value = NULL)

  t_name <- names(xdt)

  ## check or construct s_ggrp for stratified sampling
  if ( length(s_ggrp) == 0 && auto_create_ggrp ) {

    message("skm_mls: auto_create g w.r.t first letter of s_name.\n")

    g <- as.numeric(as.factor(substr(s_name, 1, 1)))

  } else {

    if ( c("s", "g") %in% names(s_ggrp) ) {

      g <- as.numeric(as.factor(plyr::mapvalues(s_name, from = s_ggrp[["s"]], to = s_ggrp[["g"]])))

    } else {

      message("skm_mls: auto_create is off and no s_ggrp provided, g is created as all one - no stratified sampling.\n")

      g <- rep(1, length(s_name))

    }

  }

  ## create s_must list - caution - cpp index started from 0
  s_must_idx_cpp <- integer(0L)

  if( length(s_must) > 0 ) {

    s_must_idx_cpp <- match(s_must, s_name) - 1

  }

  #- apply skm_mls_cpp

  xmt <- as.matrix(xdt)

  xs <- NULL # fix R CMD check --as-cran NOTE: no visible binding for global variable 'xs'

  eval(parse(text = paste0(
    'xs <- data.table(',
      'o = "', d_colname, '", ',
      'w = "', w_colname, '", ',
      'k = k, ',
      's = list(list(character(0L))), ',
      'd = numeric(length(k))',
    ')'
  )))

  for ( ik in 1L:length(k) ) {

    message("skm_mls: optimizing on k <", k[ik], "> ...\n")

    skm_lst <- skm_mls_cpp(
      x = xmt, k = k[ik], g = g, s_must = s_must_idx_cpp,
      max_it = max_it, max_at = max_at
    )

    #- collect result into data.table

    ## s - optim s w.r.t. min(sum(min(d(s, t) * w(s, t))))
    ## s - cpp return s indices which indexed from 0 not 1
    data.table::set(xs, ik, 4L, list(list(s_name[c(skm_lst$s + 1L)])))

    ## d - average d weighted by w over all t
    data.table::set(xs, ik, 5L, skm_lst$o)

    #- set extra run based on previous optim s<source>
    if ( ik > 1L && extra_immaculatism ) {

      message("skm_mls: set extra runs based on previous optim s ...\n")

      ## s_init_idx_cpp_0 - caution - cpp index started from 0.
      s_init_idx_cpp_0 <- match(c( unlist(xs[ik - 1L, s, drop = TRUE]) ), s_name) - 1L

      for ( jk in c(1L:extra_at) ) {

        s_init_idx_cpp_1 <- sample(
          x = setdiff(seq(length(s_name)) - 1L, s_init_idx_cpp_0),
          size = k[ik] - length(s_init_idx_cpp_0), replace = FALSE
        )

        a_skmSolution <- skm_sgl_cpp(
          x = xmt, s_init = c(s_init_idx_cpp_0, s_init_idx_cpp_1),
          s_must = s_must_idx_cpp, max_it = max_it
        )

        if ( a_skmSolution[["o"]] < xs[ik, get("d"), drop = TRUE] ) {

          message("skm_mls: encounter better solution at jk <", jk, "> run based on previous optim s ...\n")

          data.table::set(xs, ik, 4L, list(list(s_name[c(a_skmSolution[["s"]] + 1L)])))

          ## d - average d weighted by w over all t
          data.table::set(xs, ik, 5L, a_skmSolution[["o"]])

        }

      }

      message("skm_mls: set extra runs based on previous optim s ... done.\n")

    }

    message("skm_mls: optimizing on k <", k[ik], "> ... done.\n")

  }

  .ptd <- proc.time() - .ptc

  message("skm_mls: cosumes ", .ptd[3], " seconds.\n")

  message("skm_mls: skm solver via skm_mls_cpp ... done.\n")

  return(xs)

}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------------ math ------------------------------------#
#------------------------------------------------------------------------------#
#' dist_wlatlng
#' @description
#'  calculate distance btwn coordinate1<lat1, lng1> and coordinate2<lat2, lng2>
#' @details
#'  calculate the great circle distance between 2 points with Haversine formula,
#'  which deliberately ignores elevation differences.
#'
#'  Haversine formula (from R.W. Sinnott, "Virtues of the Haversine",
#'  Sky and Telescope, vol. 68, no. 2, 1984, p. 159):
#'
#'  dlon = lon2 - lon1
#'
#'  dlat = lat2 - lat1
#'
#'  a = sin^2(dlat/2) + cos(lat1) * cos(lat2) * sin^2(dlon/2)
#'
#'  c = 2 * arcsin(min(1,sqrt(a)))
#'
#'  d = R * c
#'
#' @param .lat1 latitude of coordinate1
#' @param .lng1 longitude of coordinate1
#' @param .lat2 latitude of coordinate2
#' @param .lng2 longitude of coordinate2
#' @param .measure - mi or km
#' @export
dist_wlatlng <- function(.lat1, .lng1, .lat2, .lng2, .measure = "mi") {

  # earth radium in 3956 mi - 6367 km
  .r <- c(3956, 6367)[match(.measure, c("mi", "km"))]

  .dlat <- .lat2 - .lat1

  .dlng <- .lng2 - .lng1

  # most computers require the arguments of trignometric functions to be expressed in radians. To convert lon1,lat1 and lon2,lat2 from degrees, minutes, and seconds to radians, first convert them to decimal degrees. To convert decimal degrees to radians, multiply the number of degrees by pi/180 = 0.017453293 radians/degree. <http://www.movable-type.co.uk/scripts/gis-faq-5.1.html>

  .a <- sin(.dlat/2 * pi/180)^2 + cos(.lat1 * pi/180) * cos(.lat2 * pi/180) * sin(.dlng/2 * pi/180)^2

  .c <- 2 * asin(pmin(1, sqrt(.a)))

  .d <- .r * .c

  return(.d)
}
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------------ init ------------------------------------#
#------------------------------------------------------------------------------#
# `%+%` - concatenate strings
# `%+%` <- function(stringX, stringY) { return( paste0(stringX, stringY) ) }
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------------ .cpp ------------------------------------#
#------------------------------------------------------------------------------#
# .onUnload: clean up when package is unloaded as C++ code use in package.
.onUnload <- function (libpath) {
  library.dynam.unload("skm", libpath)
}
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------------ load ------------------------------------#
#------------------------------------------------------------------------------#
# Rcpp export class and module
loadModule("skm_module", TRUE)
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------- sets r-options -------------------------------#
#------------------------------------------------------------------------------#
# protect from using all thread when run skmRpl parallel
# setThreadOptions(numThreads = defaultNumThreads() / 2)
#------------------------------------------------------------------------------#
