#' Re-evaluate index after jittering the projection by an angle alpha.
#'
#' @param proj Original projection plane
#' @param d Data matrix
#' @param alpha Jitter angle
#' @param idx Index function
#' @return New index value
#' @export
jitter_angle <-function(proj, d, alpha, idx){
  newProj <- basis_nearby(proj, alpha = alpha, method = "geodesic")
  newD <- d %*% newProj
  return(idx(newD))
}

#' Re-evaluate index after jittering all points by an amount alpha.
#'
#' @param proj_data Original projected data points
#' @param alpha Jitter amount (passed into the jitter() function)
#' @param idx Index function
#' @return New index value
#' @export
jitter_points <-function(proj_data, alpha, idx){
  newD <- jitter(proj_data, amount=alpha)
  return(idx(newD))
}

#' Evaluate mean index value over n jittered views.
#'
#' @param proj Original projection plane
#' @param d Data matrix
#' @param alpha Jitter amount (for jittering angle or points)
#' @param idx Index function
#' @param method Select between "jitterAngle" (default) and "jitterPoints"
#'     (otherwise we return original index value)
#' @param n Number of evaluations entering mean value calculation
#' @return Mean index value
#' @export
get_index_mean <- function(proj, d, alpha, idx, method="jitter_angle", n=10){
  dProj <- d %*% proj
  orig <- idx(dProj)
  if(method == "jitter_angle"){
    valVec <- replicate(n, jitter_angle(proj, d, alpha, idx))
  } else if (method=="jitter_points"){
    valVec <- replicate(n, jitter_points(dProj, alpha, idx))
  }
  else { return(orig)}
  return(mean(c(orig, valVec)))
}

#' Compare traces with different smoothing options.
#'
#' @param d Data matrix
#' @param tPath Interpolated tour path (as list of projections)
#' @param idx Index function
#' @param alphaV Jitter amounts to compare (for jittering angle or points)
#' @param n Number of evaluations entering mean value calculation
#' @return Table of mean index values
#' @export
#' @examples
#' d <- as.matrix(spiral_data(30, 3))
#' tPath <- tourr::save_history(d, max_bases=2)
#' tPath <- as.list(tourr::interpolate(tPath, 0.3))
#' idx <- scag_index("stringy")
#' compS <- compare_smoothing(d, tPath, idx, alphaV = c(0.01, 0.05), n=2)
#' plot_smoothing_comparison(compS)
compare_smoothing <- function(d, tPath, idx, alphaV=c(0.01, 0.05, 0.1), n=10){
  sc <- tibble::tibble(
    index_mean= numeric(),
    t=numeric(),
    method=character(),
    alpha=numeric())
  for (method in c("jitter_angle", "jitter_points", "no_smoothing")){
    for (alpha in alphaV){
      for (i in seq_along(tPath)) {
        idM <- get_index_mean(tPath[[i]], d, alpha, idx, method, n)
        sc <- sc |>
          tibble::add_row(index_mean=idM, t=i, method=method, alpha=alpha)
      }
    }
  }
  return(sc)
}

#' Plot the comparison of smoothing methods.
#'
#' Plotting method for the results of compareSmoothing.
#' The results are mapped by facetting over values of alpha and
#' mapping the method (jitter_angle, jitter_points, no_smoothing) to
#' linestyle and color (black dashed, black dotted, red solid). By
#' default legend drawing is turned off, but can be turned on via
#' the lPos argument, e.g. setting to "bottom" for legend below the plot.
#'
#' @param sm_mat Result from compare_smoothing
#' @param lPos Legend position passed to ggplot2 (default is none for no legend shown)
#' @return ggplot visualisation of the comparison
#' @export
plot_smoothing_comparison <- function(sm_mat, lPos="none"){
  sm_mat <- sm_mat |>
    dplyr::mutate(method = factor(
      method, levels = c("jitter_angle", "jitter_points", "no_smoothing")
      )) |>
    ggplot2::ggplot(
      ggplot2::aes(x=t, y=index_mean, color=method, linetype=method)
      ) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values = c("black", "black", "red")) +
    ggplot2::scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
    ggplot2::theme(legend.position = lPos)+
    ggplot2::ylim(0,1) +
    ggplot2::facet_grid(alpha~.) +
    ggplot2::xlab("Sequence of projections (t)") +
    ggplot2::ylab("PPI value")
  sm_mat
}

