#' Test rotation invariance of index functions for selected 2-d data set.
#'
#' Ideally a projection pursuit index should be roation invariant, we
#' test this explicitly by profiling the index while rotating a
#' distribution.
#'
#' @param d data (2 column matrix containing distribution to be rotated)
#' @param index_list list of index functions to calculate for each entry
#' @param index_labels labels used in the output
#' @param n number of steps in the rotation (default = 200)
#' @return index values for each rotation step
#' @export
#' @examples 
#' d <- as.matrix(sin_data(30, 2))
#' index_list <- list(tourr::holes(), scag_index("stringy"), mine_indexE("MIC"))
#' index_labels <- c("holes", "stringy", "mic")
#' pRot <- profile_rotation(d, index_list, index_labels, n = 50)
#' plot_rotation(pRot)
profile_rotation <- function(d, index_list, index_labels, n=200){
  # initialise results storage
  res_mat <- matrix(ncol = length(index_labels)+1, nrow = n+1)
  colnames(res_mat) <- c(index_labels, "alpha")

  # loop over rotation angles
  i <- 1
  for (a in seq(0,2*pi, 2*pi/n)){
    rotM <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), ncol = 2)
    dprj <- d %*% rotM
    res <- c()
    for (idx in index_list){
      res <- c(res, idx(dprj))
    }
    res_mat[i,] <- c(res, a)
    i <- i+1
  }
  res_mat
}

#' Plot rotation traces of indexes obtained with profileRotation.
#'
#' @param res_mat data (result of profileRotation)
#' @return ggplot visualisation of the tracing data
#' @export
plot_rotation <- function(res_mat){
  PPI <- colnames(res_mat)
  PPI <- PPI[PPI != "alpha"] # columns are index names or angle alpha
  res_melt <- tibble::as_tibble(res_mat) |>
    dplyr::mutate(angle=alpha*360/(2*pi)) |>
    dplyr::select(-alpha) |>
    tidyr::gather(PPI, value, -angle) |>
    ggplot2::ggplot(ggplot2::aes(x=angle, y=value)) +
    ggplot2::facet_wrap(~PPI) +
    ggplot2::coord_polar() +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::geom_line() +
    ggplot2::ylim(c(-1,1.1))
  res_melt
}
