#' Tracing the index over an interpolated planned tour path.
#'
#' Tracing is used to test if the index value varies smoothly
#' over an interpolated tour path. The index value is calculated
#' for the data d in each projection in the interpolated sequence.
#' Note that all index functions must take the data in 2-d matrix
#' format and return the index value.
#'
#' @param d data
#' @param m list of projection matrices for the planned tour
#' @param index_list list of index functions to calculate for each entry
#' @param index_labels labels used in the output
#' @return index values for each interpolation step
#' @export
#' @examples
#' d <- spiral_data(100, 4)
#' m <- list(basis_matrix(1,2,4), basis_matrix(3,4,4))
#' index_list <- list(tourr::holes(), tourr::cmass())
#' index_labels <- c("holes", "cmass")
#' trace <- get_trace(d, m, index_list, index_labels)
#' plot_trace(trace)
get_trace <- function(d, m, index_list, index_labels){
  mX <- m[[1]]
  if(ncol(mX) != 2){
    warning("Each projection matrix must have exactly two columns!")
    return(NULL)
  }
  # problem with planned tour: skipping first two entries
  # so we generate proxies (not used)
  mSkip1 <- tourr::basis_random(nrow(mX))
  mSkip2 <- tourr::basis_random(nrow(mX))
  mList <- append(list(mSkip1, mSkip2), m)
  # get tour path object
  tPath <- tourr::save_history(d, tour_path=tourr::planned_tour(mList))
  # get interpolated path and unformat to list
  tFullPath <- as.list(tourr::interpolate(tPath))

  # initialise results storage
  res_mat <- matrix(ncol = length(index_labels)+1, nrow = length(tFullPath))
  colnames(res_mat) <- c(index_labels, "t")

  # loop over path and index functions
  for (i in seq_along(tFullPath)){
    dprj <- as.matrix(d) %*% tFullPath[[i]]
    res <- c()
    for (idx in index_list){
      res <- c(res, idx(dprj))
    }
    res_mat[i,] <- c(res, i)
  }
  res_mat
}

#' Plot traces of indexes obtained with \code{\link{get_trace}}.
#'
#' @param res_mat data (result of get_trace)
#' @param rescY bool to fix y axis range to \[0,1\]
#' @return ggplot visualisation of the tracing data
#' @export
plot_trace <- function(res_mat, rescY=TRUE){
  PPI <- colnames(res_mat)
  PPI <- PPI[PPI != "t"] # columns are index names or time counter
  res_melt <- tibble::as_tibble(res_mat) |>
    tidyr::gather(PPI, value, -t) |>
    ggplot2::ggplot(ggplot2::aes(x=t, y=value)) +
    ggplot2::geom_line() +
    ggplot2::theme(legend.position="none") +
    ggplot2::xlab("Sequence of projections (t)") +
    ggplot2::ylab("PPI value")
  if (rescY) {
    res_melt <- res_melt +
      ggplot2::facet_grid(PPI~.) +
      ggplot2::ylim(c(0,1)) # usually we want index values between 0 and 1
  }
  else {
    res_melt <- res_melt +
      ggplot2::facet_grid(PPI~., scales = "free_y")
  }
  res_melt
}
