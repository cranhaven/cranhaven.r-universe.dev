#' Time each index evaluation for projections in the tour path.
#'
#' Index evaluation timing may depend on the data distribution, we
#' evaluate the computing time for a set of different projections to
#' get an overview of the distribution of computing times.
#'
#' @param d Input data in matrix format
#' @param t List of projection matrices (e.g. interpolated tour path)
#' @param idx Index function
#' @param pmax Maximum number of projections to evaluate (cut t if longer than pmax)
#' @return numeric vector containing all distances
#' @export
#' @examples 
#' d <- as.matrix(spiral_data(500, 4))
#' t <- purrr::map(1:10, ~ tourr::basis_random(4))
#' idx <- scag_index("stringy")
#' time_sequence(d, t, idx, 10)

time_sequence <- function(d, t, idx, pmax){
  i <- 1
  dfTimer <- data.frame(t= numeric(), i=numeric())
  for(pMatrix in t){
    if(i>pmax) break
    tictoc::tic.clearlog()
    tictoc::tic() #start timer
    dProj <- d %*% pMatrix
    res <- idx(dProj)
    tictoc::toc(log=TRUE,quiet=TRUE)
    log <- tictoc::tic.log(format = FALSE)
    resT <- log[[1]]$toc - log[[1]]$tic
    dfTimer <- tibble::add_row(dfTimer, t=resT, i=i)
    i <- i+1
  }
  return(dfTimer)
}
