#' Title
#'
#' @param degree selected degrees of  cristallinity for performing the analysis
#' @param mat matrix of the all the thermograms checked using the functiom mat.check
#'
#' @return "DT" built with the values of mat according to the specified degrees
#' @export
#'
#'
select_degree <- function(mat, degree=seq(0.01, 0.99, by = 0.01)){
  id_cycle<-NULL
DT <- lapply(degree, function(x) mat[, .SD[which.min(abs(ri - x))], by = list(id_cycle)])
for (i in 1:length(degree))
  {
    DT[[i]]$rit <- degree[[i]]
  }
  DT <- rbindlist(DT)
  return(DT)
}

