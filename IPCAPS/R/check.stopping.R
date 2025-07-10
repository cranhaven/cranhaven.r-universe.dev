
#' (Internal function) Check whether the IPCAPS process meets the stopping
#' criterion.
#'
#' @param eigen.value A vector of Eigenvalues return from \code{svd} ($d),
#' \code{rARPACK::svds} ($d),  \code{eigen} ($values) or \code{rARPACK::eigs} ($values).
#' @param threshold A threshold or a cutoff to stop the IPCAPS process. Also see
#' \code{\link{ipcaps}} (the parameter \code{threshold}).
#'
#' @return A list containing \code{status}, \code{eigen.value}, \code{eigen.fit},
#' \code{threshold}, and \code{no.significant.PC} as explained below:
#' \itemize{
#' \item \code{$status} is either \code{0} representing that the criterion is
#' not met, or \code{1} representing that the criterion is met.
#' \item \code{$eigen.value} is a vector of Eigenvalues as the input parameter.
#' \item \code{$eigen.fit} is a vector of EigenFit values.
#' \item \code{$threshold} is a threashold as the input parameter.
#' \item \code{$no.significant.PC} is an estimated number of sinificant
#' principal components (PC).
#' }
#'
#' @include parallelization.R

check.stopping <- function(eigen.value, threshold){

  eigen.fit.vec = cal.eigen.fit(eigen.value)
  eigen.fit = max(eigen.fit.vec)
  no.significant.PC = length(eigen.fit.vec[1:which(eigen.fit.vec == eigen.fit)[1]])
  if (no.significant.PC<3){
    no.significant.PC = 3
  }

  ret = list("status"=0,"eigen.value"=eigen.value,"eigen.fit"=eigen.fit, "threshold"=threshold, "no.significant.PC" = no.significant.PC)
  if (eigen.fit < threshold){  #case of status = 1, no more spliting, stopping criteria are met
    ret = list("status"=1,"eigen.value"=eigen.value,"eigen.fit"=eigen.fit, "threshold"=threshold)
  }
  return(ret)
}


