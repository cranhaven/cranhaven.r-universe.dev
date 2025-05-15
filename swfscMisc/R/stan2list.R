#' @title Convert STAN posterior to list
#' @description Convert `stan` posterior to named list of vectors or arrays.
#'
#' @param x list of class `stan`. The output from a call to 
#'   \link[rstan]{stan}.
#' @param collapse.chains return array with dimension for each chain?
#'
#' @note If \code{collapse.chains = TRUE}, the last dimension of arrays will always 
#'   be samples from the posterior. If \code{collapse.chains = FALSE}, the last 
#'   dimension of arrays will be individual chains, and the one prior to that 
#'   will be samples from the posterior for each chain.
#' 
#' @seealso
#'   \link[base]{aperm} to transpose the array if necessary.   
#'   \link[base]{as.data.frame.table} to convert arrays to data.frames.
#'   
#' @export
# 
stan2list <- function(x, collapse.chains = TRUE) {
  if(!methods::is(x, "stanfit")) stop("'x' must be of class 'stanfit'")
  mcmc2list(
    x = rstan::As.mcmc.list(x), 
    pars = x@model_pars, 
    collapse.chains = collapse.chains
  )
}
