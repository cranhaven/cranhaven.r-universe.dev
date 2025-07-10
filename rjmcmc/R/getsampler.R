#' Define Function To Sample From MCMC Output
#' 
#' A utility function which accepts a matrix of MCMC output and creates a 
#' function which samples from the posterior distribution for the parameters of 
#' the model.
#' 
#' @param modelfit A matrix of output from a previously-run MCMC algorithm, with 
#' one column per variable and one row per iteration.
#' @param sampler.name A string giving the desired name for the function to be 
#'   defined.
#' @param order A numeric vector of indices specifying the desired parameters to
#'  extract from \code{modelfit}, and in which order.
#' @param envir The environment in which to define the sampling function. 
#'   Defaults to the global environment.
#' @return A function is defined in \code{envir} which randomly samples from the
#'   posterior distribution for the parameters. Note that this function does not
#'   take any arguments. A function generated in this way is suitable for 
#'   passing to the \code{rjmcmcpost} function.
#'   
#' @references Plummer, M. (2003) JAGS: A program for analysis of Bayesian 
#'   graphical models using Gibbs sampling. \emph{Proceedings of the 3rd 
#'   international workshop on distributed statistical computing (Vol. 124, p. 
#'   125)}.
#'   
#' @seealso \code{\link{rjmcmcpost}}
#' @examples
#' # Generate synthetic 'MCMC output' for a model with 3 parameters. There is
#' # one column per parameter, and 1000 iterations.
#' matrix_output = matrix(c(runif(1000,0,1), rnorm(1000,5,2), rgamma(1000,2,2)), 1000, 3)
#' 
#' getsampler(modelfit=matrix_output, sampler.name="posterior1")
#' set.seed(100)
#' posterior1()
#' 
#' ## Alternatively
#' posterior1b = getsampler(modelfit=matrix_output)  # this successfully defines a function named
#' # posterior1b but also defines an identical function corresponding to the value 
#' # of sampler.name, i.e. the default "post.draw" in this case.
#' set.seed(100)
#' posterior1b()
#' set.seed(100)
#' posterior1()
#' 
#' @export
getsampler = function(modelfit, sampler.name="post.draw", order="default", envir=.GlobalEnv){
  if(any(order == "default")){
    ord = 1:dim(modelfit)[2]
  } else {
    ord = order
  }
  tmp = paste("function() {\n temp = ",deparse(substitute(modelfit)),"[sample(dim(",
              deparse(substitute(modelfit)),")[1],1),c(",paste(ord,collapse=","),")]\n }",sep="")
  assign(sampler.name,eval(parse(text = tmp)), envir = envir)
}
