#' Pre-process Run-Off Triangles
#'
#' Pre-process Run-Off Triangles.
#' @param cumulative.payments.triangle \code{triangle matrix} or \code{matrix array} object, input triangle of cumulative payments.
#' @param entries.weights \code{triangle matrix} or \code{matrix array} model entries weights.
#' @param eta \code{numeric}, individual claims exposure in the cell, also known as lost exposure. It must be in the interval (0,1].
#' 
#' @examples
#' data(sifa.mtpl)
#' sifa.mtpl.rtt <- AggregateDataPP(cumulative.payments.triangle=sifa.mtpl)
#' 
#' @return An object of class \code{AggregateDataPP}. Lists the following elements:
#'   \item{cumulative.payments.triangle}{\code{triangle matrix} object, input triangle of cumulative payments.}
#'   
#'   \item{occurrance}{\code{matrix array} object, the occurrence derived from the input triangle.}
#'   
#'   \item{exposure}{\code{matrix array} object, the exposure derived from the input triangle, under the \code{eta} claims arrival assumption.}
#'   
#'   \item{incremental.payments.triangle}{\code{triangle matrix} object, incremental payments derived from the input.}
#'   
#'   \item{fit.w}{\code{matrix array} object, the weights used during estimation.}
#'   
#'   \item{J}{\code{integer}, Run-off triangle dimension.}
#'   
#'   \item{diagonal}{ \code{numeric}, cumulative payments last diagonal.}
#'   
#'   \item{eta}{ \code{numeric}, Expected time-to-event in the cell. I.e., lost exposure.}
#'  
#'   
#' @references 
#' Pittarello, G., Hiabu, M., & Villegas, A. M. (2023). Replicating and extending chain-ladder via an age-period-cohort structure on the claim development in a run-off triangle. arXiv preprint arXiv:2301.03858.
#' 
#' @export
AggregateDataPP <- function(cumulative.payments.triangle, entries.weights=NULL, eta=1/2)
{
  
  rtt.input.env$properties.cpt(cumulative.payments.triangle)
  
  incrementals = ChainLadder::cum2incr(cumulative.payments.triangle)
  J=dim(cumulative.payments.triangle)[2]
  
  # find out occurrance and exposure
  occurrance=pkg.env$t2c(incrementals)
  drop=1-eta
  exposure=pkg.env$t2c(cumulative.payments.triangle-drop*incrementals)
  
  
  # find out the weights
  if(is.null(entries.weights)){
    
    fit.w <- matrix(1,nrow=J,ncol = J) 

    
    }
  else{
    
    fit.w <- entries.weights
    
  }
  
  fit.w[,1]=0
  fit.w=pkg.env$t2c(fit.w)
  fit.w[is.na(fit.w)]=0
  
  
  tr <- list(
    cumulative.payments.triangle = cumulative.payments.triangle,
    occurrance = occurrance,
    exposure = exposure,
    fit.w=fit.w,
    incremental.payments.triangle = incrementals,
    J=J,
    diagonal=pkg.env$t2c(cumulative.payments.triangle)[,J],
    eta=eta
  )
  
  ## Set the name for the class
  class(tr) <- "AggregateDataPP"
  tr
  
}


