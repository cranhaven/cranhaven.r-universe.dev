#' Updating the posterior distribution as part of the MCMC estimation process
#' A procedure which performs a single update of the posterior distribution
#'
#' @param i index of the draw number (i>1)
#' @param draws a matrix containing the posterior draws to update
#' @param pscOb an pscOb object which has been passed through pscData() and
#' init() functions
#' @return An updated set of posterior draws
#' @export
pscEst_update <- function(i,draws,pscOb){

  ### error trap
  if(i==1) stop("i must be > 1")

  ### Getting last state
  last <- draws[i-1,];last
  lastBeta <- last[-which(names(last)%in%names(pscOb$co))];lastBeta
  lastBeta <- lastBeta[-length(lastBeta)];lastBeta

  ### drawing candidate from posterior
  drawPost <- pscOb$cfmPost(1);drawPost

  ### drawing candidate from target
  drawBeta <- pscOb$target(1);drawBeta


  ### Priors
  prior_last <- pscOb$betaPrior(lastBeta);prior_last
  prior_draw <- pscOb$betaPrior(drawBeta);prior_draw

  ### estimating likelihoods
  pscOb_est <- pscOb
  pscOb_est$co <- drawPost
  names(pscOb_est$co) <- names(pscOb$co)

  lold <- -pscOb$lik(lastBeta,pscOb_est) + prior_last;lold
  lnew <- -pscOb$lik(drawBeta,pscOb_est) + prior_draw;lnew

  ### evaluating outcomes
  cond <- acc(lnew,lold);cond

  ### Saving outcomes
  if(cond){
    ret  <- c(drawPost,drawBeta,lnew)
  }
  if(!cond){
    ret <- c(drawPost,lastBeta,lold)
  }
  ret

}
