#########1#########2#########3#########4#########5#########6#########7#########8
#' Partial Regression Plot
#'
#' Plot the partial regression plot for one of the predictors of a linear model 
#'
#' @param mod A linear model object (obtained via the lm function)
#' @param pred The name (in quotes) of the predictor for which the plot should
#' be produced
#' @param ... Any other arguments to be passed to the plot 
#' @return A partial regression plot for \code{pred} in the linear model 
#' \code{mod}
#' @examples
#' lmod=lm(mpg~.,mtcars)
#' lmPartReg(lmod,"wt")
#' @export
################################################################################
lmPartReg<-function(mod,pred,...) {
  if (!inherits(mod,"lm")) stop("Not a supported model")
  if (length(mod$call)<3) stop("Must specify the dataset in the linear model")
  #Get the predictors
  preds=attr(terms(mod),"term.labels")
  if (!pred%in%preds) stop("Not a predictor in the model")
  #Get the response
  resp=as.character(mod$call[[2]][[2]])
  #Get the dataset
  datnm=mod$call[[3]]
  dat=eval(datnm)
  d=resid(lm(formula(paste(resp,"~",paste(preds[preds!=pred],collapse="+"))),dat))
  g=resid(lm(formula(paste(pred,"~",paste(preds[preds!=pred],collapse="+"))),dat))
  newmod=lm(d~g)
  plot(d~g,xlab=paste(pred,"residuals"), ylab=paste(datnm,"residuals"),...)
  graphics::abline(newmod, col=2)
  invisible(newmod)
}
