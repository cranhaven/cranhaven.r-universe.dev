#' plot the cross-validation curve produced by cv.classo
#'
#' Plots the cross-validation curve, and upper and lower standard deviation
#' curves, as a function of the \code{lambda} values used.
#'
#' A plot is produced, and nothing is returned.
#'
#' @aliases plot.cv.classo
#' @param x fitted \code{"cv.classo"} object
#' @param sign.lambda Either plot against \code{log(lambda)} (default) or its
#' negative if \code{sign.lambda=-1}.
#' @param \dots Other graphical parameters to plot.
#' 
#' @return No return value, called for side effects (produces a plot).
#' 
#' @author Navonil Deb, Younghoon Kim, Sumanta Basu \cr Maintainer: Younghoon Kim
#' \email{yk748@cornell.edu}
#' @seealso \code{classo} and \code{plot} methods for \code{"cv.classo"}.
#' @examples
#' \donttest{ 
#' set.seed(1010)
#' n = 1000
#' p = 200
#' x = array(rnorm(n*p), c(n,p)) + (1+1i) * array(rnorm(n*p), c(n,p))
#' for (j in 1:p) x[,j] = x[,j] / sqrt(mean(Mod(x[,j])^2))
#' e = rnorm(n) + (1+1i) * rnorm(n)
#' b = c(1, -1, rep(0, p-2)) + (1+1i) * c(-0.5, 2, rep(0, p-2))
#' y = x %*% b + e
#' cv.test = cv.classo(x,y)
#' plot(cv.test)
#' }
#' @method plot cv.classo
#' @export
#'
plot.cv.classo <- function(x,sign.lambda=1,...){

  # cvobj <- cv_result
  cvobj <- x
  xlab <- expression(Log(lambda))
  #  xlab="log(Lambda)"
  if(sign.lambda<0){
    xlab <- paste("-",xlab,sep="")
  }
  plot.args <- list(x=sign.lambda*log(cvobj$lambda),
                    y=cvobj$cvm,ylim=range(cvobj$cvup,cvobj$cvlo),xlab=xlab,ylab=cvobj$name,type="n")
  # new.args <- list(...)
  # if(length(new.args)){
  #   plot.args[names(new.args)]<-new.args
  # }

  # ------------------------------------------------ #
  do.call("plot",plot.args)
  error.bars(sign.lambda*log(cvobj$lambda),cvobj$cvup,cvobj$cvlo,width=0.01,
             col="darkgrey")

  points(sign.lambda*log(cvobj$lambda),cvobj$cvm,pch=20,col="red")
  # axis(side=3,at=sign.lambda*log(cvobj$lambda),labels=paste(cvobj$nz),tick=FALSE,line=0)
  abline(v=sign.lambda*log(cvobj$lambda.min),lty=3)
  abline(v=sign.lambda*log(cvobj$lambda.1se),lty=3)
  invisible()
}

