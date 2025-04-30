#' @keywords internal
q975 <- function(x)
{
quantile(x,0.975)
}

#' @keywords internal
q025 <- function(x){
quantile(x,0.025)
}

#' @title Summarizing \pkg{bcgam} fits 
#'
#' @description \code{summary} method for class \code{"bcgam"}.
#'
#' @param object an object of class \code{"bcgam"}.
#' @param ... further arguments passed to or from other methods.
#' 
#' @export 
#'
#' @details All summary statistics are based on the posterior distribution in the \code{bcgam} object.
#'
#' @return The function \code{summary.bcgam} computes and return a list of summary
#' statistics (estimated mean, standard error, 95\% bounds, estimated median) of the fitted \code{bcgam} given in \code{object}.
#' 
#' @author Cristian Oliva-Aviles and Mary C. Meyer
#' 
#' @examples
#' \dontrun{
#' n<-50
#' x<-(1:n)^{1/3}
#' z<-as.factor(rbinom(n, 1, 0.6))
#' y<-x+7*as.numeric(z)+rnorm(n,sd=2) 
#' bcgam.fit <- bcgam(y~sm.incr(x)+z, nloop=100)
#' summary(bcgam.fit)
#' }
summary.bcgam <- function(object, ...)
{
	mean.alpha=apply(object$alpha.sims,2,"mean")	
	sd.alpha=apply(object$alpha.sims,2,"sd")	
	q025.alpha=apply(object$alpha.sims,2,"q025")
	median.alpha=apply(object$alpha.sims,2,"median")		
	q975.alpha=apply(object$alpha.sims,2,"q975")	
	ans=cbind(mean.alpha,sd.alpha,q025.alpha,median.alpha, q975.alpha)
	dimnames(ans) <- list(object$znms, c("Mean", 
            "Std. Error", "2.5%","Median", "97.5%"))
	cat("Call:\n")
	print(object$call)
	cat("\nCoefficients:\n")	
	ans
}
