
#' Methods for the rj Class
#' 
#' An object of class \code{rj} is returned from the functions 
#' \code{\link{rjmcmcpost}} or \code{\link{defaultpost}}. The following methods 
#' can be applied to an object of this class. See Details for more information.
#' 
#' The \code{print} method prints the point estimates obtained from the 
#' algorithm, including the transition matrix, posterior model probabilities and
#' Bayes factors.
#' 
#' The \code{plot} method plots how the estimates of the posterior probabilities
#' changed as the algorithm progressed, illustrating convergence.
#' 
#' The \code{summary} method returns quantiles of the posterior densities for
#' each model (as well as likelihoods and prior densities). The point estimates
#' as in \code{print} are also returned. Note that this requires \code{save.all}
#' to be \code{TRUE} in the \code{rjmcmcpost} call.
#' 
#' @param x,object An object of class \code{rj}.
#' @param ... Further arguments to the generic method.
#' @param legend,col,ylim,lwd,lty Some useful graphical arguments to the generic
#'   \code{plot} method.
#' @param quantiles The desired density quantiles for \code{summary} to find.
#'   
#' @importFrom graphics lines plot
#' @name rjmethods
NULL

#' @rdname rjmethods
#' @export
print.rj = function(x, ...){
  if(any(names(x)=="result")){ 
    r = x$result
    cat('Transition Matrix: \n')
    print(r[[1]])
    cat('\nPosterior Model Probabilities: \n')
    print(r[[2]])
    cat('\nBayes Factors: \n')
    print(r[[3]])
    cat('\nSecond Eigenvalue: \n')
    print(r[[4]])
  } else { stop("No element named 'result'.") }
}

#' @rdname rjmethods
#' @export
plot.rj = function(x, legend=TRUE, col="maroon4", ylim=c(0,1), lwd=2, lty=c(1,1,1), ...){
  if(any(names(x)=="progress") & any(names(x)=="meta")){
    numplots = dim(x$progress$prb)[2]
    if(length(col)==1){ col = c(col, 2:numplots+2) }
    plot(seq(x$meta$TM.thin,x$meta$chainl,by=x$meta$TM.thin), x$progress$prb[,1], type="l", lty=lty, lwd=lwd, col=col[1], ylim=ylim,
         main="Estimated Posterior Model Probabilities", xlab="Number of Iterations", ylab="Estimated Posterior Probability")
    for(i in 2:numplots){
      lines(seq(x$meta$TM.thin,x$meta$chainl,by=x$meta$TM.thin), x$progress$prb[,i], lwd=lwd, col=col[i])
    }
    if(legend){ legend("topright", legend=paste("p(Model", 1:numplots, "| y)"), lwd=2, col=col) }
  } else { stop("Object does not contain 'progress' and 'meta' elements.") }
}

#' @rdname rjmethods
#' @export
summary.rj = function(object, quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), ...){
  if(any(names(object)=="densities")){ 
    dens = object$densities[[1]]
    for(i in 2:length(object$densities)){
      dens = rbind(dens, object$densities[[i]])
    }
    summ = list(object$result, t(apply(dens, 2, stats::quantile, quantiles)))
    class(summ) = 'summary.rj'
    return(summ) 
  } else { stop("No element named 'densities' -- ensure that save.all=TRUE.") }
}

print.summary.rj = function(x, digits=4, ...){
  cat('Transition Matrix: \n')
  print(x[[1]][[1]], digits=digits)
  cat('\nPosterior Model Probabilities: \n')
  print(x[[1]][[2]], digits=digits)
  cat('\nBayes Factors: \n')
  print(x[[1]][[3]], digits=digits)
  cat('\nSecond Eigenvalue: \n')
  print(x[[1]][[4]], digits=digits)
  cat('\n\nQuantiles for Posterior, Likelihood, and Prior Densities: \n')
  print(x[[2]], digits=digits)
  invisible(x)
}

rj = function(x){
  class(x) = c("rj", class(x))
  return(x)
}
