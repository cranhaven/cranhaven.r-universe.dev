#' Make split softening optimized with Nelder-Mead.
#'
#' This softening configures all parameters in the tree
#' with optimization method Nelder-Mead to minimize the given `miss' function.
#'
#' @param tr The soft tree
#' @param d The data set to be used in intialization for determining data boundaries
#'    and in optimization step to evaluate the objective function on the predictions
#'    on this data set by the soft tree with updated softening parameters.
#' @param miss.fn Function to provide the value of the objective function for optimization.
#'
#'    The function obtains as an argument the matrix of class probabilities
#'    as returned by \code{\link{predictSoftsplits}}
#'    when making predictions for the data set \code{d} using the soft tree \code{tr}
#'    but with some softening parameters reset within optimization procedure.
#'    The function is expected to return one numeric value;
#'    this value is minimized by the optimization method.
#' @param verbosity The verbosity level configures how many additional information is printed
#' @param implementation Indentify implementation of optimizer.
#' @param iteration.count Number of optimizer iterations.
#' @param sft.ini Parameter of softening used as the initial value for the optimization.
#'
#'  \describe{
#'    \item{\code{"gsl"}}{ uses \code{multimin} function from \code{gsl} package.}
#'    \item{\code{"R"}}{ uses \code{optim} - the standard optimization function in R.}
#'  }
#'
#' @return The soft tree with the new softening parameters
#'
#' @importFrom "stats" "optim"
#' @export
softening.optimized <- function( tr, d, miss.fn, verbosity=0, implementation=c("gsl", "R"), iteration.count=NULL, sft.ini=1 ) {
  if (implementation=="gsl" && !requireNamespace("gsl")) {
    stop( "To use the Nelder-Mead optimization method for softening when the implementation argument is \"gsl\" the package `gsl' is required." )
  }
  split.index <- tr$ncat %in% c( -1L, 1L )
  split.count <- sum(split.index)
  bounds <- tree.data.bounds( tr, d )
  widths <- data.frame( lb=tr$splits-bounds$lb, ub=bounds$ub-tr$splits )
  new.widths <- c(widths$lb[split.index],widths$ub[split.index])/2
  scale <- new.widths
  bad.scale.index <- (scale<=0)
  if ( any( bad.scale.index ) ) {
    if ( verbosity > 0 )
    {
      print( "Fixing zero scale!!" )
    }
    if ( verbosity > 5 )
    {
      print( scale )
    }
    scale[bad.scale.index] <- 1E-4
  }

  #test.para <- new.widths
  #print(sprintf("test value: %f", eval.attached(c(tr$splits[split.index]-test.para[1:split.count],
  #              tr$splits[split.index]+test.para[(split.count+1):(2*split.count)]))))
  softsplits.params <- function( para ) {
    transformed.para <- scale*para^2
    return( c(tr$splits[split.index]-transformed.para[1:split.count],
          tr$splits[split.index]+transformed.para[(split.count+1):(2*split.count)]) )
  }
  eval.sq <- function(para) {
    tr.para <- softsplits.params( para )
    tr$lb[split.index] <- tr.para[1:split.count]
    tr$ub[split.index] <- tr.para[(split.count+1):(2*split.count)]
    return( miss.fn(predictSoftsplits(tr, d)) )
  }

  para.ini <- rep_len( sft.ini, length(new.widths) )
  if (is.null(iteration.count)) {
      iteration.count <- (200*split.count)
  }

  if ( verbosity > 3 ) {
    print(sprintf("optimizing bounds using NM, in %d iterations. Initial value = %f",iteration.count,eval.sq(para.ini)))
  }
  if (implementation=="gsl") {
    optim.state <- gsl::multimin.init(para.ini,eval.sq,method="nm")
    for (i in 1:iteration.count) {
      optim.state <- gsl::multimin.iterate(optim.state)
      if ( verbosity > 4 ) {
        if ( 0 == i %% 100 ) print(sprintf("after iteration %d: value = %f", i, optim.state$f))
      }
    }
    para.opt <- optim.state$x
    value <- optim.state$f
  } else if (implementation=="R") {
    optim.result <- optim(para.ini, eval.sq, method="Nelder-Mead", control=list(trace=max(0, verbosity-4), maxit=iteration.count))
    para.opt <- optim.result$par
    value <- optim.result$value
  } else {
    stop("Wrong value of 'implementation' parameter");
  }

  if ( verbosity > 3 ) {
    print(sprintf("optimization done, value = %f",value))
  }

  ssp <- softsplits.params( para.opt )
  tr$lb[split.index] <- ssp[1:split.count]
  tr$ub[split.index] <- ssp[(split.count+1):(2*split.count)]
  return(tr)
}

