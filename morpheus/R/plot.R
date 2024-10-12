# extractParam
#
# Extract successive values of a projection of the parameter(s).
# The method works both on a list of lists of results,
# or on a single list of parameters matrices.
#
# @inheritParams plotHist
#
.extractParam <- function(mr, x=1, y=1)
{
  if (is.list(mr[[1]]))
  {
    # Obtain L vectors where L = number of res lists in mr
    return ( lapply( mr, function(mr_list) {
      sapply(mr_list, function(m) m[x,y])
    } ) )
  }
  sapply(mr, function(m) m[x,y])
}

#' plotHist
#'
#' Plot compared histograms of a single parameter (scalar)
#'
#' @name plotHist
#'
#' @param mr Output of multiRun(), list of lists of functions results
#' @param x Row index of the element inside the aggregated parameter
#' @param y Column index of the element inside the aggregated parameter
#' @param ... Additional graphical parameters (xlab, ylab, ...)
#'
#' @examples
#' \dontrun{
#' beta <- matrix(c(1,-2,3,1),ncol=2)
#' mr <- multiRun(...) #see bootstrap example in ?multiRun
#'                     #mr[[i]] is a list of estimated parameters matrices
#' mu <- normalize(beta)
#' for (i in 1:2)
#'   mr[[i]] <- alignMatrices(res[[i]], ref=mu, ls_mode="exact")
#' plotHist(mr, 2, 1) #second row, first column}
#'
#' @export
plotHist <- function(mr, x, y, ...)
{
  params <- .extractParam(mr, x, y)
  L <- length(params)
  # Plot histograms side by side
  par(mfrow=c(1,L), cex.axis=1.5, cex.lab=1.5, mar=c(4.7,5,1,1))
  args <- list(...)
  for (i in 1:L)
  {
    hist(params[[i]], breaks=40, freq=FALSE,
      xlab=ifelse("xlab" %in% names(args), args$xlab, "Parameter value"),
      ylab=ifelse("ylab" %in% names(args), args$ylab, "Density"))
  }
}

# NOTE: roxygen2 bug, "@inheritParams plotHist" fails in next header:

#' plotBox
#'
#' Draw compared boxplots of a single parameter (scalar)
#'
#' @name plotBox
#'
#' @param mr Output of multiRun(), list of lists of functions results
#' @param x Row index of the element inside the aggregated parameter
#' @param y Column index of the element inside the aggregated parameter
#' @param ... Additional graphical parameters (xlab, ylab, ...)
#'
#' @examples
#' \dontrun{
#' beta <- matrix(c(1,-2,3,1),ncol=2)
#' mr <- multiRun(...) #see bootstrap example in ?multiRun
#'                     #mr[[i]] is a list of estimated parameters matrices
#' mu <- normalize(beta)
#' for (i in 1:2)
#'   mr[[i]] <- alignMatrices(res[[i]], ref=mu, ls_mode="exact")
#' plotBox(mr, 2, 1) #second row, first column}
#'
#' @export
plotBox <- function(mr, x, y, ...)
{
  params <- .extractParam(mr, x, y)
  L <- length(params)
  # Plot boxplots side by side
  par(mfrow=c(1,L), cex.axis=1.5, cex.lab=1.5, mar=c(4.7,5,1,1))
  args <- list(...)
  for (i in 1:L)
  {
    boxplot(params[[i]],
      ifelse("ylab" %in% names(args), args$ylab, "Parameter value"))
  }
}

#' plotCoefs
#'
#' Draw a graph of (averaged) coefficients estimations with their standard,
#' deviations ordered by mean values.
#' Note that the drawing does not correspond to a function; it is just a
#' convenient way to visualize the estimated parameters.
#'
#' @name plotCoefs
#'
#' @param mr List of parameters matrices
#' @param params True value of the parameters matrix
#' @param ... Additional graphical parameters
#'
#' @examples
#' \dontrun{
#' beta <- matrix(c(1,-2,3,1),ncol=2)
#' mr <- multiRun(...) #see bootstrap example in ?multiRun
#'                     #mr[[i]] is a list of estimated parameters matrices
#' mu <- normalize(beta)
#' for (i in 1:2)
#'   mr[[i]] <- alignMatrices(res[[i]], ref=mu, ls_mode="exact")
#' params <- rbind( c(.5,.5), beta, c(0,0) ) #p, beta, b stacked in a matrix
#' plotCoefs(mr[[1]], params)}
#'
#' @export
plotCoefs <- function(mr, params, ...)
{
  d <- nrow(mr[[1]])
  K <- ncol(mr[[1]])

  params_hat <- matrix(nrow=d, ncol=K)
  stdev <- matrix(nrow=d, ncol=K)
  for (x in 1:d)
  {
    for (y in 1:K)
    {
      estims <- .extractParam(mr, x, y)
      params_hat[x,y] <- mean(estims)
      # Another way to compute stdev: using distances to true params
#      stdev[x,y] <- sqrt( mean( (estims - params[x,y])^2 ) )
      # HACK remove extreme quantile in estims[[i]] before computing sd()
      stdev[x,y] <- sd(estims) #[ estims < max(estims) & estims > min(estims) ] )
    }
  }

  par(cex.axis=1.5, cex.lab=1.5, mar=c(4.7,5,1,1))
  params <- as.double(params)
  o <- order(params)
  avg_param <- as.double(params_hat)
  std_param <- as.double(stdev)
  args <- list(...)
  matplot(
    cbind(params[o],avg_param[o],
      avg_param[o]+std_param[o],avg_param[o]-std_param[o]),
    col=1, lty=c(1,5,3,3), type="l", lwd=2,
    xlab=ifelse("xlab" %in% names(args), args$xlab, "Parameter index"),
    ylab=ifelse("ylab" %in% names(args), args$ylab, "") )

  #print(o) #not returning o to avoid weird Jupyter issue... (TODO:)
}
