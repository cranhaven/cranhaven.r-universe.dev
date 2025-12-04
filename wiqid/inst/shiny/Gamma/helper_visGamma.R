
# Helper functions for shiny app "visGamma"
# =========================================

# The plotting function
# ---------------------

visGamma <- function (shape=2, rate=2, count=0, units=0, 
  showPost=FALSE, npoints=100, xmax=10, param) 
{
  shape <- max(shape, 0.0001)
  rate <- max(rate, 0.0001)
  if(units > 0)
    xmax <- max(xmax, round(2 * count / units))
  ## Calculation of the Poisson likelihood curve
  xx <- c(0, seq(0.0002, xmax, length=npoints - 1))
  if(units > 0) {
    lik.curve <- dpois(count, xx*units) * units
    lik.max <- max(lik.curve)
  } else {
    lik.max <- 0
  }
  
  ## Calculation of the posterior curve
  if(units > 0 && showPost) {
    post.curve <- pmin(dgamma(xx, shape+count, rate+units), 1e100)
    post.max <- min(max(post.curve), 3)
    post.mean <- (shape+count)  / (rate+units)
  } else {
    post.max <- 0
  }
  prior.curve <- pmin(dgamma(xx, shape, rate), 1e100)
  prior.max <- min(max(prior.curve), 3)
  prior.mean <- shape / rate
  ylim <- range(0, prior.max, lik.max, post.max)
  plot(c(0, xmax), ylim, type='n', las=1, 
      xlab = expression(lambda), ylab = "Probability density")
  segments(0,0,xmax,0, col='lightgrey')
  if(units > 0) {
    lines(xx, lik.curve, lwd=3, col='lightgrey')
    points(rep(count/units, 2), c(0, lik.max), 
      col='lightgrey', pch=19, cex=2)
  }
  lines(xx, prior.curve, col='red', lwd=2)
  if(units > 0 && showPost == 1) {
    lines(xx, post.curve, lwd=2, lty=2, col='blue')
    abline(v=post.mean, col='blue', lty=4)
  }
  abline(v=prior.mean, col='red', lty=3)
  msg <- if(param == "useMode") { "Prior controlled by Mode and Rate" 
  } else { "Prior controlled by Shape and Rate"}
  mtext(msg, 3)
}

# ......................................................................

# The results table function
# --------------------------

resultsGamma <- function (shape=2, rate=2, count=0, units=0, 
  showPost=FALSE) 
{
  shape <- max(shape, 0.0001)
  rate <- max(rate, 0.0001)
  priorMean <- round(shape / rate, 2)
  priorSD <- round(sqrt(shape) / rate, 2)
  priorMode <- round(max((shape-1)/rate, 0), 2)
  res <- matrix(c("Gamma prior(shape, rate)",
    as.character(c(shape, rate, priorMean, priorSD, priorMode))), nrow=1)
  if(units > 0) {
    res <- rbind(res, c("Poisson data (count, units)",
        as.character(c(count, units)), "", "", round(count/units,2)))
    if(showPost) {
      postShape <- shape + count
      postRate <- rate + units
      postMean <- round(postShape / postRate, 2)
      postSD <- round(sqrt(postShape) / postRate, 2)
      postMode <- round(max((postShape-1)/postRate, 0), 2)
      res <- rbind(res, c("Posterior gamma(shape, rate)", 
          as.character(c(postShape, postRate, postMean, postSD, postMode))))
    }
  }
  # Convert to data frame
  resDF <- as.data.frame(res, stringsAsFactors=FALSE)
  colnames(resDF) <- c("Component", "Param1", "Param2", "Mean", "SD", "Mode")
  return(resDF)
}
