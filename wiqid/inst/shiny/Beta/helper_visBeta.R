
# Helper functions for shiny app "visBeta"
# =======================================

# The plotting function
# ---------------------

visBeta <- function (shapes, data,
  showPost=FALSE, npoints=100, param)
{
  # if(length(shapes) < 1)
    # shapes <- c(0, 0)
  shapes <- pmax(shapes, 0.0001)
  ## Calculation of the binomial likelihood curve
  xx <- c(0, seq(0.0002, 0.9998, length=npoints-2), 1)
  if(data[2] > 0) {
    binom.curve <- dbinom(data[1], data[2], xx) * (data[2]+1)
    binom.max <- max(binom.curve)
  } else {
    binom.max <- 0
  }

  ## Calculation of the posterior curve
  if(data[2] > 0 && showPost == 1) {
    post.curve <- pmin(dbeta(xx, shapes[1]+data[1], shapes[2]+diff(data)), 1e100)
    post.mean <- (shapes[1]+data[1]) / (sum(shapes)+data[2])
    post.max <- min(max(post.curve), 20)
  } else {
    post.max <- 0
  }
  beta.curve <- pmin(dbeta(xx, shapes[1], shapes[2]), 1e100)
  beta.max <- min(max(beta.curve), 20)
  mu <- shapes[1] / sum(shapes)
  ylim <- range(0, beta.max, binom.max, post.max)
  plot(0:1, ylim, type='n', las=1,
      xlab = expression(theta), ylab = "Probability density")
  segments(0,0,1,0, col='lightgrey')
  if(data[2] > 0) {
    lines(xx, binom.curve, lwd=3, col='lightgrey')
    points(rep(data[1]/data[2], 2), c(0, binom.max),
      col='lightgrey', pch=19, cex=2)
  }
  lines(xx, beta.curve, col='red', lwd=2)
  if(data[2] > 0 && showPost == 1) {
    lines(xx, post.curve, lwd=2, lty=2, col='blue')
    abline(v=post.mean, col='blue', lty=4)
  }
  abline(v=mu, col='red', lty=3)
  msg <- if(param == "useMode") {"Prior controlled by Mode and Concentration"
    } else {"Prior controlled by the shape parameters"}
  mtext(msg, 3)
}

# ......................................................................

# The results table function
# --------------------------

resultsBeta <- function (shapes, data,
  showPost=FALSE)
{
  shapes <- pmax(shapes, 0.0001)
  priorMean <- round(shapes[1] / sum(shapes), 2)
  priorSD <- round(sqrt(prod(shapes)) /
      (sum(shapes)^2 * (sum(shapes) + 1)), 2)
  priorMode <- round((shapes[1] -1) / sum(shapes -1), 2)
  priorConc <- round(sum(shapes), 2)
  res <- matrix(c("Beta prior(shape1, shape2)",
    as.character(c(shapes, priorMean, priorSD, priorMode, priorConc))), nrow=1)
  if(data[2] > 0) {
    fail <- diff(data)
    likMode <- round(data[1] / data[2], 2)
    res <- rbind(res, as.character(c("Binomial data (successes, failures)",
        data[1], fail, "", "", likMode, "")))
    if(showPost) {
      postShape1 <- shapes[1] + data[1]
      postShape2 <- shapes[2] + fail
      postMean <- round(postShape1 / (postShape1 + postShape2), 2)
      postSD <- round(sqrt((postShape1 * postShape2) /
          ((postShape1+postShape2)^2 * (postShape1 + postShape2 + 1))), 2)
      postMode <- round((postShape1-1) / (postShape1 + postShape2-2), 2)
      postConc <- round(postShape1+postShape2, 2)
      res <- rbind(res, as.character(c("Posterior beta(shape1, shape2)",
          postShape1, postShape2, postMean, postSD, postMode, postConc)))
    }
  }
  # Convert to data frame
  resDF <- as.data.frame(res, stringsAsFactors=FALSE)
  colnames(resDF) <- c("Component", "Param1", "Param2", "Mean", "SD", "Mode", "Concentration")
  return(resDF)
}
