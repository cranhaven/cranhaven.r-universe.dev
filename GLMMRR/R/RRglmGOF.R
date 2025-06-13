#' Goodness-of-fit statistics for binary Randomized Response data
#'
#' Compute goodness-of-fit statistics for binary Randomized Response data.
#' Pearson, Deviance and Hosmer-Lemeshow statistics are available.
#'
#' @param RRglmOutput
#' a model fitted with the \code{\link{RRglm}} function.
#' @param doPearson
#' compute Pearson statistic.
#' @param doDeviance
#' compute Deviance statistic.
#' @param doHlemeshow
#' compute Hosmer-Lemeshow statistic.
#' @param hlemeshowGroups
#' number of groups to split the data into for the Hosmer-Lemeshow statistic (default: 10).
#' @param rm.na
#' remove cases with missing data.
#'
#' @return
#' an option of class RRglmGOF.
#' @export
#'
#' @examples
#'out <- RRglm(response ~ Gender + RR + pp + age, link="RRlink.logit", RRmodel=RRmodel,
#'          p1=RRp1, p2=RRp2, data=Plagiarism, etastart=rep(0.01, nrow(Plagiarism)))
#'RRglmGOF(RRglmOutput = out, doPearson = TRUE, doDeviance = TRUE, doHlemeshow = TRUE)
RRglmGOF <- function(RRglmOutput, doPearson = TRUE, doDeviance = TRUE, doHlemeshow = TRUE, hlemeshowGroups = 10, rm.na = TRUE)
{
  # Initialize
  pearson <- list(do = doPearson, obs = NULL, exp = NULL, res = NULL, stat = NA, pvalue = NA, df = NA, nGroup = NA)
  deviance <- list(do = doDeviance, obs = NULL, exp = NULL, res = NULL, stat = NA, pvalue = NA, df = NA, nGroup = NA)
  hlemeshow <- list(do = doHlemeshow, stat = NA, pvalue = NA, df = NA, overview = NULL, nGroup = hlemeshowGroups)

  # Get variables in formula
  vars <- all.vars(RRglmOutput$formula)

  # Create a data frame to work with
  df.work <- data.frame(y.obs = RRglmOutput$model[, vars[1]], RRglmOutput$model[, vars[2:length(vars)]])

  # Get fitted values
  y.fitted.tmp <- RRglmOutput$fitted.values

  # Handle NA's
  if (rm.na)
  {
    df.work <- na.omit(df.work)
    y.fitted.tmp <- na.omit(y.fitted.tmp)
  }

  # Attach fitted values at the beginning
  df.work <- data.frame(y.fitted = y.fitted.tmp, df.work)

  if (doPearson || doDeviance)
  {
    # Determine possible unique groups, or clusters in the data
    df.x <- data.frame(df.work[, 3:ncol(df.work)])
    factor.groups <- getUniqueGroups(df.x)

    # Number of unique groups
    nGroup <- length(levels(factor.groups))

    # Saved for print function
    pearson$nGroup <- nGroup
    deviance$nGroup <- nGroup

    # Number of estimated parameters
    nParam <- length(RRglmOutput$coeff)

    if(nGroup <= nParam)
    {
      cat("The Pearson Fit statistic is defined for an unsaturated model","\n")
      cat("The number of unique groups should be higher than the number of estimated parameters","\n")
    }
    else
    {
      # Expected cell proportions
      vec.pihat <- getCellMeans(y = df.work$y.fitted, factor.groups = factor.groups)

      # Observed cell proportions
      vec.prophat <- getCellMeans(y = df.work$y.obs, factor.groups = factor.groups)

      # Number of observations per group
      vec.groupN <- getCellSizes(n = length(df.work$y.obs), factor.groups = factor.groups)

      # Pearson statistic
      if (doPearson)
      {
        pearson$obs <- vec.prophat
        pearson$exp <- vec.pihat
        pearson$res <- (vec.prophat - vec.pihat) / sqrt(vec.pihat * (1 - vec.pihat) / vec.groupN)
        pearson$stat <- sum(vec.groupN * (((vec.prophat - vec.pihat)^2) / (vec.pihat * (1 - vec.pihat))))
        pearson$df <- nGroup - nParam
        pearson$pvalue = 1 - pchisq(pearson$stat, df = pearson$df)
      }

      # Deviance statistic
      if (doDeviance)
      {
        # 0 and 1 not possible
        vec.prophat[which(vec.prophat == 0)] <- 1e-15
        vec.prophat[which(vec.prophat == 1)] <- 1-1e-15
        deviance$obs <- vec.prophat
        deviance$exp <- vec.pihat

        sign <- rep(1, length(vec.prophat))
        sign[which(vec.prophat < vec.pihat)] <- -1
        deviance$res <- sign * sqrt(2 * vec.groupN * (vec.prophat * log(vec.prophat / vec.pihat) + (1 - vec.prophat) * log((1 - vec.prophat) / (1 - vec.pihat))))
        deviance$stat <- 2 * sum(vec.groupN * (vec.prophat * log(vec.prophat / vec.pihat) + (1 - vec.prophat) * log((1 - vec.prophat) / (1 - vec.pihat))))
        deviance$df <- nGroup - nParam
        deviance$pvalue = 1 - pchisq(deviance$stat, df = deviance$df)
      }
    }
  }

  if(doHlemeshow)
  {
    # 1: Expected probabilities
    # 2: Observed probabilities
    # 3: Index used to set cutting points for equally sized groups
    probmatrix = matrix(nrow = nrow(df.work), ncol = 2)
    probmatrix[, 1] = df.work$y.fitted
    probmatrix[, 2] = df.work$y.obs

    # Order by expected probabilities
    probmatrix <- probmatrix[order(probmatrix[, 1], decreasing=FALSE), ]

    # Set index
    probmatrix <- cbind(probmatrix, seq(1, nrow(df.work)))

    # Split matrix into given number of groups
    breaks <- quantile(probmatrix[, 3], probs = seq(0, 1, by = 1 / hlemeshowGroups))
    probmatrix[, 3] <- cut(probmatrix[, 3], breaks = breaks, include.lowest = TRUE)
    problistSplitted <- split(as.data.frame(probmatrix), probmatrix[, 3])

    # 1: Predicted probability success -> P
    # 2: Predicted probability failure -> 1 - P
    # 3: Observed probability success -> P
    # 4: Observed probability failure -> 1 - P
    # 5/6: Predicted successes/failures
    # 7/8: Observed successes/failures
    # 9: Hosmer Lemeshow Statistic for group n
    # 10: Residual for group n
    hlmatrix = matrix(nrow = hlemeshowGroups, ncol = 10)
    for(ii in 1:hlemeshowGroups)
    {

      predictedProbs <- problistSplitted[[ii]][[1]]
      observedProbs <- problistSplitted[[ii]][[2]]
      nObservations <- length(problistSplitted[[ii]][[2]])
      nObservedSuccesses <- sum(problistSplitted[[ii]][[2]])

      hlmatrix[ii, 1] <- mean(predictedProbs)
      hlmatrix[ii, 3] <- mean(observedProbs)
      hlmatrix[ii, 2] <- mean(1 - predictedProbs)
      hlmatrix[ii, 4] <- mean(1 - observedProbs)

      hlmatrix[ii, 5] <- hlmatrix[ii,1] * nObservations
      hlmatrix[ii, 6] <- nObservations - hlmatrix[ii, 5]
      hlmatrix[ii, 7] <- nObservedSuccesses
      hlmatrix[ii, 8] <- nObservations - hlmatrix[ii, 7]

      ## From book "Regression for Categorical Data" by Tutz
      nObservationsInGroup <- nObservations
      avgPredictedSuccesses <- hlmatrix[ii, 1] #hlmatrix[ii,5]/ nObservationsInGroup
      avgObservedSuccesses <- hlmatrix[ii, 3] #hlmatrix[ii,7]/ nObservationsInGroup

      hlmatrix[ii, 9] <- nObservationsInGroup * (((avgObservedSuccesses - avgPredictedSuccesses)^2) / (avgPredictedSuccesses * (1 - avgPredictedSuccesses)))
      hlmatrix[ii, 10] <- (avgObservedSuccesses - avgPredictedSuccesses)/sqrt(avgPredictedSuccesses * (1 - avgPredictedSuccesses) / nObservationsInGroup)

    }

    df.hlemeshow <- data.frame(obs = hlmatrix[, 7] + hlmatrix[, 8], exp.suc = hlmatrix[, 5], obs.suc = hlmatrix[, 7],
                               stat = hlmatrix[, 9], res = hlmatrix[, 10])
    colnames(df.hlemeshow) <- c("Observations", "Expected successes", "Observed successes", "H-L Statistic", "Pearson Residuals")
    rownames(df.hlemeshow) <- paste("Group", seq(1, hlemeshowGroups))

    hlemeshow$stat <- sum(hlmatrix[, 9])
    hlemeshow$df <- hlemeshowGroups - 2
    hlemeshow$pvalue <- 1 - pchisq(hlemeshow$stat, df = hlemeshow$df)
    hlemeshow$overview <- df.hlemeshow
  }

  ls.return <- list(pearson = pearson, deviance = deviance, hlemeshow = hlemeshow, vars = vars, n = nrow(df.work), family = RRglmOutput$family)
  class(ls.return) <- "RRglmGOF"
  return(ls.return)
}

