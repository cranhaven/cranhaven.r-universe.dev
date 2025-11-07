validate_predict <- function(gpModel, newData) {
  checkFitted(gpModel)

  # check for repeats of oldData in newData
  oldData <- getData(gpModel)
  oldData <- unique(oldData)
  newData <- unique(newData)
  data_all <- rbind(oldData, newData)

  if (anyDuplicated(data_all)) {
    stop("newData contains data that was used for fitting the model")
  }
}


#' GPPM predictions
#'
#' Obtain person-specific predictions.
#'
#' @inheritParams coef.GPPM
#' @param newData a data frame with the same column names as the data frame used for generating \code{gpModel} with \code{\link{gppm}}. May only contain new data, that is, data that was not used for fitting.
#' @return Predictions of the dependent variable for all rows in newData. Conditional predictions for all persons in newData that are also present
#' in the data used for fitting gpModel; unconditional predictions for others persons.
#' See examples for format.
#'
#' @examples
#' \donttest{
#' data("demoLGCM")
#' # remove all measurements from person 1 and the first form person 2
#' predIdx <- c(which(demoLGCM$ID == 1), which(demoLGCM$ID == 2)[1])
#' fitDemoLGCM <- demoLGCM[setdiff(1:nrow(demoLGCM), predIdx), ]
#'
#' lgcm <- gppm(
#'   "muI+muS*t", "varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma",
#'   fitDemoLGCM, "ID", "y"
#' )
#' lgcm <- fit(lgcm)
#' predRes <- predict(lgcm, demoLGCM[predIdx, ])
#' }
#' @export
predict.GPPM <- function(object, newData, ...) {
  validate_predict(object, newData)
  checkFitted(object)

  # get relevant old data
  oldData <- getData(object)
  IDfield <- getID(oldData)
  predictionIDs <- unique(newData[, IDfield])
  oldData <- oldData[oldData[, IDfield] %in% predictionIDs, ]

  # switch NA to stan representation
  DVField <- getDV(oldData)
  naPlaceHolder <- -81975897359872328957325.12904812903481249081249082497
  newData[is.na(newData[, DVField]), DVField] <- naPlaceHolder

  dataUpdate <- rbind(oldData, newData)

  # get model implied mean and covariance matrices for all relevant persons
  newModel <- updateData(object, dataUpdate)
  newModel <- fit(newModel, useOptimizer = FALSE, init = coef(object), hessian = FALSE, ...)
  meansAndCovs <- fitted(newModel)

  # calculate predictions
  res <- list(predMean = list(), predCov = list(), type = character(), preds = list(), trueVals = list(), train = list())
  res$ID <- meansAndCovs$ID
  res$DV <- DVField
  means <- meansAndCovs$mean
  covs <- meansAndCovs$cov
  for (i in seq_len(length(res$ID))) {
    borderOld <- sum(oldData[, IDfield] == res$ID[i])
    nTimePoints <- sum(dataUpdate[, IDfield] == res$ID[i])

    indexOld <- seq_len(borderOld)
    indexNew <- (borderOld + 1):nTimePoints
    onlyDataForPersonI <- dataUpdate[dataUpdate[, IDfield] == res$ID[i], ]
    res$preds[[i]] <- onlyDataForPersonI[indexNew, preds(object), drop = FALSE]
    tmp <- onlyDataForPersonI[indexNew, DVField, drop = TRUE]
    tmp[tmp == naPlaceHolder] <- NA
    res$trueVals[[i]] <- tmp
    fullMean <- means[[i]]
    fullCov <- covs[[i]]

    mNew <- fullMean[indexNew]
    cNew <- fullCov[indexNew, indexNew, drop = FALSE]
    # we have measurements for this person
    if (!length(indexOld) == 0) {
      mOld <- fullMean[indexOld]
      cOld <- fullCov[indexOld, indexOld, drop = FALSE]
      cOldInv <- solve(cOld)

      crossNewOld <- fullCov[indexNew, indexOld, drop = FALSE]
      crossOldNew <- fullCov[indexOld, indexNew, drop = FALSE]
      stopifnot(all.equal(crossNewOld, t(crossOldNew), tolerance = 1e-10)) # TODO why identical fail

      y <- as.matrix(getIntern(newModel, "stanData")$Y[[i]][indexOld])
      # calculate
      res$predMean[[i]] <- mNew + crossNewOld %*% cOldInv %*% (y - mOld)
      res$predCov[[i]] <- cNew - crossNewOld %*% cOldInv %*% crossOldNew
      res$type[i] <- "conditional"
      res$train[[i]] <- list(preds = onlyDataForPersonI[indexOld, preds(object), drop = FALSE], DV = "NA")
      res$train[[i]]$DV <- y
    } else {
      res$predMean[[i]] <- mNew
      res$predCov[[i]] <- cNew
      res$type[i] <- "unconditional"
    }
  }
  class(res) <- "GPPMPred"
  res
}

#' Accuracy Estimates for Predictions
#'
#' Estimate the accuracy based on predictions.
#'
#' @param predRes  object of class \code{GPPMPred} as obtained by \code{\link{predict.GPPM}}
#' @return accuracy estimates in the form of the mean squared error (MSE), the negative log-predictive probability (nLPP), and the sum squared error (SSE)
#'
#' @examples
#' \donttest{
#' data("demoLGCM")
#' # remove all measurements from person 1 and the first form person 2
#' predIdx <- c(which(demoLGCM$ID == 1), which(demoLGCM$ID == 2)[1])
#' fitDemoLGCM <- demoLGCM[setdiff(1:nrow(demoLGCM), predIdx), ]
#'
#' lgcm <- gppm(
#'   "muI+muS*t", "varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma",
#'   fitDemoLGCM, "ID", "y"
#' )
#' lgcm <- fit(lgcm)
#' predRes <- predict(lgcm, demoLGCM[predIdx, ])
#' accEsts <- accuracy(predRes)
#' accEsts$MSE # mean squared error
#' accEsts$nLPP # negative log-predictive probability
#' accEsts$MAE # mean absolute error
#' }
#' @export
accuracy <- function(predRes) {
  nPers <- length(predRes$ID)
  MSE <- c()
  absDist <- c()
  LL <- rep(NA, nPers)
  for (i in 1:nPers) {
    MSE <- c(MSE, as.numeric((predRes$predMean[[i]] - predRes$trueVals[[i]])^2))
    absDist <- c(absDist, as.numeric((abs(predRes$predMean[[i]] - predRes$trueVals[[i]]))))
    LL[i] <- log(mvtnorm::dmvnorm(t(predRes$trueVals[[i]]), mean = as.vector(predRes$predMean[[i]]), sigma = predRes$predCov[[i]]))
  }
  mMSE <- mean(MSE)
  mabsDist <- mean(absDist)
  sLL <- sum(LL)
  sSE <- sum(MSE)
  return(list(MSE = mMSE, nLPP = -sLL, SSE = sSE, mAE = mabsDist))
}


#' Plotting predictions
#'
#' Plots person-specific predictions
#'
#' @param x object of class \code{GPPMPred} as obtained by \code{\link{predict.GPPM}}
#' @param plotId character string or integer. ID of the person for which the predictions should be plotted
#' @param ... additional arguments (currently not used).
#' @return A plot visualizing the predictive distribution. The bold line describes the mean and the shaded area the 95\% credibility interval.
#'
#' @examples
#' \donttest{
#' data("demoLGCM")
#' # remove all measurements from person 1 and the first form person 2
#' predIdx <- c(which(demoLGCM$ID == 1), which(demoLGCM$ID == 2)[1])
#' fitDemoLGCM <- demoLGCM[setdiff(1:nrow(demoLGCM), predIdx), ]
#'
#' lgcm <- gppm(
#'   "muI+muS*t", "varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma",
#'   fitDemoLGCM, "ID", "y"
#' )
#' lgcm <- fit(lgcm)
#' predRes <- predict(lgcm, demoLGCM[predIdx, ])
#' plot(predRes, 1)
#' }
#' @method plot GPPMPred
#' @export
plot.GPPMPred <- function(x, plotId, ...) {
  stopifnot(length(plotId) == 1)
  idIdx <- plotId == x$ID
  stopifnot(sum(idIdx) == 1) # only for one subject
  idIdx <- which(idIdx) # solved indexing errors

  means <- x$predMean[[idIdx]]
  vars <- diag(x$predCov[[idIdx]])

  quantiles <- c(.50, .75, .95)
  lbsCDF <- (1 - quantiles) / 2
  sds <- qnorm(lbsCDF) * -1

  lb1 <- means + sds[1] * vars
  ub1 <- means - sds[1] * vars

  lb2 <- means + sds[2] * vars
  ub2 <- means - sds[2] * vars

  lb3 <- means + sds[3] * vars
  ub3 <- means - sds[3] * vars

  shapes <- as.factor(c(3, 8))
  alphas <- as.factor(c(1, 2, 3))
  names(x$trueVals[[idIdx]]) <- NULL
  toPlot <- data.frame(mypreds = x$preds[[idIdx]], theMeans = means, lb1 = lb1, ub1 = ub1, ub2 = ub2, lb2 = lb2, ub3 = ub3, lb3 = lb3, trueV = x$trueVals[[idIdx]])
  thePlot <- ggplot(toPlot, aes(x = .data[[names(toPlot)[1]]], y = .data$"theMeans")) +
    geom_ribbon(aes(ymax = .data$ub3, ymin = .data$lb3, alpha = alphas[1]), fill = "black") +
    geom_ribbon(aes(ymax = .data$ub2, ymin = .data$lb2, alpha = alphas[2]), fill = "black") +
    geom_ribbon(aes(ymax = .data$ub1, ymin = .data$lb1, alpha = alphas[3]), fill = "black") +
    geom_line(linewidth = 1, aes(color = "black")) +
    geom_line(aes(y = .data$ub3)) +
    geom_line(aes(y = .data$lb3)) +
    geom_point(aes(y = .data$trueV, shape = shapes[1]))

  ## plot training data when present
  if (x$type[idIdx] == "conditional") {
    toPlot2 <- data.frame(mypreds = x$train[[idIdx]]$preds[, 1], trueV = x$train[[idIdx]]$DV[, 1])
    thePlot <- thePlot + geom_point(data = toPlot2, aes(x = .data$mypreds, y = .data$trueV, shape = shapes[2]))
  }

  ## legend
  thePlot <- thePlot + scale_color_manual(name = "Predictive Mean", values = c("black"), labels = c("Mean"))
  thePlot <- thePlot + scale_alpha_manual(values = c(.05, .15, .3), labels = c("95%$", "75%", "50%"), guide = "legend", name = "Credibility Intervals")
  thePlot <- thePlot + scale_shape_manual(name = "Data", values = c(8, 3), labels = c("Test Data", "Training Data"))



  ## aesthetics
  thePlot <- thePlot + ggthemes::theme_tufte() + xlab(paste0("Predictor ", names(toPlot)[1])) + ylab(paste0("Outcome ", x$DV))
  thePlot
}
