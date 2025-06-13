#' Accessing GLMMRR Fits for fixed-effect models
#'
#' Compute residuals for RRglm objects. Extends \code{\link{residuals.glm}} with residuals for grouped binary Randomized Response data.
#'
#' @param object
#' an object of class RRglm.
#' @param type
#' the type of residuals which should be returned. The alternatives are: "deviance" (default), "pearson", "working", "response", "partial",
#' "deviance.grouped", "pearson.grouped" and "hosmer-lemeshow".
#' @param ngroups
#' the number of groups if Hosmer-Lemeshow residuals are computed (default: 10).
#' @param ...
#' further arguments passed to or from other methods.
#'
#' @return
#' A vector of residuals.
#' @method residuals RRglm
#' @export
#' @seealso \code{\link{residuals.glm}}
residuals.RRglm <- function (object, type = c("deviance", "pearson", "working", "response", "partial",
                                              "deviance.grouped", "pearson.grouped", "hosmer-lemeshow"), ngroups = 10, ...)
{
  type <- match.arg(type)
  y <- object$y
  mu <- object$fitted.values
  vars <- all.vars(object$formula)

  if (type == "deviance.grouped" || type == "pearson.grouped")
  {
    # Determine possible unique groups, or clusters in the data
    df.x <- object$model[, vars[2:length(vars)]]
    factor.groups <- getUniqueGroups(df.x)

    # Expected cell proportions
    vec.pihat <- getCellMeans(y = mu, factor.groups = factor.groups)

    # Observed cell proportions
    vec.prophat <- getCellMeans(y = y, factor.groups = factor.groups)
    vec.prophat[which(vec.prophat == 0)] <- 1e-15
    vec.prophat[which(vec.prophat == 1)] <- 1-1e-15

    # Number of observations per group
    vec.groupN =  getCellSizes(n = length(y), factor.groups = factor.groups)

    if (type == "deviance.grouped")
    {
      sign <- rep(1, length(vec.prophat))
      sign[which(vec.prophat < vec.pihat)] <- -1
      res <- sign * sqrt(2 * vec.groupN * (vec.prophat * log(vec.prophat / vec.pihat) + (1 - vec.prophat) * log((1 - vec.prophat) / (1 - vec.pihat))))
    }
    else if (type == "pearson.grouped")
      res <- (vec.prophat - vec.pihat) / sqrt(vec.pihat * (1 - vec.pihat) / vec.groupN)
  }
  else if (type == "hosmer-lemeshow")
  {
    # 1: Expected probabilities
    # 2: Observed probabilities
    # 3: Index used to set cutting points for equally sized groups
    probmatrix = matrix(nrow = length(y), ncol = 2)
    probmatrix[, 1] = mu
    probmatrix[, 2] = y

    # Order by expected probabilities
    probmatrix <- probmatrix[order(probmatrix[, 1], decreasing=FALSE), ]

    # Set index
    probmatrix <- cbind(probmatrix, seq(1, length(y)))

    # Split matrix into given number of groups
    breaks <- quantile(probmatrix[, 3], probs = seq(0, 1, by = 1 / ngroups))
    probmatrix[, 3] <- cut(probmatrix[, 3], breaks = breaks, include.lowest = TRUE)
    problistSplitted <- split(as.data.frame(probmatrix), probmatrix[, 3])

    res <- numeric(ngroups)
    for(ii in 1:ngroups)
    {
      predictedProbs <- problistSplitted[[ii]][[1]]
      observedProbs <- problistSplitted[[ii]][[2]]
      nObservationsInGroup<- length(problistSplitted[[ii]][[2]])

      avgPredictedSuccesses <- (mean(predictedProbs))
      avgObservedSuccesses <- (mean(observedProbs))

      res[ii] <- (avgObservedSuccesses - avgPredictedSuccesses)/sqrt(avgPredictedSuccesses * (1 - avgPredictedSuccesses) / nObservationsInGroup)
    }
  }
  else
    res <- residuals.glm(object = object, type = type, ...)

  return(as.numeric(res))

}


#' Accessing GLMMRR Fits for mixed-effect models
#'
#' Compute residuals for RRglmer objects. Extends \code{\link{residuals.glmResp}} to access conditional and
#' unconditional residuals for grouped binary Randomized Response data.
#'
#' @param object
#' an object of class RRglmer.
#' @param type
#' the type of residuals which should be returned. The alternatives are: "deviance" (default), "pearson", "working", "response", "partial",
#' "unconditional.response" and "unconditional.pearson".
#' @param ...
#' further arguments passed to or from other methods.
#'
#' @return
#' A vector of residuals.
#' @method residuals RRglmerMod
#' @export
#' @seealso \code{\link{residuals.glmResp}}
residuals.RRglmerMod <- function(object, type = c("deviance", "pearson", "working", "response",
                                                  "partial", "unconditional.response", "unconditional.pearson"), ...)
{
  type <- match.arg(type)
  if(type == "unconditional.response" || type == "unconditional.pearson")
  {
    linkinv <- object@resp$family$linkinv
    pi <- linkinv(lme4::getME(object, "X") %*% lme4::fixef(object))
    res <- object@resp$y - pi
    if(type == "unconditional.pearson")
      res <- as.numeric(res / sqrt(pi * (1 - pi)))
  }
  else
  {
    object2 <- object
    class(object2) <- "glmerMod"
    res <- residuals(object = object2, type = type, ...)
  }

  return(as.numeric(unname(res)))
}
# wtres <- sqrtrWt*(y-mu) # type="pearson"
#sqrtrWt: the square root of the weights used to define the weighted sum of squares of the residuals, sqrt(pWt/var)
#Pearson: sqrt(out2@resp$weights/(out2@resp$mu*(1-out2@resp$mu)))*(out2@resp$y - out2@resp$mu)
