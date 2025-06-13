#' Summarizing GLMMRR fits for fixed-effect models
#'
#' @param object
#' an object of class RRglm.
#' @param p1p2.digits
#' number of digits for aggregating data based on the level of protection (default: 2).
#' @param ...
#' further arguments passed to or from other methods.
#'
#' @return
#' An object of class summary.RRglm. Extends the class \code{summary.glm} with Randomize Response data.
#' @method summary RRglm
#' @export
summary.RRglm <- function(object, p1p2.digits = 2, ...)
{
  # Grab summary.glm output
  output.glm <- summary.glm(object, ...)

  items <- levels(as.factor(object$Item))
  data.all <- data.frame("Response" = object$y, "Item" = object$Item, "RRmodel" = object$RRmodel, "p1" = object$RRp1,
                         "p2" = object$RRp2, "c" = object$RRc, "d" = object$RRd, "residuals" = na.omit(output.glm$deviance.resid))

  dataPerItem <- list()
  prevalence <- data.frame("Item" = NULL, "RRmodel" = NULL, "level" = NULL, "estimate" = NULL, "se" = NULL, "n" = NULL, "c" = NULL, "d" = NULL, "observed" = NULL, "model.level" = NULL)
  prevalence.weighted <- data.frame("Item" = NULL, "RRmodel" = NULL, "estimate.weighted" = NULL, "se.weighted" = NULL, "n" = NULL)
  for(aa in 1:length(items))
  {
    dataPerItem[[aa]] <- list()

    # Create a dataframe of RR parameters and glm data to work with
    dataPerItem[[aa]]$data <- data.all[data.all$Item == items[aa], ]

    # Combine p1 and p2 into a single column
    dataPerItem[[aa]]$data$level <- paste("(", format(dataPerItem[[aa]]$data$p1, digits = p1p2.digits), " | ", format(dataPerItem[[aa]]$data$p2, digits = p1p2.digits), ")", sep = "")

    # Combine RR model and level of protection into a single column
    dataPerItem[[aa]]$data$model.level <- paste(dataPerItem[[aa]]$data$RRmodel, dataPerItem[[aa]]$data$level, sep = " ")

    # Subset data per RR model and per level of protection (p1 and p2)
    rrmodels <- levels(as.factor(dataPerItem[[aa]]$data$RRmodel))
    dataPerItem[[aa]]$dataPerModel <- list() # data per RR model
    dataPerItem[[aa]]$p1p2PerModel <- list() # levels of protection per RR model
    dataPerItem[[aa]]$dataPerLevel <- list() # data per level

    for(ii in 1:length(rrmodels))
    {
      dataPerItem[[aa]]$dataPerModel[[ii]] <- dataPerItem[[aa]]$data[dataPerItem[[aa]]$data$RRmodel == rrmodels[ii], ]
      this.levels <- dataPerItem[[aa]]$p1p2PerModel[[ii]] <- levels(as.factor(dataPerItem[[aa]]$dataPerModel[[ii]]$level))

      for(jj in 1:length(this.levels))
      {
        this.data <- dataPerItem[[aa]]$dataPerLevel[[length(dataPerItem[[aa]]$dataPerLevel) + 1]] <- dataPerItem[[aa]]$dataPerModel[[ii]][dataPerItem[[aa]]$dataPerModel[[ii]]$level == this.levels[[jj]], ]

        tmp.prev <- getMLPrevalence(mu = mean(this.data$Response), n = nrow(this.data), c = this.data[1, "c"], d = this.data[1, "d"])
        prevalence <- rbind.data.frame(prevalence, data.frame("Item" = items[aa], "RRmodel" = rrmodels[ii], "level" = this.levels[jj], "estimate" = tmp.prev$'ML estimate', "se" = sqrt(tmp.prev$'variance'),
                                                              "n" = nrow(this.data), "c" = this.data[1, "c"], "d" = this.data[1, "d"], "observed" = mean(this.data$Response), "model.level" = this.data[1, "model.level"]))
      }
    }

    # Weighted prevalence
    this.prevalence <- prevalence[which(prevalence$Item == items[aa]),]
    sum.n <- with(this.prevalence, aggregate(list(n=n), list(RRmodel = RRmodel), sum))
    weighted.estimate <- as.vector(by(this.prevalence[c("estimate", "n")], list(this.prevalence$RRmodel), function(x) { do.call(weighted.mean, unname(x)) } ))
    weighted.c <- as.vector(by(this.prevalence[c("c", "n")], list(this.prevalence$RRmodel), function(x) { do.call(weighted.mean, unname(x)) } ))
    weighted.d <- as.vector(by(this.prevalence[c("d", "n")], list(this.prevalence$RRmodel), function(x) { do.call(weighted.mean, unname(x)) } ))
    weighted.mu <- as.vector(by(this.prevalence[c("observed", "n")], list(this.prevalence$RRmodel), function(x) { do.call(weighted.mean, unname(x)) } ))
    weighted.se <- sqrt(getMLPrevalence(mu = weighted.mu, n = sum.n$n, c = weighted.c, d = weighted.d)$variance)
    prevalence.weighted <- rbind.data.frame(prevalence.weighted, data.frame("Item" = items[aa], "RRmodel" = sum.n[1], "estimate.weighted" = weighted.estimate, "se.weighted" = weighted.se, "n" = sum.n[2]))

  }

  output.glm$dataPerItem <- dataPerItem
  output.glm$prevalence <- prevalence
  output.glm$prevalence.weighted <- prevalence.weighted
  output.glm$RRlink <- object$RRlink

  class(output.glm) <- c("summary.RRglm", "summary.glm")

  return(output.glm)
}

#' Summarizing GLMMRR fits for fixed-effect models
#'
#' @param object
#' an object of class RRglm.
#' @param p1p2.digits
#' number of digits for aggregating data based on the level of protection (default: 2).
#' @param ...
#' further arguments passed to or from other methods.
#'
#' @return
#' An object of class summary.RRglmerMod. Extends the class \code{summary.glmerMod} with Randomize Response data.
#' @method summary RRglmerMod
#' @export
summary.RRglmerMod <- function(object, p1p2.digits = 2, ...)
{
  # Grab summary.merMod output
  tmp <- summary(as(object, "glmerMod"), ...)
  output.merMod <- as.list(tmp)

  items <- levels(as.factor(object@RRparam$Item))
  data.all <- data.frame("Response" = object@resp$y, "Item" = object@RRparam$Item, "RRmodel" = object@RRparam$RRmodel, "p1" = object@RRparam$p1,
                         "p2" = object@RRparam$p2, "c" = object@RRparam$c, "d" = object@RRparam$d, "residuals" = na.omit(residuals(tmp, type = "deviance")))

  dataPerItem <- list()
  prevalence <- data.frame("Item" = NULL, "RRmodel" = NULL, "level" = NULL, "estimate" = NULL, "se" = NULL, "n" = NULL, "c" = NULL, "d" = NULL, "observed" = NULL, "model.level" = NULL)
  prevalence.weighted <- data.frame("Item" = NULL, "RRmodel" = NULL, "estimate.weighted" = NULL, "se.weighted" = NULL, "n" = NULL)
  for(aa in 1:length(items))
  {
    dataPerItem[[aa]] <- list()

    # Create a dataframe of RR parameters and glm data to work with
    dataPerItem[[aa]]$data <- data.all[data.all$Item == items[aa], ]

    # Combine p1 and p2 into a single column
    dataPerItem[[aa]]$data$level <- paste("(", format(dataPerItem[[aa]]$data$p1, digits = p1p2.digits), " | ", format(dataPerItem[[aa]]$data$p2, digits = p1p2.digits), ")", sep = "")

    # Combine RR model and level of protection into a single column
    dataPerItem[[aa]]$data$model.level <- paste(dataPerItem[[aa]]$data$RRmodel, dataPerItem[[aa]]$data$level, sep = " ")

    # Subset data per RR model and per level of protection (p1 and p2)
    rrmodels <- levels(as.factor(dataPerItem[[aa]]$data$RRmodel))
    dataPerItem[[aa]]$dataPerModel <- list() # data per RR model
    dataPerItem[[aa]]$p1p2PerModel <- list() # levels of protection per RR model
    dataPerItem[[aa]]$dataPerLevel <- list() # data per level

    for(ii in 1:length(rrmodels))
    {
      dataPerItem[[aa]]$dataPerModel[[ii]] <- dataPerItem[[aa]]$data[dataPerItem[[aa]]$data$RRmodel == rrmodels[ii], ]
      this.levels <- dataPerItem[[aa]]$p1p2PerModel[[ii]] <- levels(as.factor(dataPerItem[[aa]]$dataPerModel[[ii]]$level))

      for(jj in 1:length(this.levels))
      {
        this.data <- dataPerItem[[aa]]$dataPerLevel[[length(dataPerItem[[aa]]$dataPerLevel) + 1]] <- dataPerItem[[aa]]$dataPerModel[[ii]][dataPerItem[[aa]]$dataPerModel[[ii]]$level == this.levels[[jj]], ]

        tmp.prev <- getMLPrevalence(mu = mean(this.data$Response), n = nrow(this.data), c = this.data[1, "c"], d = this.data[1, "d"])
        prevalence <- rbind.data.frame(prevalence, data.frame("Item" = items[aa], "RRmodel" = rrmodels[ii], "level" = this.levels[jj], "estimate" = tmp.prev$'ML estimate', "se" = sqrt(tmp.prev$'variance'),
                                                              "n" = nrow(this.data), "c" = this.data[1, "c"], "d" = this.data[1, "d"], "observed" = mean(this.data$Response), "model.level" = this.data[1, "model.level"]))
      }
    }

    # Weighted prevalence
    this.prevalence <- prevalence[which(prevalence$Item == items[aa]),]
    sum.n <- with(this.prevalence, aggregate(list(n=n), list(RRmodel = RRmodel), sum))
    weighted.estimate <- as.vector(by(this.prevalence[c("estimate", "n")], list(this.prevalence$RRmodel), function(x) { do.call(weighted.mean, unname(x)) } ))
    weighted.c <- as.vector(by(this.prevalence[c("c", "n")], list(this.prevalence$RRmodel), function(x) { do.call(weighted.mean, unname(x)) } ))
    weighted.d <- as.vector(by(this.prevalence[c("d", "n")], list(this.prevalence$RRmodel), function(x) { do.call(weighted.mean, unname(x)) } ))
    weighted.mu <- as.vector(by(this.prevalence[c("observed", "n")], list(this.prevalence$RRmodel), function(x) { do.call(weighted.mean, unname(x)) } ))
    weighted.se <- sqrt(getMLPrevalence(mu = weighted.mu, n = sum.n$n, c = weighted.c, d = weighted.d)$variance)
    prevalence.weighted <- rbind.data.frame(prevalence.weighted, data.frame("Item" = items[aa], "RRmodel" = sum.n[1], "estimate.weighted" = weighted.estimate, "se.weighted" = weighted.se, "n" = sum.n[2]))

  }

  output.merMod$dataPerItem <- dataPerItem
  output.merMod$prevalence <- prevalence
  output.merMod$prevalence.weighted <- prevalence.weighted
  output.merMod$glmerMod <- tmp
  output.merMod$RRlink <- object@RRparam$RRlink

  class(output.merMod) <- c("summary.RRglmerMod", "summary.glmerMod")

  return(output.merMod)
}
