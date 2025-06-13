#' Print RRglmGOF values
#'
#' @param x
#' an object of class RRglmGOF.
#' @param digits
#' minimal number of \emph{significant digits} (default: 3).
#' @param ...
#' further arguments passed to or from other methods.
#' @method print RRglmGOF
#' @export
print.RRglmGOF <- function(x, digits = 3, ...)
{
  cat("\n")
  cat("### GLMMRR - Binary Randomized Response Data ###")
  cat("\n")
  cat("Generalized linear fixed-effects model")
  cat("\n\n")
  cat("# Goodness-of-Fit Testing #\n")
  cat("---------------------------------------------------------\n")
  cat("Family:\t\t\t\t", x$family$family, "\n")
  cat("Link function:\t\t\t", x$family$link, "\n")
  cat("Response variable:\t\t", x$vars[1], "\n")
  cat("Predictor(s):\t\t\t", x$vars[2:length(x$vars)], "\n")
  cat("Entries dataset:\t\t", x$n, "\n")

  if(any(c(x$pearson$do, x$deviance$do, x$hlemeshow$do)))
  {
    # Create empty data frame
    df.output <- data.frame("Statistic" = numeric(0), "P.value" = numeric(0), "df" = numeric(0), "Groups" = numeric(0))

    if(x$pearson$do)
    {
      df.pearsonOutput <- data.frame("Statistic" = x$pearson$stat, "P.value" = x$pearson$pvalue, "df" = x$pearson$df, "Groups" = x$pearson$nGroup)
      rownames(df.pearsonOutput) <- "Pearson"
      df.output <- rbind(df.output, df.pearsonOutput)
    }

    if(x$deviance$do)
    {
      df.devianceOutput <- data.frame("Statistic" = x$deviance$stat, "P.value" = x$deviance$pvalue, "df" = x$deviance$df, "Groups" = x$deviance$nGroup)
      rownames(df.devianceOutput) <- "Deviance"
      df.output <- rbind(df.output, df.devianceOutput)
    }

    if(x$hlemeshow$do)
    {
      df.hlemeshowOutput <- data.frame("Statistic" = x$hlemeshow$stat, "P.value" = x$hlemeshow$pvalue, "df" = x$hlemeshow$df, "Groups" = x$hlemeshow$nGroup)
      rownames(df.hlemeshowOutput) <- "Hosmer-Lemeshow"
      df.output <- rbind(df.output, df.hlemeshowOutput)
    }


    cat("---------------------------------------------------------\n")
    cat("Summary: \n\n")
    print(df.output, digits = digits, right = FALSE)
    cat("\n")
    if(x$hlemeshow$do)
    {
      cat("---------------------------------------------------------\n")
      cat("Hosmer-Lemeshow: \n\n")
      print(x$hlemeshow$overview, digits = digits, right = FALSE)
      cat("\n")
    }


  }
}

#' Print RRglm summary
#'
#' @param x
#' an object of class summary.RRglm.
#' @param printPrevalence
#' print estimated population prevalence per item and RR model (default: true).
#' @param printPrevalencePerLevel
#' print estimated population prevalence per item, RRmodel and protection level (default: false).
#' @param printResiduals
#' print deviance residuals (default: false).
#' @param digits
#' minimal number of \emph{significant digits} (default: 5).
#' @param ...
#' further arguments passed to or from other methods.
#'
#' @method print summary.RRglm
#' @export
print.summary.RRglm <- function(x, printPrevalence = TRUE, printPrevalencePerLevel = FALSE, printResiduals = FALSE, digits = 5, ...)
{
  # Output is organized per item
  ls.perItemResidualOutput <- list()
  ls.perItemModels <- list()
  items <- c()
  for (ii in 1:length(x$dataPerItem))
  {
    this.item <- x$dataPerItem[[ii]]
    this.models <- ls.perItemModels[[ii]] <- c()
    items <- cbind(items, as.character(this.item$dataPerModel[[1]][1, "Item"]))
    if (printResiduals)
    {
      ls.perItemResidualOutput[[ii]] <- data.frame("Min." = NULL, "1st Qu." = NULL, "Median" = NULL, "3rd Qu." = NULL, "Max." = NULL)
    }

    for(rr in 1:length(this.item$dataPerModel))
    {
      this.models <- cbind(this.models, paste(as.character(this.item$dataPerModel[[rr]][1, "RRmodel"])))

      if (printResiduals)
      {
        # Obtain information about residuals for the model
        tmp <- summary(this.item$dataPerModel[[rr]]$residuals)
        tmp <- unclass(tmp)

        # Create a data frame for each RR model
        ls.perItemResidualOutput[[ii]] <- rbind.data.frame(ls.perItemResidualOutput[[ii]], data.frame("Min." = tmp[1], "1st Qu." = tmp[2], "Median" = tmp[3], "3rd Qu." = tmp[5], "Max." = tmp[6]))
      }
    }

    ls.perItemModels[[ii]] <- this.models

    if (printResiduals)
    {
      rownames(ls.perItemResidualOutput[[ii]]) <- ls.perItemModels[[ii]] <- this.models
      colnames(ls.perItemResidualOutput[[ii]]) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    }
  }

  # Print the output given by summary.glm()
  x.asglm <- x
  class(x.asglm) <- "summary.glm"
  print(x.asglm, ...)

  cat("\n")
  cat("### GLMMRR - Binary Randomized Response Data ###")
  cat("\n")
  cat("Generalized linear fixed-effects model")
  cat("\n\n")
  cat("Family:\t\t\t", x$family$family, "\n")
  cat("Link function:\t\t", x$family$link, "\n")

  for(ii in 1:length(items))
  {
    cat("\n")
    cat("---------------------------------------------------------")
    cat("\n")
    cat("Item:\t\t\t", items[ii], "\n")
    cat("Model(s):\t\t", ls.perItemModels[[ii]][[1]], x$dataPerItem[[ii]]$p1p2PerModel[[1]], "\n")
    if(length(ls.perItemModels[[ii]]) > 1)
    {
      for (rr in 2:length(ls.perItemModels[[ii]]))
      {
        cat("\t\t\t", ls.perItemModels[[ii]][[rr]], x$dataPerItem[[ii]]$p1p2PerModel[[rr]], "\n")
      }
    }
    if(printPrevalence)
    {
      cat("\n")
      cat("## Estimated Population Prevalence (weighted per RR model) \n")
      print(x$prevalence.weighted[which(x$prevalence.weighted$Item == items[ii]), 2:5], digits = digits, row.names = FALSE)
    }
    if(printPrevalencePerLevel)
    {
      cat("\n")
      cat("## Estimated Population Prevalence\n")
      print(x$prevalence[which(x$prevalence$Item == items[ii]), 2:6], digits = digits, row.names = FALSE)
    }
    if(printResiduals)
    {
      cat("\n")
      cat("## Deviance residuals\n")
      print(ls.perItemResidualOutput[[ii]], digits = digits, row.names = FALSE)
    }
  }
  cat("\n")

}


#' Print RRglmer summary
#'
#' @param x
#' an object of class summary.RRglmerMod.
#' @param printPrevalence
#' print estimated population prevalence per item and RR model (default: true).
#' @param printPrevalencePerLevel
#' print estimated population prevalence per item, RRmodel and protection level (default: false).
#' @param printResiduals
#' print conditional deviance residuals (default: false).
#' @param digits
#' minimal number of \emph{significant digits} (default: 5).
#' @param ...
#' further arguments passed to or from other methods.
#'
#' @method print summary.RRglmerMod
#' @export
print.summary.RRglmerMod <- function(x, printPrevalence = TRUE, printPrevalencePerLevel = FALSE, printResiduals = FALSE, digits = 5, ...)
{
  # Output is organized per item
  ls.perItemResidualOutput <- list()
  ls.perItemModels <- list()
  items <- c()
  for (ii in 1:length(x$dataPerItem))
  {
    this.item <- x$dataPerItem[[ii]]
    this.models <- ls.perItemModels[[ii]] <- c()
    items <- cbind(items, as.character(this.item$dataPerModel[[1]][1, "Item"]))
    if (printResiduals)
    {
      ls.perItemResidualOutput[[ii]] <- data.frame("Min." = NULL, "1st Qu." = NULL, "Median" = NULL, "3rd Qu." = NULL, "Max." = NULL)
    }

    for(rr in 1:length(this.item$dataPerModel))
    {
      this.models <- cbind(this.models, paste(as.character(this.item$dataPerModel[[rr]][1, "RRmodel"])))

      if (printResiduals)
      {
        # Obtain information about residuals for the model
        tmp <- summary(this.item$dataPerModel[[rr]]$residuals)
        tmp <- unclass(tmp)

        # Create a data frame for each RR model
        ls.perItemResidualOutput[[ii]] <- rbind.data.frame(ls.perItemResidualOutput[[ii]], data.frame("Min." = tmp[1], "1st Qu." = tmp[2], "Median" = tmp[3], "3rd Qu." = tmp[5], "Max." = tmp[6]))
      }
    }

    ls.perItemModels[[ii]] <- this.models

    if (printResiduals)
    {
      rownames(ls.perItemResidualOutput[[ii]]) <- ls.perItemModels[[ii]] <- this.models
      colnames(ls.perItemResidualOutput[[ii]]) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    }
  }

  # Print the output given by summary.glmerMod()
  print(x$glmerMod, ...)

  cat("\n")
  cat("### GLMMRR - Binary Randomized Response Data ###")
  cat("\n")
  cat("Generalized linear mixed-effects model")
  cat("\n\n")
  cat("Family:\t\t\t", x$family, "\n")
  cat("Link function:\t\t", x$link, "\n")

  for(ii in 1:length(items))
  {
    cat("\n")
    cat("---------------------------------------------------------")
    cat("\n")
    cat("Item:\t\t\t", items[ii], "\n")
    cat("Model(s):\t\t", ls.perItemModels[[ii]][[1]], x$dataPerItem[[ii]]$p1p2PerModel[[1]], "\n")
    if(length(ls.perItemModels[[ii]]) > 1)
    {
      for (rr in 2:length(ls.perItemModels[[ii]]))
      {
        cat("\t\t\t", ls.perItemModels[[ii]][[rr]], x$dataPerItem[[ii]]$p1p2PerModel[[rr]], "\n")
      }
    }
    if(printPrevalence)
    {
      cat("\n")
      cat("## Estimated Population Prevalence (weighted per RR model) \n")
      print(x$prevalence.weighted[which(x$prevalence.weighted$Item == items[ii]), 2:5], digits = digits, row.names = FALSE)
    }
    if(printPrevalencePerLevel)
    {
      cat("\n")
      cat("## Estimated Population Prevalence\n")
      print(x$prevalence[which(x$prevalence$Item == items[ii]), 2:6], digits = digits, row.names = FALSE)
    }
    if(printResiduals)
    {
      cat("\n")
      cat("## Conditional deviance residuals\n")
      print(ls.perItemResidualOutput[[ii]], digits = digits, row.names = FALSE)
    }
  }
  cat("\n")

}


