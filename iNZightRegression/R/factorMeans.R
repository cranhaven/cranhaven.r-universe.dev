factorMeans = function(fit) {
  varTypes = attr(fit$terms, "dataClasses")
  isFactor = varTypes %in% c("factor", "ordered")
  factorNames = names(varTypes)[isFactor]
  if (any(isFactor)) {
      yvar = names(varTypes[1])
      factorNames = factorNames[factorNames != yvar]
      yvals = if (isGlm(fit)) fit$y else fit$model[,yvar]
      termLabels = attr(fit$terms, "term.labels")
      interactions = termLabels[grep(":", termLabels)]
      interactingVars = strsplit(interactions, ":")
      if (any(sapply(interactingVars, length) > 2))
          stop("Interactions involving more than 2 factors are not supported")
      nonInteractions = factorNames
      for (i in seq_along(interactions))
          nonInteractions = nonInteractions[! nonInteractions %in% interactingVars[[i]]]

      res = list()
      for (i in seq_along(nonInteractions)) {
          f = nonInteractions[i]
          facvals = structure(list(fit$model[,f]), names = f)
          facmeans = aggregate(yvals, facvals, mean)
          res[[f]] = structure(facmeans$x, names = levels(facmeans[,f]))
      }
      for (i in seq_along(interactions)) {
          f = interactingVars[[i]]
          facvals = structure(as.list(fit$model[,f]), names = f)
          facmeans = aggregate(yvals, facvals, mean)
          meanNames = paste(facmeans[,f[1]], gsub(" ", "", facmeans[,f[2]]), sep = ".")
          res[[interactions[i]]] = structure(facmeans$x, names = meanNames)
      }
      return(res)
  }
  else warning('There are no factors in this model.')
}



## Adjusted means for the levels of a factor: Sets values of continuous
## variables equal to their means and sets other factors equal to their
## baseline values

adjustedMeans = function(fit) {
  varTypes = attr(fit$terms, "dataClasses")[-1]
  isFactor = varTypes %in% c("factor", "ordered")
  if (any(isFactor)) {
      factorNames = names(varTypes)[isFactor]
      numericNames = names(varTypes)[!isFactor]
      numericMeans = sapply(fit$model[,numericNames, drop = FALSE], mean)

      termLabels = attr(fit$terms, "term.labels")
      interactions = termLabels[grep(":", termLabels)]
      interactingVars = strsplit(interactions, ":")
      if (any(sapply(interactingVars, length) > 2))
          stop("Interactions involving more than 2 factors are not supported")
      nonInteractions = factorNames
      for (i in seq_along(interactions))
          nonInteractions =
              nonInteractions[! nonInteractions %in% interactingVars[[i]]]

      res = list()
      for (i in seq_along(nonInteractions)) {
          f = nonInteractions[i]
          facLevels = levels(fit$model[,f])
          xVars = names(varTypes)[names(varTypes) != f]
          xVarsList = list()
          for (j in seq_along(xVars)) {
              if (xVars[j] %in% factorNames) {
                  val = factor(levels(fit$model[,xVars[j]])[1])
                  levels(val) <- levels(fit$model[, xVars[[j]]])
              } else {
                  val = numericMeans[xVars[j]]
              }
              xVarsList[[xVars[j]]] = val
          }
          adjMean = structure(numeric(length(facLevels)), names = facLevels)
          for (flev in facLevels) {
              xVarsList[[f]] = factor(flev, levels = facLevels)
              adjMean[flev] = predict(fit, data.frame(xVarsList, stringsAsFactors = TRUE))
          }
          res[[f]] = adjMean
      }

      for (i in seq_along(interactions)) {
          f = interactingVars[[i]]
          facCombinations = expand.grid(levels(fit$model[,f[1]]),
              levels(fit$model[,f[2]]))
          nCombinations = nrow(facCombinations)
          combinationNames = paste(facCombinations[,1], facCombinations[,2],
              sep = ".")
          xVars = names(varTypes)[!names(varTypes) %in% f]
          xVarsList = list()
          for (i in seq_along(xVars)) {
              val = ifelse(xVars[i] %in% factorNames,
                  levels(fit$model[,xVars[i]])[1],
                  numericMeans[xVars[i]])
              xVarsList[[xVars[i]]] = val
          }

          adjMean = structure(numeric(nCombinations), names = combinationNames)
          for (i in 1:nCombinations) {
              for (j in 1:2)
                  xVarsList[[f[j]]] = facCombinations[i, j]
              adjMean[combinationNames[i]] = predict(fit, data.frame(xVarsList, stringsAsFactors = TRUE))
          }
          ord = order(names(adjMean))
          adjMean = adjMean[names(adjMean)[ord]]

          res[[paste(f, collapse = ":")]] = adjMean
      }

      ## If GLM, then inverse-link the results
      res <- if (isGlm(fit)) lapply(res, fit$family$linkinv) else res
      return(res)
  }
  else {
      warning('There are no factors in this model.')
  }
}

#' Compare factor levels
#'
#' Computes confidence intervals for the pairwise differences between levels
#' of a factor, based off of \code{stats::TukeyHSD}.
#'
#' @param fit a lm/glm/svyglm object
#' @param factor the name of the factor to compare
#' @return a factor level comparison object with estimates, CIs, and (adjusted) p-values
#' @author Tom Elliott
#' @export
#' @examples
#' f <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
#' factorComp(f, "Species")
factorComp <- function(fit, factor) {
    z <- sprintf("multcomp::mcp(%s = \"Tukey\")", factor)
    comp <- summary(multcomp::glht(fit, linfct = eval(parse(text = z))))
    ci <- confint(comp)$confint

    levels <- fit$xlevels[[factor]]
    n <- length(levels)
    diff <- comp$test$coefficients
    lower <- ci[, "lwr"]
    upper <- ci[, "upr"]
    pval <- comp$test$pvalues

    pvalmat <- diffmat <-
        matrix(nrow = n - 1, ncol = n - 1)
    cimat <-
        matrix(NA, nrow = 2 * (n - 1), ncol = n - 1)

    k <- 0
    for (i in 1:(n-1)) {
        for (j in i:(n-1)) {
            k <- k + 1
            diffmat[i, j] <- diff[k]
            pvalmat[i, j] <- pval[k]
            cimat[i*2 - 1, j] <- lower[k]
            cimat[i*2, j] <- upper[k]
        }
    }
    colnames(diffmat) <- colnames(cimat) <- colnames(pvalmat) <- levels[-1]
    rownames(diffmat) <- rownames(pvalmat) <- levels[-n]
    rownames(cimat) <- rbind(levels[-n], "")

    out <- structure(
        list(estimate = diffmat, ci = cimat, p.value = pvalmat),
        class = "inzfactorcomp",
        response = colnames(fit$model)[1],
        factor = factor
    )
    if (inherits(fit, "svyglm"))
        out$reg.term.test <- survey::regTermTest(fit, factor)
    out
}

#' @param x an `inzfactorcomp` object
#' @param ... extra arguments for print (ignored)
#' @export
#' @describeIn factorComp print method for object of class `inzfactorcomp`
#' @md
print.inzfactorcomp <- function(x, ...) {
    if (!is.null(x$reg.term.test)) {
        cat(sprintf(
            "# Overall effect of %s on %s\n\n",
            attr(x, "factor"),
            attr(x, "response")
        ))
        cat(sprintf(
            "Null hypothesis: all coefficients associated with '%s' are zero\n",
            attr(x, "factor")
        ))
        cat(sprintf("\nF = %.02f on %i and %i df, p-value%s%s\n\n\n",
            x$reg.term.test$Ftest[1],
            x$reg.term.test$df,
            x$reg.term.test$ddf,
            ifelse(x$reg.term.test$p < 1e-8, " ", " = "),
            format.pval(x$reg.term.test$p, digits = 5, eps = 1e-8, na.form = "")
        ))
    }

    cat(sprintf(
        "# Estimated differences in mean %s between levels of %s\n",
        attr(x, "response"), attr(x, "factor")
    ))
    cat("  adjusted for the other variables in the model\n")
    cat("  (col group - row group)\n\n")

    cat("Estimates\n\n")
    print(x$estimate, na.print = "")

    cat("\n95% Confidence Intervals\n\n")
    print(x$ci, na.print = "")

    cat("\nP-values\n\n")
    pmat <- x$p.value
    pmat[1:prod(dim(pmat))] <-
      format.pval(pmat, digits = 5, eps = 1e-8, na.form = "")
    print(pmat, quote = FALSE)

    invisible(NULL)
}
