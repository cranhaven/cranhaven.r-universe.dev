pickHardThreshold_alternative <- function (data,
                                           RsquaredCut = 0.85,
                                           cutVector = seq(0.1, 0.9, by = 0.05)) {
#' @title Alternative implementation of WGCNA::pickHardThreshold
#'
#' @description (INTERNAL) Alternative implementation of \code{\link[WGCNA]{pickHardThreshold}} to
#' fit to the needs of this package. Most importantly the function was simplified to only apply to
#' the use case of finding a cut-off value to reduce a correlation matrix. The following changes
#' were applied in comparison to the original function:
#' * The function __always__ assumes similarity matrices (i.e. correlation matrices) as input
#' * Additional settings have been removed (`dataIsExpr`, `moreNetworkConcepts`, `removeFirst`,
#' `corFnc`, `corOptions`, `nBreaks`)
#' * The function uses \code{\link{scaleFreeFitIndex_alternative}} for fit index calculation
#' * Print prompts and additional metrics were removed
#' * An error message that reports the lowest R-squared computed in case this value did not satisfy
#' the \code{RsquaredCut} value was added.
#'
#' Description by \code{\link[WGCNA]{pickHardThreshold}}:Analysis of scale free topology for
#' multiple hard thresholds. The aim is to help the user pick an appropriate threshold for network
#' construction.
#'
#' @param data Similarity (correlation) matrix. With entries between 0 and 1 (i.e. absolute values
#' of correlation matrix)
#' @param RsquaredCut desired minimum scale free topology fitting index
#' @param cutVector a vector of hard threshold cuts for which the scale free topology fit indices
#' are to be calculated.
#' @export
#' @return estimate of an appropriate hard-thresholding cut: the lowest cut for which the scale
#' free topology fit exceeds \code{RsquaredCut}. If is below \code{RsquaredCut} for all cuts, an
#'error is thrown.
#' @examples
#' adjacency_matrix <- matrix(rnorm(36),nrow=6)
#' diag(adjacency_matrix) <- 1
#' RsquaredCut <- 0.001
#' cutVector <- seq(0.2, 0.8, by = 0.05)
#' \dontshow{
#' WGCNA::disableWGCNAThreads()
#' }
#' cutEstimate_coarse <- pickHardThreshold_alternative(abs(adjacency_matrix), RsquaredCut,
#' cutVector)
#' @source \code{\link[WGCNA]{pickHardThreshold}} and \code{\link[WGCNA]{scaleFreeFitIndex}}
#'
    removeFirst <- FALSE
    nBreaks <- 10

    Rsquared <- rep(NA, length(cutVector))

    fun1 <-  function(corx) {
        out1 <- rep(NA, length(cutVector))
        for (j in c(1:length(cutVector))) {
            out1[j] <- sum(corx > cutVector[j], na.rm = TRUE)
        }

        return(out1)
    }

    datk = t(apply(data, 2, fun1))
    message("Calculating Rsquared for ", length(cutVector)," cut-off values\n")
    for (i in c(1:length(cutVector))) {
        message("Computing Rsquared for cut-off ",cutVector[i], ". cut-off number ",i,"\n")
        khelp <-  datk[, i] - 1
        SFT1 <- scaleFreeFitIndex_alternative(k = khelp,
                                  nBreaks = nBreaks,
                                  removeFirst = removeFirst)
        Rsquared[i] = SFT1
    }

    ind1 <- Rsquared > RsquaredCut
    highestR2 <- max(Rsquared)
    indcut <- NA
    indcut <- if (sum(ind1) > 0) min(c(1:length(ind1))[ind1]) else indcut;
    cutEstimate = cutVector[indcut][[1]]
    if(is.na(cutEstimate)) stop(paste0("pickHardThreshold could not find a cut-off value that satisfies the specified R-squared criterion. Please try setting a lower R-squared or using a different network reduction method. Highest R-squared computed was ",round(highestR2, 3)))
    return(cutEstimate)
}



scaleFreeFitIndex_alternative <- function(k, nBreaks = 10, removeFirst = FALSE) {
    #' @title Alternative implementation of WGCNA::scaleFreeFitIndex
    #'
    #' @description (INTERNAL) This function is a copy of \code{\link[WGCNA]{scaleFreeFitIndex}}
    #' with minor changes introduced to fit the needs of this package.
    #'
    #' Description by \code{\link[WGCNA]{scaleFreeFitIndex}}: The function scaleFreeFitIndex
    #' calculates several indices (fitting statistics) for evaluating scale free topology fit. The
    #' input is a vector (of connectivities) k. Next k is discretized into nBreaks number of equal-
    #' width bins. Let's denote the resulting vector dk. The relative frequency for each bin is
    #' denoted p.dk.
    #'
    #' The entire original function code is contained in the function and deleted lines are
    #' commented out using \code{#--} to show changes.
    #'
    #' @param k numeric vector whose components contain non-negative values
    #' @param nBreaks positive integer. This determines the number of equal width bins.
    #' @param removeFirst logical. If TRUE then the first bin will be removed.
    #' @export
    #' @return the model fitting index (R.squared) from the following model lm(log.p.dk ~ log.dk)
    #' @source \code{\link[WGCNA]{scaleFreeFitIndex}}
    discretized.k = cut(k, nBreaks)
    dk = tapply(k, discretized.k, mean)
    p.dk = as.vector(tapply(k, discretized.k, length)/length(k))
    breaks1 = seq(from = min(k), to = max(k),
                  length = nBreaks + 1)
    hist1 = graphics::hist(k, breaks = breaks1, plot = FALSE, right = TRUE)
    dk2 = hist1$mids
    dk = ifelse(is.na(dk), dk2, dk)
    dk = ifelse(dk == 0, dk2, dk)
    p.dk = ifelse(is.na(p.dk), 0, p.dk)
    log.dk = as.vector(log10(dk))
    if (removeFirst) {
        p.dk = p.dk[-1]
        log.dk = log.dk[-1]
    }
    log.p.dk= as.numeric(log10(p.dk + 1e-09))
    lm1 = try(stats::lm(log.p.dk ~ log.dk));
    if (inherits(lm1, "try-error")) browser();
    #-- lm2 = lm(log.p.dk ~ log.dk + I(10^log.dk))
    #-- datout=data.frame(Rsquared.SFT = summary(lm1)$r.squared,
                      #-- slope.SFT=summary(lm1)$coefficients[2, 1],
                      #-- truncatedExponentialAdjRsquared= summary(lm2)$adj.r.squared)
    #-- datout
    return(summary(lm1)$r.squared)
}
