#' GWAS with SLOPE
#'
#' Performs GWAS with SLOPE on given snp matrix and phenotype.
#' At first clumping procedure is performed. Highly correlated
#' (that is stronger than parameter \emph{rho}) snps are clustered.
#' Then SLOPE is used on snp matrix which contains
#' one representative for each clump.
#'
#' @export
#' @param clumpingResult clumpProcedure output
#' @param fdr, numeric, False Discovery Rate for SLOPE
#' @param type method for snp selection. slope (default value) is SLOPE
#' on clump representatives, smt is Benjamini-Hochberg procedure on
#' single marker test p-values for clump representatives
#' @param lambda lambda for SLOPE. See \code{\link{create_lambda}}
#' @param sigma numeric, sigma for SLOPE
#' @param verbose logical, if TRUE progress bar is printed
#' @return object of class \code{\link{selectionResult}}
#'
#' @examples
#' \dontrun{
#' slope.result <- select_snps(clumping.result, fdr=0.1)
#' }
select_snps <- function(clumpingResult, fdr = 0.1, type = c("slope", "smt"),
                        lambda = "gaussian", sigma = NULL, verbose = TRUE){
  if(fdr>=1 | fdr <= 0){
    stop("FDR has to be within range (0,1)")
  }
  if(length(clumpingResult$y) != nrow(clumpingResult$X)){
    stop("Length of y must match
         number of rows in X")
  }

  if(any(type=="slope")){
    lambda <- create_lambda(length(clumpingResult$y),
                                   clumpingResult$numberOfSnps, fdr, "gaussian")
    lambda <- lambda[1:ncol(clumpingResult$X)]
    lambda_diffs <- diff(lambda)
    if(any(lambda_diffs==0) & which.min(lambda_diffs==0)==1){
      warning("All lambdas are equal. SLOPE does not guarantee
            False Discovery Rate control")
    }
    # lambdas from the previous SLOPE implementation have to be divided by the sqrt(n)
    # as LASSO scaling of the loss function is used
    lambda <- lambda / sqrt(nrow(clumpingResult$X))
    
    if (is.null(sigma) && (nrow(clumpingResult$X) >= ncol(clumpingResult$X) + 30)) {
      selected = NULL
      repeat {
        selected.prev = selected
        sigma = c(sigma, estimate_noise(clumpingResult$X[, selected], clumpingResult$y))
        result = SLOPE::SLOPE(x = clumpingResult$X, y = clumpingResult$y,
                              q = fdr, lambda = lambda, alpha = tail(sigma, 1))
        # TODO: Remove this workaround once SLOPE 1.0.0 is released and coefficients is a list
        if (is.list(result$coefficients)) {
          selected = which(as.vector(result$coefficients[[1]] != 0))
        } else {
          selected = which(result$nonzeros)
        }
        if (identical(selected, selected.prev))
          break
      }
      sigma = tail(sigma, 1)
    }

    slopeResult <- SLOPE::SLOPE(x = clumpingResult$X, y = clumpingResult$y,
                                q = fdr, lambda = lambda, alpha = sigma)
    # TODO: Remove this workaround once SLOPE 1.0.0 is released and coefficients is a list
    if (is.list(result$coefficients)) {
      selected = which(as.vector(slopeResult$coefficients[[1]] != 0))
    } else {
      selected = which(slopeResult$nonzeros)
    }
    selectedSNPs <- unlist(clumpingResult$SNPnumber)[selected]
    selectedSNPs <- sort(selectedSNPs)
  } else if(type=="smt"){
    repPVals = clumpingResult$pVals[clumpingResult$selectedSnpsNumbers]
    numbClumps = length(clumpingResult$SNPclumps)
    rejected <- which.min(repPVals<1:numbClumps/clumpingResult$numberOfSnps)-1
    selectedSNPs <- unlist(clumpingResult$SNPnumber)[1:rejected]
    selected = 1:rejected
    selectedSNPs <- sort(selectedSNPs)
  }


  X_selected <- clumpingResult$X_all[,selectedSNPs, drop = FALSE]
  if(length(selectedSNPs)==0) {
    lm.fit.summary <- summary(lm(clumpingResult$y~1))
  } else {
    # refitting linear model
    lm.fit.summary <- summary(lm(clumpingResult$y~scale(X_selected)))
  }

  result <- structure(
    list( X = X_selected,
          effects = lm.fit.summary$coefficients[-1,1],
          R2 = lm.fit.summary$r.squared,
          selectedSNPs = selectedSNPs,
          selectedClumps = clumpingResult$SNPclumps[selected],
          lambda = lambda,
          y = clumpingResult$y,
          clumpRepresentatives = clumpingResult$SNPnumber,
          clumps = clumpingResult$SNPclumps,
          X_info = clumpingResult$X_info,
          X_clumps = clumpingResult$X,
          X_all = clumpingResult$X_all,
          selectedSnpsNumbers = clumpingResult$selectedSnpsNumbersScreening[selectedSNPs],
          clumpingRepresentativesNumbers = clumpingResult$selectedSnpsNumbers,
          screenedSNPsNumbers = clumpingResult$selectedSnpsNumbersScreening,
          numberOfSnps = clumpingResult$numberOfSnps,
          pValMax = clumpingResult$pValMax,
          fdr = fdr),
    class="selectionResult")
  return(result)
}

