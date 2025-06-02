################################################################################
#
# Wrapper for cgam shape constrained smoothing
#
################################################################################

#' @import cgam
smooth_cgam <- function(x, y, formula, Xcov, ...){
  cgam_data <- data.frame(as.data.frame(x), Xcov)  
  p <- ncol(cgam_data)
  n <- nrow(cgam_data)
  cgam_data[attr(y, "varname")] <- y
  gfit <- cgam::cgam(formula, data = cgam_data, ...)
  # Extract estimated terms
  preds <- t(gfit$etacomp)
  if (!is.null(gfit$zmat)){ 
    preds <- cbind(preds, mapply("*", as.data.frame(gfit$zmat), gfit$zcoef[-1]))
  }
  colnames(preds) <- c(gfit$xnms, gfit$znms)
  whichinds <- pmatch(colnames(x), colnames(preds))
  gx <- preds[,c(whichinds, setdiff(seq_len(ncol(preds)), whichinds)), drop = FALSE]
  # Estimate first derivative 
  xmat <- cbind(gfit$xmat_add, gfit$zmat)
  colnames(xmat) <- c(gfit$xnms, gfit$znms)
  xmat <- xmat[,c(whichinds, setdiff(seq_len(ncol(preds)), whichinds)), drop = FALSE]
  dgx <- mapply(function(x, gx) stats::splinefun(x, gx)(x, deriv = 1), 
    as.data.frame(gfit$xmat_add), as.data.frame(t(gfit$etacomp)))
  nz <- length(gfit$zcoef[-1])
  if (nz > 0) dgx <- cbind(dgx, matrix(gfit$zcoef[-1], nrow = n, ncol = nz))
  colnames(dgx) <- c(gfit$xnms, gfit$znms)
  dgx <- dgx[,c(whichinds, setdiff(seq_len(ncol(preds)), whichinds)), drop = FALSE]
  beta0 <- gfit$coef[1]
  return(list(intercept = beta0, gz = gx, dgz = dgx, edf = sum(gfit$edf0)))
}