################################################################################
#
# Wrapper for scam shape constrained smoothing
#
################################################################################

smooth_scam <- function(x, y, formula, Xcov, ...)
{ 
  # Construct data for scam fitting
  scam_data <- data.frame(as.data.frame(x), Xcov)
  p <- ncol(scam_data)
  n <- nrow(scam_data)
  scam_data[attr(y, "varname")] <- y
  iscons <- any(sapply(scam_lookup, grepl, as.character(formula)[3]))
  if (iscons){
    gfit <- scam::scam(formula, data = scam_data, ...)
    # print("scam"); flush.console()
  } else {
    gfit <- mgcv::gam(formula, data = scam_data, ...)
    derivs <- as.data.frame(gratia::derivatives(gfit, data = scam_data))
    # print("gam"); flush.console()
  }
  # Extract estimated terms
  preds <- try(stats::predict(gfit, type = "terms", se.fit = TRUE), 
    silent = TRUE)
  if (inherits(preds, "try-error")){
    preds <- stats::predict(gfit, type = "terms", se.fit = FALSE)
    gx <- preds
    ses <- matrix(NA, nrow(gx), ncol(gx))
  } else {
    gx <- preds$fit
    ses <- preds$se.fit
  }
  colnames(gx) <- colnames(ses) <- gsub("s\\(|\\)", "", colnames(gx))
  gx <- gx[,match(colnames(scam_data)[1:p], colnames(gx)), drop = FALSE]
  # Estimate first derivative
  trms <- stats::terms(formula, specials = "s") 
  smterms <- attr(trms, "specials")$s - 1
  dgx <- matrix(0, n, p)
  for (j in 1:p){
    jind <- which(all.vars(formula)[-1] == names(scam_data)[j])
    if (jind %in% smterms){
      if (iscons){
        dgx[,j] <- scam::derivative.scam(gfit, jind)$d
      } else {
        dgx[,j] <- derivs[derivs$.smooth == gfit$smooth[[jind]]$label, 
          ".derivative"]
      }
    } else {
      if (is.numeric(scam_data[j])){
        dgx[,j] <- stats::coef(gfit)[names(scam_data)[j]]
      }
    }
  }
  beta0 <- if(attr(stats::terms(formula), "intercept")) stats::coef(gfit)[1] else 0
  return(list(intercept = beta0, gz = gx, dgz = dgx, edf = sum(gfit$edf),
    se = ses))
}
