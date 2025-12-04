
# S3 methods for mcmcOutput objects

# Helper function to build the summary matrix, not exported

summarise <- function(x, median, mode, CRItype, CRImass, Rhat, MCEpc, n.eff,
    overlap0, f, ...) {

  nChains <- attr(x, "nChains")
  ndraws <- nrow(x)
  draws.per.chain <- ndraws / nChains

  summary <- data.frame(
    "mean" = colMeans(x),
    "sd" = apply(x, 2, sd))
  if(median)
    summary <- cbind(summary, "median" = apply(x, 2, median))
  if(mode)
    summary <- cbind(summary, "mode" = apply(x, 2, getMode))
  if(!is.na(CRImass) && CRItype != "none") {
    if(CRItype == "hdi") {
      CRI <- t(apply(x, 2, HDInterval::hdi, credMass=CRImass))
      colnames(CRI) <- paste0(c("l", "u"), round(CRImass * 100))
    } else {
      tail <- (1 - CRImass)/2
      CRI <- t(apply(x, 2, quantile, probs=c(tail, 1-tail), na.rm=TRUE))
      colnames(CRI) <- sprintf("p%04.1f", c(tail, 1-tail)*100)
    }
    summary <- cbind(summary, CRI)
  }
  if(Rhat & nChains > 1 & draws.per.chain > 100)
    summary <- cbind(summary, "Rhat" = simpleRhat(x))
  if(MCEpc & draws.per.chain > 100)
    summary <- cbind(summary, "MCEpc" = getMCEpc(x))
  if(n.eff & ndraws > 100)
    summary <- cbind(summary, "n.eff" = round(safeNeff(x)))
  if(overlap0 && exists("CRI"))#!is.na(CRImass) )  # this is numeric, 0/1
    summary <- cbind(summary, "overlap0" = apply(CRI, 1, function(x) prod(x) < 0)*1)
  if(f) {
    f0 <- apply(x, 2, function(x) mean(x > 0))
    summary <- cbind(summary, "f" = ifelse(colMeans(x) > 0, f0, 1-f0))
  }
  return(summary)
}

