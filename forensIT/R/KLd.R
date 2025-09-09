#' @title KL divergence
#' @description KL divergence
#' @param ppx probability distribution
#' @param ppy probability distribution
#' @param epsilon small number to avoid log(0)
#' @param bsigma boolean to compute sigma
#' @return KL divergence
#' @export
KLd <- function(ppx, ppy, epsilon= 1e-20, bsigma = FALSE) {
  if (sum(ppx == 0) > 0) {
    ppx[ppx == 0] <- epsilon
    ppx <- ppx / sum(ppx)
  }
  if (sum(ppy == 0) > 0) {
    ppy[ppy == 0] <- epsilon
    ppy <- ppy / sum(ppy)
  }
  kl <- sum(ppx * log10(ppx / ppy))
  if (!bsigma) {
    return(c(kl))
  }else {
    skl <- sqrt(sum(ppx * (log10(ppx / ppy))^2) - kl^2)
    return(c(kl = kl, sigma_kl = skl))
  }
  if (FALSE) {
    sum(ppx * log10(ppx)) + sum(ppx * log10(1 / ppy))
    sum(ppx * log10(ppx)) + sum(ppx) * log10(1 / epsilon)
    -H(ppx)
  }
}