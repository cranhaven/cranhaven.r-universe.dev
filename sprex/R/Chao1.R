#' @rdname f0
#' 
#' @export

Chao1 <- function(f) {
  x <- f.stats(f)
  s.obs <- unname(x["s.obs"])
  if(length(f) == 1) f <- c(f, 0)
  
  f0 <- if(f[2] > 0) {
    f[1] ^ 2 / (2 * f[2])
  } else {
    term.1 <- f[1] * (f[1] - 1)
    term.2 <- 2 * (f[2] + 1)
    term.1 / term.2
  }
  f0 <- f0 * (x["n"] - 1) / x["n"]
  
  # calculate bias-corrected form
  # following Gotelli, N.J. and Colwell, R.K. (2011) Estimating Species Richness. In: Biological Diversity: Frontiers in Measurement and Assessment, Oxford University Press, United Kingdom, 39-54.
  # https://www.uvm.edu/~ngotelli/manuscriptpdfs/Chapter%204.pdf
  # bc stands for 'bias corrected'
  f0.bc <- ((f[1] * (f[1] - 1)) / (2 * (f[2] + 1)))
  s.est.bc <- s.obs + f0.bc

  # calculate variance
  # following Chao A. Estimating the population size for capture-recapture data with unequal catchability. Biometrics. 1987 Dec 1:783-91.
  # https://doi.org/10.2307/2531532
  if(f[1]>0 && f[2]>0){
    variance <- f[2] * ((0.5 * ((f[1] / f[2])^2)) +
                        ((f[1] / f[2])^3) +
                        (0.25 * ((f[1] / f[2])^4)))

    # calculate the confidence intervals (p787 of Chao 1987)
    # here based on the bias corrected f0
    C <- exp(1.96 * sqrt(log(1 + (variance / (s.est.bc - s.obs)^2 ))))

    s.est.bc.upperCI <- s.obs + (f0.bc * C)
    s.est.bc.lowerCI <- s.obs + (f0.bc / C)
  } else {
    # these are undefined if either f[1] or f[2] are 0
    variance <- NA
    s.est.bc.upperCI <- NA
    s.est.bc.lowerCI <- NA
  }

  x <- c(s.est = unname(f0 + s.obs), 
         f0 = unname(f0),
         f0.bc = f0.bc,
         s.est.bc = s.est.bc,
         s.est.bc.upperCI = s.est.bc.upperCI,
         s.est.bc.lowerCI = s.est.bc.lowerCI,
         x)
  x[is.nan(x)] <- NA
  x
}
