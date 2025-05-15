#' @rdname f0
#' 
#' @param pct.n percent of samples to use in bootstrap draws. Must be in range of \code{0:1}.
#' @param num.reps number of random re-orderings of samples to fit curve to.
#' 
#' @export
#' 
Clench <- function(f, pct.n = 0.85, num.reps = 100) {
  if(!swfscMisc::isBetween(pct.n, 0, 1, include.ends = T)) {
    stop("'pct.n' must be in 0:1")
  }
  # convert f to sample frequency distribution
  sample.freq <- species.to.sample.freq(f)
  # get maximum number of samples
  n <- sum(sample.freq)
  sample.n <- ceiling(n * pct.n)
  sample.n <- max(sample.n, 2)
  sample.n <- min(sample.n, n)
  
  # fit clench curves to bootstrap subsamples of the data 
  sample.vec <- rep(1:length(sample.freq), sample.freq)
  s.est.vec <- sapply(1:num.reps, function(i) {
    sample.order <- sample(sample.vec)
    cum.species <- sapply(1:sample.n, function(j) length(unique(sample.order[1:j])))
    df <- data.frame(num.samples = 1:sample.n, cum.species = cum.species)
    clench.nls <- stats::nls(
      cum.species ~  (a * num.samples) / (1 + (b * num.samples)), 
      data = df, 
      start = list(a = 1, b = 1), 
      control = stats::nls.control(minFactor = 1 / 16384, warnOnly = TRUE)
    )
    nls.coefs <- stats::coef(clench.nls)
    unname(floor(nls.coefs["a"] / nls.coefs["b"]))
  })
  
  s.est <- stats::median(s.est.vec)
  sd.s.est <- stats::sd(s.est.vec)
  s.obs <- length(unique(sample.vec))
  f0 <- max(s.obs, s.est) - s.obs
  c(s.est = s.obs + f0, f0 = f0, s.obs = s.obs, n = n, sd.s.est = sd.s.est)
}