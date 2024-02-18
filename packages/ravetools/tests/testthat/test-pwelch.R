test_that("pwelch", {
  rave_pwelch <- function (x, fs, window = 64, noverlap = 8, nfft = 256, col = "black",
                           xlim = NULL, ylim = NULL, main = "Welch periodogram", plot = TRUE,
                           log = "xy", spec_func = stats::spectrum, cex = 1, ...)
  {
    x <- as.vector(x)
    x_len <- length(x)
    nfft <- max(min(nfft, length(x)), window)
    window <- hanning(window)
    window_len <- length(window)
    step <- max(floor(window_len - noverlap + 0.99), 1)
    offset <- seq(1, x_len - window_len + 1, by = step)
    N <- length(offset)
    re <- sapply(seq_len(N), function(i) {
      a <- detrend_naive(x[offset[i] - 1 + seq_len(window_len)])
      a <- fftwtools::fftw_r2c(postpad(a$Y * window, nfft))
      Mod(a)^2
    })
    NN <- floor((nfft + 1)/2)
    freq <- seq(0, fs/2, length.out = NN)
    spec <- rowMeans(re[seq_len(NN), , drop = F])/(window_len/2)^2
    res <- list(freq = freq, spec = spec, method = "Welch")
    return(invisible(res))
  }


  x <- rnorm(10000)
  a <- pwelch(x, 100, plot = FALSE)
  b <- rave_pwelch(x, 100, log = '')

  expect_equal(a$freq, b$freq)
  expect_equal(a$spec, b$spec)

  # if(FALSE){
  #   x <- rnorm(600000)
  #   microbenchmark::microbenchmark(
  #     rave = {
  #       rave_pwelch(x, 2000, log = '')
  #     },
  #     ravetools = {
  #       pwelch(x, 2000, plot = FALSE)
  #     }, times = 20
  #   )
  # }

})
