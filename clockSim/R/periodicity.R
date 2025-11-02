# Periodicity of trajectory

#' Compute periodicity of time series
#' 
#' Supports the following methods: `fft` and `lomb`. `lomb` uses the `lomb` 
#' package for computation. Please note that `lomb` can be resource intensive 
#' and significantly slower than `fft`.
#' 
#' If `ts_time` is provided, it is passed to the Lomb-Scargle algorithm for 
#' unevenly sampled data computation. In this case, `lomb` method returns 
#' period in unit of `ts_time`. `ts_time` has no effect on the `fft` method as 
#' it requires even time spacing.
#' 
#' If `ts_time` is `NULL`, assume even time spacing and period unit will be 
#' in the (implicitly provided) time spacing of the time series vector.
#' 
#' Power, SNR, p-value, and ellipsis of the period detection are method-specific:
#' 
#' 1. `fft`: power = Spectrum power of the peak. 
#' SNR = (power of peak)/(median power). p-value is not available (NA). 
#' In this case, `...` is not used
#' 2. `lomb`: power = LS power of the peak. 
#' SNR is not available (NA). p-value = LS p-value. 
#' In this case, `...` is forwarded to `lomb::lsp()`. Note: It is assumed that 
#' the period of interest is >1. Otherwise result will be incorrect.
#' 
#'
#' @param ts_vector Numeric vector of time series
#' @param ts_time Numeric vector of time points (if `NULL` use "step" unit)
#' @param method Period calculation method.
#' @param verbose Whether to print verbose messages on, e.g., time resolution.
#' @param ... Passed to the specific method for finer control, see details.
#' 
#' @returns Named vector of length 4 (period, power, snr, p.value).
#' @export
#'
#' @examples
#' # Generate a period = 50 sine wave data with some noise (even spacing)
#' n <- 1000
#' time <- seq(1, n, by = 1)
#' ts_data <- sin(2 * pi * time / 50) + rnorm(n, sd = 0.5)
#' compute_period(ts_data)
#' compute_period(ts_data, method = "lomb")
#' # Uneven sampling of the previous data and run lomb method again
#' s <- sample(1:n, n/3)
#' compute_period(ts_data[s], time[s], method = "lomb")
compute_period <- 
  function(ts_vector, ts_time = NULL, method = "fft", verbose = FALSE, ...){
    switch(
      method,
      fft = {
        # Compute FFT and get peak
        fft_result <- stats::fft(ts_vector)
        n <- length(ts_vector)
        freq <- (1:(n-1)) / n
        fft_result <- fft_result[2:n]
        power <- Mod(fft_result[1:(n/2)])
        peak_idx <- which.max(power)
        peak_power <- power[peak_idx]
        period <- 1 / freq[peak_idx]
        # Compute SNR
        noise_floor <- stats::median(power)
        snr <- peak_power / noise_floor
        # Output verbose message
        if (verbose){
          rlang::inform(paste0(
            "FFT period grid = ", paste(1/freq, collapse = ",")))
        }
        # Return
        c(period = period, power = peak_power, snr = snr, p.value = NA)
      },
      lomb = {
        if (is.null(ts_time)) ts_time <- seq_along(ts_vector)
        lsp_result <- lomb::lsp(ts_vector, ts_time, plot = FALSE, ...)
        period <- 1 / lsp_result$peak.at[lsp_result$peak.at < 1]
        peak_power <- lsp_result$peak
        snr <- NA
        p.value <- lsp_result$p.value
        # Output verbose message
        if (verbose){
          rlang::inform(paste0(
            "LOMB scan grid = ", paste(lsp_result$scanned, collapse = ",")))
        }
        # Return
        c(period = period, power = peak_power, snr = NA, p.value = p.value)
      },
      rlang::abort("Unsupported method.")
    )
  }
