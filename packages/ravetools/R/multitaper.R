#' @title Compute 'multitaper' spectral densities of time-series data
#' @name multitaper
#'
#' @param data numerical vector, signal traces
#' @param data_length length of data
#' @param fs sampling frequency in 'Hz'
#' @param frequency_range frequency range to look at; length of two
#' @param time_bandwidth a number indicating time-half bandwidth product; i.e.
#' the window duration times the half bandwidth of main lobe; default is
#' \code{5}
#' @param num_tapers number of 'DPSS' tapers to use; default is \code{NULL} and
#' will be automatically computed from \code{floor(2*time_bandwidth - 1)}
#' @param window_params vector of two numbers; the first number is the
#' window size in seconds; the second number if the step size; default is
#' \code{c(5, 1)}
#' @param detrend_opt how you want to remove the trend from data window; options
#' are \code{'linear'} (default), \code{'constant'}, and \code{'off'}
#' @param nfft 'NFFT' size, positive; see 'Details'
#' @returns \code{multitaper_config} returns a list of configuration parameters
#' for the filters; \code{multitaper} also returns the time, frequency and
#' corresponding spectral power.
#'
#' @details The original source code comes from 'Prerau' Lab (see 'Github'
#' repository \code{'multitaper_toolbox'} under user \code{'preraulab'}).
#' The results tend to agree with their 'Python' implementation with precision
#' on the order of at \code{1E-7} with standard deviation at most \code{1E-5}.
#' The original copy was licensed under a Creative Commons Attribution
#' 'NC'-'SA' 4.0 International License
#' (\url{https://creativecommons.org/licenses/by-nc-sa/4.0/}).
#'
#' This package (\code{'ravetools'}) redistributes the \code{multitaper}
#' function under minor modifications on \code{nfft}. In the original copy
#' there is no parameter to control the exact numbers of \code{nfft}, and
#' the \code{nfft} is always the power of 2. While choosing
#' \code{nfft} to be the power of 2 is always recommended, the modified code
#' allows other choices.
#'
#' @examples
#'
#' if(interactive()) {
#'
#' time <- seq(0, 3, by = 0.001)
#' x <- sin(time * 20*pi) + exp(-time^2) * cos(time * 10*pi)
#'
#' res <- multitaper(
#'   x, 1000, frequency_range = c(0,15),
#'   time_bandwidth=1.5,
#'   window_params=c(2,0.01)
#' )
#'
#'
#' image(
#'   x = res$time,
#'   y = res$frequency,
#'   z = 10 * log10(res$spec),
#'   xlab = "Time (s)",
#'   ylab = 'Frequency (Hz)',
#'   col = matlab_palette()
#' )
#'
#' }
#'
NULL

multitaper_process_input <- function(
  len_data, fs, frequency_range=NULL, time_bandwidth=5,
  num_tapers=NULL, window_params=c(5,1), nfft=NA,
  detrend_opt='linear'){


  # Set frequency range if not provided
  if(is.null(frequency_range)){
    frequency_range <- c(0, fs/2)
  }

  # Set detrend method
  detrend_opt <- tolower(detrend_opt)
  if(detrend_opt != 'linear'){
    if(detrend_opt == 'const'){
      detrend_opt <- 'constant'
    } else if(detrend_opt == 'none' || detrend_opt == 'false'){
      detrend_opt <- 'off'
    }else{
      stop(paste("'", toString(detrend_opt),
                 "' is not a valid detrend_opt argument. The",
                 " choices are: 'constant', 'linear', or 'off'.",
                 sep=""))
    }
  }

  # Check if frequency range is valid
  if(frequency_range[2] > fs/2){
    frequency_range[2] <- fs/2
    warning(paste(
      "Upper frequency range greater than Nyquist, setting range to [",
      toString(frequency_range[1]), ",",
      toString(frequency_range[2]), "].",
      sep=""))
  }

  # Set number of tapers if none provided
  optimal_num_tapers <- floor(2*time_bandwidth) - 1
  if(is.null(num_tapers)){
    num_tapers <- optimal_num_tapers
  }

  # Warn if number of tapers is suboptimal
  if(num_tapers != optimal_num_tapers){
    warning(paste("Suboptimal number of tapers being used. Number of tapers is optimal at floor(2*TW) - 1 which is ",
                  toString(optimal_num_tapers), " in this case.", sep=""))
  }


  # Check if window size is valid, fix if not
  if((window_params[1]*fs) %% 1 != 0){
    winsize_samples <- round(window_params[1]*fs)
    warning(paste("Window size is not divisible by sampling frequency. Adjusting window",
                  " size to ", toString(winsize_samples/fs), " seconds.", sep=""))
  } else{
    winsize_samples <- window_params[1]*fs
  }

  # Check if window step size is valid, fix if not
  if((window_params[2]*fs) %% 1 != 0){
    winstep_samples <- round(window_params[2]*fs)
    warning(paste("Window step size is not divisible by sampling frequency. Adjusting window",
                  " step size to ", toString(winstep_samples/fs), " seconds.", sep=""))
  } else{
    winstep_samples <- window_params[2]*fs
  }


  # Check if length of data is smaller than window (bad)
  if(len_data < winsize_samples){
    stop(paste("Data length (", toString(len_data), ") is shorter than the window size (",
               toString(winsize_samples), "). Either increase data length or decrease",
               " window size.", sep=""))
  }

  # Find window start indices and num of windows
  window_start <- seq(1, len_data-winsize_samples+1, by=winstep_samples)
  num_windows <- length(window_start)

  # Get num points in FFT
  nfft <- as.integer(nfft)
  if(!is.na(nfft) && nfft < 1){
    stop("Invalid nfft")
  }
  if(is.na(nfft)){
    nfft <- max(2^ceiling(log2(abs(winsize_samples))), winsize_samples)
  }


  return(
    list(
      fs = fs,
      frequency_range = frequency_range,
      time_bandwidth = time_bandwidth,
      num_tapers = num_tapers,
      winsize_samples = winsize_samples,
      winstep_samples = winstep_samples,
      window_start = window_start,
      num_windows = num_windows,
      nfft = nfft,
      detrend_opt = detrend_opt
    )
  )
}

multitaper_process_spectrogram_params <- function(
  fs, nfft, frequency_range, window_start, datawin_size
){

  # Create frequency vector
  df <- fs/nfft
  sfreqs <- seq(df/2, fs-(df/2), by=df)

  # Get frequencies for given frequency range
  freq_inds <- (sfreqs >= frequency_range[1]) & (sfreqs <= frequency_range[2])
  sfreqs <- sfreqs[freq_inds]

  # Compute times in middle of each spectrum
  window_middle_times <- window_start + round(datawin_size/2)
  stimes <- window_middle_times / fs

  # Get indices for each window
  window_idxs <- lapply(window_start, function(start){
    seq(start, start+datawin_size-1, by=1)
  }) # list of indices for n windows


  return(list(
    window_idxs = window_idxs,
    stimes = stimes,
    sfreqs = sfreqs,
    freq_inds = freq_inds
  ))

}

#' @rdname multitaper
#' @export
multitaper_config <- function(
  data_length, fs, frequency_range=NULL, time_bandwidth=5,
  num_tapers=NULL, window_params=c(5,1),
  nfft=NA, detrend_opt='linear'
) {
  res <- multitaper_process_input(
    data_length, fs, frequency_range, time_bandwidth, num_tapers,
    window_params, nfft, detrend_opt)

  # Set up spectrogram parameters
  res2 <- with(res, {
    multitaper_process_spectrogram_params(fs, nfft, frequency_range, window_start, winsize_samples)
  })

  structure(c(res, res2), class = "ravetools-multitaper-config")

}

#' @export
`print.ravetools-multitaper-config` <- function(x, ...){
  # display_spectrogram_properties(fs, time_bandwidth, num_tapers, c(winsize_samples, winstep_samples), frequency_range,
  #                                detrend_opt)
  data_window_params <- c(x$winsize_samples, x$winstep_samples)
  data_window_params <- data_window_params / x$fs

  # Print spectrogram properties
  cat(
    sep = "",
    "Multitaper Spectrogram Configuration: \n",

    '     Spectral Resolution: ',
    toString(2 * x$time_bandwidth / data_window_params[1]), 'Hz\n',

    '     Window Length: ',
    toString(data_window_params[1]), "s\n",


    '     Window Step: ',
    toString(data_window_params[2]), "s\n",

    '     Time Half-Bandwidth Product: ',
    toString(x$time_bandwidth), "\n",

    '     Number of Tapers: ',
    toString(x$num_tapers), "\n",

    '     Frequency Range: ',
    toString(x$frequency_range[1]), "-",
    toString(x$frequency_range[2]), 'Hz\n',

    '     Detrend: ', x$detrend_opt, "\n"
  )

}



multitaper_calc_mts_segment <- function(data_segment, dpss_tapers, nfft, freq_inds, detrend_opt){

  # If segment has all zeros, return vector of zeros
  if(all(data_segment==0)){
    if(is.logical(freq_inds)){
      ret <- rep(0, sum(freq_inds))
    } else {
      ret <- rep(0, length(freq_inds))
    }

    return(ret)
  }

  # Optionally detrend data to remove low freq DC component
  if(detrend_opt != 'off'){
    data_segment <- detrend(data_segment, trend = detrend_opt)
  }

  # Multiply data by dpss tapers (STEP 2)
  if(is.matrix(dpss_tapers)){
    tapered_data <- sweep(dpss_tapers, 1, data_segment, '*',
                          check.margin = FALSE)
  } else {
    tapered_data <- matrix(data_segment * dpss_tapers, ncol = 1L)
  }


  # Manually add nfft zero-padding (R's fft function does not support)
  npad <- nfft-nrow(tapered_data)
  # npad_left <- ceiling(npad / 2)
  # npad_right <- npad - npad_left
  tapered_padded_data <- rbind(
    # array(0, c(
    #   npad_left,
    #   ncol(tapered_data)
    # )),
    tapered_data,
    array(0, c(
      npad,
      ncol(tapered_data)
    ))
  )

  # Compute the FFT (STEP 3)
  # fft_data <- apply(tapered_padded_data, 2, stats::fft)
  # fft_range = fft_data[freq_inds,,drop = FALSE]

  # nr1 <- nrow(tapered_padded_data)
  fft_data <- mvfftw_r2c(tapered_padded_data)
  # nr2 <- nrow(fft_data)
  #
  # if( nr1 %% 2 == 0 ){
  #   fft_data <- rbind(fft_data, Conj(
  #     fft_data[nr2 - seq_len(nr1 - nr2), ,drop=FALSE]
  #   ))
  # } else {
  #   fft_data <- rbind(fft_data, Conj(
  #     fft_data[(nr2 + 1) - seq_len(nr1 - nr2), ,drop=FALSE]
  #   ))
  # }
  if(is.logical(freq_inds)){
    freq_inds <- which(freq_inds)
  }
  fft_range <- fft_data[freq_inds,,drop = FALSE]

  # Take the FFT magnitude (STEP 4.1)
  magnitude <- Im(fft_range)^2 + Re(fft_range)^2
  mt_spectrum <- rowSums(magnitude)

  return(mt_spectrum)
}

#' @rdname multitaper
#' @export
multitaper <- function(
  data, fs, frequency_range=NULL, time_bandwidth=5,
  num_tapers=NULL, window_params=c(5,1),
  nfft = NA, detrend_opt='linear'
){

  # Make sure data is 1D atomic vector
  if((is.atomic(data) == FALSE) || is.list(data)){
    stop("data must be a 1D atomic vector")
  }

  dlen <- length(data)

  conf <- multitaper_config(
    dlen, fs, frequency_range, time_bandwidth, num_tapers,
    window_params, nfft, detrend_opt
  )

  window_idxs <- conf$window_idxs
  winsize_samples <- conf$winsize_samples
  freq_inds <- conf$freq_inds
  stimes <- conf$stimes
  sfreqs <- conf$sfreqs
  list2env(conf, envir = environment())


  # Split data into window segments
  split_data_helper <- function(indices, data){ # for sapply when splitting data into windows
    data_seg <- data[indices]
    return(data_seg)
  }
  data_segments <- t(sapply(window_idxs, split_data_helper, data=data))


  # COMPUTE THE MULTITAPER SPECTROGRAM
  #     STEP 1: Compute DPSS tapers based on desired spectral properties
  #     STEP 2: Multiply the data segment by the DPSS Tapers
  #     STEP 3: Compute the spectrum for each tapered segment
  #     STEP 4: Take the mean of the tapered spectra

  # Compute DPSS tapers (STEP 1)
  dpss_tapers <- waveslim::dpss.taper(winsize_samples, num_tapers, time_bandwidth) * sqrt(fs)

  # Compute multitaper
  mt_spectrogram <- apply(
    data_segments, 1, multitaper_calc_mts_segment,
    dpss_tapers=dpss_tapers, nfft=nfft,
    freq_inds=freq_inds, detrend_opt=detrend_opt)

  # Compute mean fft magnitude (STEP 4.2)
  mt_spectrogram <- Conj(t(mt_spectrogram)) / fs^2 / num_tapers


  # if(all(as.vector(mt_spectrogram) == 0)){
  #   print("Spectrogram calculated as all zeros, no plot shown")
  # }else if(plot_on){
    # fields::image.plot(x=conf$time, y=conf$frequency, 10*log10(conf$spec), xlab="Time (s)",
    #            ylab='Frequency (Hz)')
  # }

  return(list(spec = mt_spectrogram, time = stimes, frequency = sfreqs, configuration = conf))
}
