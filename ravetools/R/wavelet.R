#' @title 'Morlet' wavelet transform (Discrete)
#' @name wavelet
#' @description Transform analog voltage signals with 'Morlet'
#' wavelets: complex wavelet kernels with \eqn{\pi/2} phase
#' differences.
#' @param data numerical vector such as analog voltage signals
#' @param freqs frequency in which \code{data} will be projected on
#' @param srate sample rate, number of time points per second
#' @param wave_num desired number of cycles in wavelet kernels to
#' balance the precision in time and amplitude (control the
#' smoothness); positive integers are strongly suggested
#' @param precision the precision of computation; choices are
#' \code{'float'} (default) and \code{'double'}.
#' @param trend choices are \code{'constant'}: center the signal at zero;
#' \code{'linear'}: remove the linear trend; \code{'none'} do nothing
#' @param ... further passed to \code{\link{detrend}};
#' @returns \code{wavelet_kernels} returns wavelet kernels to be
#' used for wavelet function; \code{morlet_wavelet} returns a file-based array
#' if \code{precision} is \code{'float'}, or a list of real and imaginary
#' arrays if \code{precision} is \code{'double'}
#' @param frequency_range frequency range to calculate, default is 2 to 200
#' @param cycle_range number of cycles corresponding to \code{frequency_range}.
#' For default frequency range (2 - 200), the default \code{cycle_range} is
#' 3 to 20. That is, 3 wavelet kernel cycles at 2 Hertz, and 20 cycles at 200
#' Hertz.
#' @param signature signature to calculate kernel path to save, internally used
#'
#' @examples
#'
#' if(interactive()){
#'
#' # generate sine waves
#' time <- seq(0, 3, by = 0.01)
#' x <- sin(time * 20*pi) + exp(-time^2) * cos(time * 10*pi)
#'
#' plot(time, x, type = 'l')
#'
#' # freq from 1 - 15 Hz; wavelet using float precision
#' freq <- seq(1, 15, 0.2)
#' coef <- morlet_wavelet(x, freq, 100, c(2,3))
#'
#' # to get coefficients in complex number from 1-10 time points
#' coef[1:10, ]
#'
#' # power
#' power <- Mod(coef[])^2
#'
#' # Power peaks at 5Hz and 10Hz at early stages
#' # After 1.0 second, 5Hz component fade away
#' image(power, x = time, y = freq, ylab = "frequency")
#'
#' # wavelet using double precision
#' coef2 <- morlet_wavelet(x, freq, 100, c(2,3), precision = "double")
#' power2 <- (coef2$real[])^2 + (coef2$imag[])^2
#'
#' image(power2, x = time, y = freq, ylab = "frequency")
#'
#' # The maximum relative change of power with different precisions
#' max(abs(power/power2 - 1))
#'
#' # display kernels
#' freq <- seq(1, 15, 1)
#' kern <- wavelet_kernels(freq, 100, c(2,3))
#' print(kern)
#'
#' plot(kern)
#'
#' }
#'
NULL

#' @rdname wavelet
#' @export
wavelet_kernels <- function(freqs, srate, wave_num){
  # calculate wavelet cycles for each frequencies
  if(length(wave_num) != length(freqs)){
    # calculate wavelet cycles for each frequencies
    ratio <- (log(max(wave_num)) - log(min(wave_num))) / (log(max(freqs)) - log(min(freqs)))
    wavelet_cycles <- round(exp((log(freqs) - log(min(freqs))) * ratio + log(min(wave_num))))
  }else{
    wavelet_cycles <- wave_num
  }
  f_l <- length(freqs)


  # wavelet window calc - each columns of final wave is a wavelet kernel (after fft)
  # sts = wavelet_cycles / (2 * pi * freqs)
  # wavelet_wins = cbind(-3 * sts, 3 * sts)

  fft_waves <- lapply(seq_len(f_l), function(ii){
    fq <- freqs[ii]
    cycles <- wavelet_cycles[ii]
    # standard error
    st <- cycles / (2 * pi * fq)

    # calculate window size
    wavelet_win <- seq(-3 * st, 3 * st, by = 1/srate)

    # half of window length
    w_l_half <- (length(wavelet_win) - 1) / 2

    # wavelet 1: calc sinus in complex domain
    tmp_sine <- exp((0+1i) * 2 * pi * fq / srate * (-w_l_half:w_l_half))

    # Gaussian normalization part
    A <- 1/sqrt(st*sqrt(pi))

    # wavelet 2: calc gaussian wrappers
    tmp_gaus_win <- A * exp(-wavelet_win^2/(2 * (cycles/(2 * pi * fq))^2))

    # wave kernel
    tmp_wavelet <- tmp_sine * tmp_gaus_win

    tmp_wavelet
  })

  structure(list(
    kernels = fft_waves,
    wavelet_cycles = wavelet_cycles,
    sample_rate = srate,
    frequencies = freqs
  ), class = "ravetools-wavelet-kernels")
}

#' @export
`print.ravetools-wavelet-kernels` <- function(x, plot = FALSE, ...){
  cat("Discrete wavelet kernels\n")
  cat("  number of kernels/frequencies:", length(x$kernels), "\n")
  cat(sprintf("  frequency range: %.2f Hz - %.2f Hz\n", min(x$frequencies), max(x$frequencies)))
  cat(sprintf("  number of cycles: %.2f - %.2f\n", min(x$wavelet_cycles), max(x$wavelet_cycles)))
  invisible(x)
}

#' @export
`plot.ravetools-wavelet-kernels` <- function(
  x, cex = 1.2, cex.lab = cex * 1.2, cex.main = cex * 1.33,
  cex.axis = cex, mai = c(0.8,0.5,0.4,0.1), ...){

  fft_waves <- x$kernels
  srate <- x$sample_rate
  freqs <- x$frequencies
  wavelet_cycles <- x$wavelet_cycles
  max_l <- as.integer(max(sapply(fft_waves, length)) + 0.1 * srate)
  s <- sapply(fft_waves, function(s){
    l <- (max_l - length(s))
    pre <- floor(l / 2)
    post <- ceiling(l / 2)
    c(rep(NA, pre), s, rep(NA, post))
  })
  s_re <- Re(s)
  s_im <- Im(s)
  ind <- exp(seq(log(min(freqs)), log(max(freqs)), length.out = 10))
  ind <- sapply(ind, function(i){
    which.min(abs(i - freqs))[[1]]
  })
  ind <- unique(sort(ind))
  gap <- seq_along(ind) * 1.8 * max(abs(s_re), na.rm = TRUE)
  tmp_re <- t(t(s_re[, ind]) + gap)
  tmp_im <- t(t(s_im[, ind]) + gap)
  tmp <- rbind(tmp_re)
  x_all <- seq_len(max_l) / srate
  x_re <- x_all
  x_im <- x_re + max(x_all)
  # grid::grid.newpage()
  lay <- rbind(c(1,1), c(2,3))

  graphics::layout(mat = lay)

  old_mai <- graphics::par('mai')
  graphics::par(mai = mai)
  on.exit({
    graphics::par(mai = old_mai)
  }, add = TRUE)

  # old_mar <- par('mar')
  # on.exit({
  #   par(mar = old_mar)
  # })
  # par(mar = c(5.1, 4.1, 4.1, 2.1) * (cex / 2 + 0.5))
  graphics::matplot(y = tmp_re, x = x_re, type='l', col = 'red',
                    xlim = c(0, max(x_im)), ylim = c(min(tmp_re, na.rm = TRUE), max(gap) + 1.5 * min(gap)),
                    lty = 1, cex.lab = cex.lab, cex.main = cex.main, xlab = 'Wavelet Length (seconds)', cex.axis = cex.axis,
                    ylab = 'Frequency (Hz)', main = 'Wavelet Kernels (Real & Imaginary)', yaxt="n", xaxt="n")

  graphics::matlines(y = tmp_im, x = x_im, type='l', col = 'red', lty = 1)

  n_halftickers <- 7
  x_actual <- c(x_re, x_im)
  x_label <- c(x_all, x_all) - mean(x_all)
  xind <- seq(1, length(x_re), length.out = n_halftickers)
  xind <- c(xind, xind + length(x_re))
  xind <- as.integer(xind[-n_halftickers])
  x_label <- x_label[xind]
  x_label[n_halftickers] <- abs(x_label[n_halftickers])
  x_label <- sprintf('%.2f', x_label)
  x_label[n_halftickers] <- paste0('\u00B1', x_label[n_halftickers])
  x_text <- stats::median(x_actual)

  graphics::axis(1, at=x_actual[xind], x_label, cex.axis = cex.axis)
  graphics::axis(2, at=gap, freqs[ind], cex.axis = cex.axis, las = 1)
  graphics::abline(h = gap, col = 'grey80', lty = 2)
  leading_mod <- sapply(ind, function(ii){
    x <- s[,ii]
    cycles <- wavelet_cycles[ii]
    x <- x[!is.na(x)] #Mod(x[1]) / max(Mod(x)) * 100  #= 1.111%
    c(length(x) / srate, cycles)
  })
  graphics::text(x = x_text, y = gap, '|', cex = cex)
  graphics::text(x = x_text, y = gap, sprintf('%.3f', leading_mod[1,]), cex = cex, pos = 2)
  graphics::text(x = x_text, y = gap, sprintf('%.2f', leading_mod[2,]), cex = cex, pos = 4)
  y_mini_title <- min(gap) + max(gap)
  graphics::text(x = x_text, y = y_mini_title, '|', cex = cex.lab)
  graphics::text(x = x_text, y = y_mini_title, 'Wave Length', cex = cex.lab, pos = 2)
  graphics::text(x = x_text, y = y_mini_title, '# of Cycles', cex = cex.lab, pos = 4)

  # plot freq over wavelength and wave cycles
  wave_len <- sapply(fft_waves, length) / srate
  graphics::plot(freqs, wave_len, type = 'l', ylab = 'Wavelet Length (seconds)',
                 xlab = 'Frequency (Hz)', main = 'Wavelet Length | Frequency',
                 las = 1, cex.lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis, col = 'grey80')
  graphics::points(freqs, wave_len, col = 'red', pch = 4)

  graphics::plot(freqs, wavelet_cycles, type = 'l', ylab = 'Wavelet Cycle',
                 xlab = 'Frequency (Hz)', main = 'Wavelet Cycle | Frequency',
                 las = 1, cex.lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis, col = 'grey80')
  graphics::points(freqs, wavelet_cycles, col = 'red', pch = 4)

  return(invisible())
}


wavelet_kernels2_float <- function(freqs, srate, wave_num,
                             data_length, signature = NULL){
  freqs <- as.double(freqs)
  srate <- as.double(srate)
  wave_num <- as.double(wave_num)
  data_length <- as.integer(data_length)
  kernel_info <- wavelet_kernels(freqs = freqs, srate = srate, wave_num = wave_num)
  digest <- digest::digest(list(freqs, srate, wave_num, data_length, signature = signature))
  root_dir <- file.path(tempdir2(check = TRUE), "ravetools")
  if(!dir.exists(root_dir)){
    dir.create(root_dir, showWarnings = FALSE, recursive = TRUE)
  }
  path <- file.path(root_dir, sprintf("wavelet-%s", digest))


  arr_dim <- c(data_length, length(kernel_info$kernels))
  tryCatch({
    return(filearray::filearray_checkload(
      filebase = path, mode = "readonly", symlink_ok = FALSE,
      freqs = freqs, srate = srate, wave_num = wave_num, data_length = data_length,
      arr_dim = arr_dim, rave_data_type = "rave-wavelet-kernels-float"
    ))
  }, error = function(e){

    if(getOption("ravetools.debug", FALSE)){
      print(path)
      warning(e)
    }

    if(dir.exists(path)){
      unlink(path, recursive = TRUE)
    }

  })

  arr <- filearray::filearray_create(
    filebase = path,
    dimension = arr_dim,
    type = "complex",
    partition_size = 1
  )
  arr$.mode <- "readwrite"

  tmp <- complex(data_length)
  lapply(seq_along(kernel_info$kernels), function(ii){
    tmp_wavelet <- kernel_info$kernels[[ii]]
    w_l <- length(tmp_wavelet)
    n_pre  <- ceiling(data_length / 2) - floor(w_l/2)
    n_post <- data_length - n_pre - w_l
    x <- c(rep(0, n_pre), tmp_wavelet, rep(0, n_post))
    # arr[,ii] <- Conj(fftwtools::fftw_c2c(x))
    fftw_c2c(data = x, inverse = 0L, ret = tmp)
    conjugate(tmp)
    arr[,ii] <- tmp
    NULL
  })


  arr$.header$freqs <- freqs
  arr$.header$srate <- srate
  arr$.header$wave_num <- wave_num
  arr$.header$data_length <- data_length
  arr$.header$arr_dim <- arr_dim
  arr$.header$freqs <- freqs
  arr$.header$rave_data_type <- "rave-wavelet-kernels-float"
  arr$.save_header()

  arr$.mode <- "readonly"

  return(arr)
}

morlet_wavelet_float <- function(data, freqs, srate, wave_num,
                           trend = c("constant", "linear", "none"),
                           signature = NULL, ...){

  # Instead of using fixed wave_cycles, use flex cycles
  # lower num_cycle is good for low freq, higher num_cycle is good for high freq.
  # wavelet_cycles = wave_num;
  # lowest_freq = freqs[1];
  trend <- match.arg(trend)
  freqs <- as.double(freqs)
  srate <- as.double(srate)
  wave_num <- as.double(wave_num)
  more_args <- list(...)
  data_digest <- digest::digest(data)

  f_l <- length(freqs)
  d_l <- length(data)

  # calculate kernel, transform and store
  fft_waves <- wavelet_kernels2_float(freqs, srate, wave_num, d_l, signature = signature)

  # normalize data, and fft
  if(trend != "none"){
    data <- as.vector(detrend(data, trend = trend, ...))
    # data <- data - mean(data)
  #   fft_data <- fftw_r2c(data, inplace = TRUE)
  # } else {
  #   fft_data <- fftw_r2c(data, inplace = FALSE)
  }
  fft_data <- fftw_r2c(data)

  # Convolution Notice that since we don't pad zeros to data
  # d_l is nrows of wave_spectrum. However, if wave_spectrum is changed
  # we should not use d_l anymore. instead, use nrow(fft_waves)
  wave_len <- nrow(fft_waves)
  ind <- seq_len(ceiling(wave_len / 2))


  out_path <- tempfile2()
  output <- tryCatch({
    filearray::filearray_checkload(
      filebase = out_path, symlink_ok = FALSE,
      freqs = freqs, srate = srate, wave_num = wave_num,
      data_digest = data_digest, trend = trend,
      more_args = more_args,
      rave_data_type = "rave-wavelet-coefficients",
      precision = "float"
    )
  }, error = function(e){
    if(getOption("ravetools.debug", FALSE)){
      print(out_path)
      warning(e)
    }

    if(dir.exists(out_path)){
      unlink(out_path, recursive = TRUE)
    }
    NULL
  })
  if(inherits(output, "FileArray")){ return(output) }

  output <- filearray::filearray_create(
    filebase = out_path, dimension = dim(fft_waves),
    type = "complex", partition_size = 1
  )
  output$.mode <- "readwrite"

  tmp <- complex(length(fft_data))
  filearray::fmap(x = fft_waves, fun = function(input){
    # wave_spectrum = fftwtools::fftw_c2c(input[[1]] * fft_data, inverse = 1) / (wave_len * sqrt(srate / 2))
    fftw_c2c(data = input[[1]] * fft_data,
             inverse = 1L, ret = tmp)
    c(tmp[-ind], tmp[ind])  / (wave_len * sqrt(srate / 2))
  }, .y = output, .buffer_count = ncol(output))

  # output <- apply(fft_waves[], 2, function(x){
  #   wave_spectrum = fftwtools::fftw_c2c(x * fft_data, inverse = 1) / (wave_len * sqrt(srate / 2))
  #   c(wave_spectrum[-ind], wave_spectrum[ind])
  # })

  output$.header$freqs <- freqs
  output$.header$srate <- srate
  output$.header$wave_num <- wave_num
  output$.header$data_digest <- data_digest
  output$.header$trend <- trend
  output$.header$more_args <- more_args
  output$.header$rave_data_type <- "rave-wavelet-coefficients"
  output$.header$precision <- 'float'
  output$.save_header()

  output$.mode <- "readonly"
  output
}

wavelet_kernels2_double <- function(freqs, srate, wave_num, data_length, signature = NULL){
  freqs <- as.double(freqs)
  srate <- as.double(srate)
  wave_num <- as.double(wave_num)
  data_length <- as.integer(data_length)
  kernel_info <- wavelet_kernels(freqs = freqs, srate = srate, wave_num = wave_num)
  digest <- digest::digest(list(freqs, srate, wave_num, data_length, signature = signature))
  root_dir <- file.path(tempdir2(check = TRUE), "ravetools")
  if(!dir.exists(root_dir)){
    dir.create(root_dir, showWarnings = FALSE, recursive = TRUE)
  }
  real_path <- file.path(root_dir, sprintf("wavelet-real-%s", digest))
  imag_path <- file.path(root_dir, sprintf("wavelet-imag-%s", digest))


  arr_dim <- c(data_length, length(kernel_info$kernels))
  tryCatch({
    return(list(
      real = filearray::filearray_checkload(
        filebase = real_path, mode = "readonly", symlink_ok = FALSE,
        freqs = freqs, srate = srate, wave_num = wave_num, data_length = data_length,
        arr_dim = arr_dim, rave_data_type = "rave-wavelet-kernels-double-real"
      ),
      imag = filearray::filearray_checkload(
        filebase = imag_path, mode = "readonly", symlink_ok = FALSE,
        freqs = freqs, srate = srate, wave_num = wave_num, data_length = data_length,
        arr_dim = arr_dim, rave_data_type = "rave-wavelet-kernels-double-imag"
      )
    ))
  }, error = function(e){
    if(getOption("ravetools.debug", FALSE)){
      print(real_path)
      print(imag_path)
      warning(e)
    }

    if(dir.exists(real_path)){ unlink(real_path, recursive = TRUE) }
    if(dir.exists(imag_path)){ unlink(imag_path, recursive = TRUE) }

  })

  arr_real <- filearray::filearray_create(
    filebase = real_path,
    dimension = arr_dim,
    type = "double",
    partition_size = 1
  )
  arr_imag <- filearray::filearray_create(
    filebase = imag_path,
    dimension = arr_dim,
    type = "double",
    partition_size = 1
  )
  arr_real$.mode <- "readwrite"
  arr_imag$.mode <- "readwrite"

  tmp <- complex(data_length)
  lapply(seq_along(kernel_info$kernels), function(ii){
    tmp_wavelet <- kernel_info$kernels[[ii]]
    w_l <- length(tmp_wavelet)
    n_pre  <- ceiling(data_length / 2) - floor(w_l/2)
    n_post <- data_length - n_pre - w_l
    x <- c(rep(0, n_pre), tmp_wavelet, rep(0, n_post))
    # arr[,ii] <- Conj(fftwtools::fftw_c2c(x))
    fftw_c2c(data = x, inverse = 0L, ret = tmp)
    conjugate(tmp)
    arr_real[,ii] <- Re(tmp)
    arr_imag[,ii] <- Im(tmp)
    # arr[,ii] <- tmp
    NULL
  })

  arr_real$.header$freqs <- freqs
  arr_real$.header$srate <- srate
  arr_real$.header$wave_num <- wave_num
  arr_real$.header$data_length <- data_length
  arr_real$.header$arr_dim <- arr_dim
  arr_real$.header$freqs <- freqs
  arr_real$.header$rave_data_type <- "rave-wavelet-kernels-double-real"
  arr_real$.save_header()

  arr_imag$.header$freqs <- freqs
  arr_imag$.header$srate <- srate
  arr_imag$.header$wave_num <- wave_num
  arr_imag$.header$data_length <- data_length
  arr_imag$.header$arr_dim <- arr_dim
  arr_imag$.header$freqs <- freqs
  arr_imag$.header$rave_data_type <- "rave-wavelet-kernels-double-imag"
  arr_imag$.save_header()


  arr_real$.mode <- "readonly"
  arr_imag$.mode <- "readonly"

  return(list(
    real = arr_real,
    imag = arr_imag
  ))
}


morlet_wavelet_double <- function(data, freqs, srate, wave_num,
                                 trend = c("constant", "linear", "none"),
                                 signature = NULL, ...){

  # Instead of using fixed wave_cycles, use flex cycles
  # lower num_cycle is good for low freq, higher num_cycle is good for high freq.
  # wavelet_cycles = wave_num;
  # lowest_freq = freqs[1];
  trend <- match.arg(trend)
  freqs <- as.double(freqs)
  srate <- as.double(srate)
  wave_num <- as.double(wave_num)
  more_args <- list(...)
  data_digest <- digest::digest(data)

  f_l <- length(freqs)
  d_l <- length(data)

  # calculate kernel, transform and store
  fft_waves <- wavelet_kernels2_double(freqs, srate, wave_num, d_l, signature = signature)

  # normalize data, and fft
  if(trend != "none"){
    data <- as.vector(detrend(data, trend = trend, ...))
    # data <- data - mean(data)
    # fft_data <- fftw_r2c(data, inplace = TRUE)
  # } else {
    # fft_data <- fftw_r2c(data, inplace = FALSE)
  }
  fft_data <- fftw_r2c(data)

  # Convolution Notice that since we don't pad zeros to data
  # d_l is nrows of wave_spectrum. However, if wave_spectrum is changed
  # we should not use d_l anymore. instead, use nrow(fft_waves)
  wave_len <- nrow(fft_waves$real)
  ind <- seq_len(ceiling(wave_len / 2))


  real_path <- tempfile2(pattern = "wavelet-double-real-")
  imag_path <- tempfile2(pattern = "wavelet-double-imag-")
  output <- tryCatch({
    list(
      real = filearray::filearray_checkload(
        filebase = real_path, symlink_ok = FALSE,
        freqs = freqs, srate = srate, wave_num = wave_num,
        data_digest = data_digest, trend = trend,
        more_args = more_args,
        rave_data_type = "rave-wavelet-coefficients-real",
        precision = "double"
      ),
      imag = filearray::filearray_checkload(
        filebase = imag_path, symlink_ok = FALSE,
        freqs = freqs, srate = srate, wave_num = wave_num,
        data_digest = data_digest, trend = trend,
        more_args = more_args,
        rave_data_type = "rave-wavelet-coefficients-imag",
        precision = "double"
      )
    )
  }, error = function(e){
    if(getOption("ravetools.debug", FALSE)){
      print(real_path)
      print(imag_path)
      warning(e)
    }

    if(dir.exists(real_path)){ unlink(real_path, recursive = TRUE) }
    if(dir.exists(imag_path)){ unlink(imag_path, recursive = TRUE) }
    NULL
  })
  if(length(output) == 2){ return(output) }

  output_real <- filearray::filearray_create(
    filebase = real_path, dimension = dim(fft_waves[[1]]),
    type = "double", partition_size = 1
  )
  output_imag <- filearray::filearray_create(
    filebase = imag_path, dimension = dim(fft_waves[[1]]),
    type = "double", partition_size = 1
  )
  output_real$.mode <- "readwrite"
  output_imag$.mode <- "readwrite"

  tmp <- complex(length(fft_data))

  ii <- 1
  filearray::fmap2(x = fft_waves, fun = function(input){
    # wave_spectrum = fftwtools::fftw_c2c(input[[1]] * fft_data, inverse = 1) / (wave_len * sqrt(srate / 2))
    kernel <- input[[1]] + 1i * input[[2]]
    fftw_c2c(data = kernel * fft_data,
             inverse = 1L, ret = tmp)
    tmp <- c(tmp[-ind], tmp[ind])  / (wave_len * sqrt(srate / 2))
    output_real[,ii] <- Re(tmp)
    output_imag[,ii] <- Im(tmp)
    ii <<- ii + 1
    NULL
  }, .buffer_count = f_l)

  # output <- apply(fft_waves[], 2, function(x){
  #   wave_spectrum = fftwtools::fftw_c2c(x * fft_data, inverse = 1) / (wave_len * sqrt(srate / 2))
  #   c(wave_spectrum[-ind], wave_spectrum[ind])
  # })

  output_real$.header$freqs <- freqs
  output_real$.header$srate <- srate
  output_real$.header$wave_num <- wave_num
  output_real$.header$data_digest <- data_digest
  output_real$.header$trend <- trend
  output_real$.header$more_args <- more_args
  output_real$.header$rave_data_type <- "rave-wavelet-coefficients-real"
  output_real$.header$precision <- 'double'
  output_real$.save_header()

  output_imag$.header$freqs <- freqs
  output_imag$.header$srate <- srate
  output_imag$.header$wave_num <- wave_num
  output_imag$.header$data_digest <- data_digest
  output_imag$.header$trend <- trend
  output_imag$.header$more_args <- more_args
  output_imag$.header$rave_data_type <- "rave-wavelet-coefficients-imag"
  output_imag$.header$precision <- 'double'
  output_imag$.save_header()

  output_real$.mode <- "readonly"
  output_imag$.mode <- "readonly"
  list(
    real = output_real,
    imag = output_imag
  )
}

#' @rdname wavelet
#' @export
morlet_wavelet <- function(data, freqs, srate, wave_num, precision = c("float", "double"),
                           trend = c("constant", "linear", "none"),
                           signature = NULL, ...) {
  precision <- match.arg(precision)
  if(precision == "float"){
    re <- morlet_wavelet_float(data = data, freqs = freqs, srate = srate,
                               wave_num = wave_num, trend = trend,
                               signature = signature, ...)
  } else {
    re <- morlet_wavelet_double(data = data, freqs = freqs, srate = srate,
                               wave_num = wave_num, trend = trend,
                               signature = signature, ...)
  }
  return(re)
}

#' @rdname wavelet
#' @export
wavelet_cycles_suggest <- function(
    freqs,
    frequency_range = c(2, 200),
    cycle_range = c(3, 20)
) {
  v1 <- log(cycle_range[[1]])
  v2 <- log(cycle_range[[2]])
  cycle <- (v2 - v1) / (log(frequency_range[[2]]) - log(frequency_range[[1]])) *
    (log(freqs) - log(frequency_range[[1]])) + v1
  cycle <- round(exp(cycle))
  cycle[cycle <= 0] <- 1

  data.frame(
    Frequency = freqs,
    Cycles = cycle
  )
}


# x <- rnorm(10000)
# y2 <- morlet_wavelet(x, freqs = 2:200, srate = 2000, wave_num = c(2,20), demean = TRUE)
# y1 <- rave::wavelet(x, freqs = 2:200, srate = 2000, wave_num = c(2,20), demean = TRUE)

