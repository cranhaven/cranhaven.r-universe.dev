#' @title Plot one or more signal traces in the same figure
#' @param signals numerical matrix with each row to be a signal trace and each
#' column contains the signal values at a time point
#' @param sample_rate sampling frequency
#' @param col signal color, can be vector of one or more
#' @param space vertical spacing among the traces; for values greater than 1,
#' the spacing is absolute; default is \code{0.995}; for values less equal to 1,
#' this is the percentile of the whole data. However, the quantile mode can be
#' manually turned off is \code{"absolute"} is required; see \code{space_mode}
#' @param space_mode mode of spacing, only used when \code{space} is less equal
#' to one; default is quantile
#' @param start_time the time to start drawing relative to the first column
#' @param duration duration of the signal to draw
#' @param compress whether to compress signals if the data is too large
#' @param channel_names \code{NULL} or a character vector of channel names
#' @param time_shift the actual start time of the signal. Unlike
#' \code{start_time}, this should be the actual physical time represented
#' by the first column
#' @param new_plot whether to draw a new plot; default is true
#' @param xline,yline the gap between axis and label
#' @param xlab,ylab,lwd,xlim,cex,cex.lab,mar,mgp,xaxs,yaxs,tck,... plot
#' parameters; see \code{\link[base]{plot}} and \code{\link[graphics]{par}}
#' @examples
#'
#'
#' n <- 1000
#' base_signal <- c(rep(0, n/2), sin(seq(0,10,length.out = n/2))) * 10
#' signals <- rbind(rnorm(n) + base_signal,
#'                  rbinom(n, 10, 0.3) + base_signal,
#'                  rt(n, 5) + base_signal)
#' plot_signals(signals, sample_rate = 100)
#' plot_signals(signals, sample_rate = 100, start_time = 5)
#' plot_signals(signals, sample_rate = 100,
#'              start_time = 5, time_shift = 100)
#'
#'
#' @export
plot_signals <- function (
    signals, sample_rate = 1, col = graphics::par("fg"),
    space = 0.995, space_mode = c("quantile", "absolute"),
    start_time = 0, duration = NULL,
    compress = TRUE, channel_names = NULL, time_shift = 0,
    xlab = "Time (s)", ylab = "Electrode",
    lwd = 0.5, new_plot = TRUE, xlim = NULL,
    cex = 1, cex.lab = 1,
    mar = c(3.1, 2.1, 2.1, 0.8) * (0.25 + cex * 0.75) + 0.1,
    mgp = cex * c(2, 0.5, 0),
    xaxs = "r", yaxs = "i", xline = 1.5 * cex, yline = 1 * cex,
    tck = -0.005 * (3 + cex), ...)
{
  space_mode <- match.arg(space_mode)
  if (space_mode == "quantile" && space <= 1) {
    # quantile is slow: sample
    if(length(signals) > 100000) {
      space <- stats::quantile(
        signals[sample(length(signals), 100000)],
        space, na.rm = TRUE
      ) * 2
    } else {
      space <- stats::quantile(signals, space, na.rm = TRUE) * 2
    }
  }
  # make sure spacing is positive
  space <- abs(space)
  compress <- round(compress)
  if(!is.matrix(signals)) {
    if(is.vector(signals)) {
      signals <- matrix(signals, nrow = 1)
    } else {
      signals <- as.matrix(signals)
    }
  }
  ns <- nrow(signals)
  nt <- ncol(signals)

  if (compress == 1) {
    if (length(duration)) {
      n_tp <- round(duration * sample_rate)
    } else {
      n_tp <- round(nt - start_time * sample_rate)
    }
    if (n_tp * ns > 100000) {
      compress <- (n_tp * ns/100000)
      if(n_tp / compress < 1000) {
        compress <- n_tp / 1000
      }
    }
  }

  if (compress > 1) {
    tidx <- round(seq(1, ncol(signals), by = compress))
    nt <- length(tidx)

    Time <- (tidx - 1) / sample_rate

    sample_rate <- sample_rate/compress
  } else {
    nt <- ncol(signals)
    tidx <- seq_len(nt)

    Time <- (tidx - 1) / sample_rate
  }

  start_time <- min(start_time, range(Time)[2] - 10/sample_rate)
  if (is.null(duration)) {
    time_range <- c(start_time, range(Time)[2])
  } else {
    time_range <- c(start_time, start_time + duration)
  }
  tsl <- Time >= time_range[[1]] & Time <= time_range[[2]]

  tidx <- tidx[tsl]
  Time <- Time[tsl]

  signals <- signals[, tidx, drop = FALSE]
  nt <- ncol(signals)

  if (length(col) == 1) {
    col <- rep(col, ns)
  }
  y0 <- space * seq_len(ns)
  r <- y0 + signals

  if (is.null(channel_names)) {
    channel_names <- as.character(seq_len(ns))
    if (length(y0) > 30) {
      ind <- seq(0, length(y0), by = 5)
      ind <- unique(c(1, ind[-1], length(y0)))
      diff_ind <- diff(ind)
      if (diff_ind[[length(diff_ind)]] == 1) {
        ind <- ind[-(length(ind) - 1)]
      }
      y0 <- y0[ind]
      channel_names <- channel_names[ind]
    }
  }
  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main", "cex.lab", "cex.axis", "cex.sub"))
  par_opt$cex.lab <- cex.lab

  if (new_plot) {
    graphics::par(mar = mar, mgp = mgp, cex.lab = par_opt$cex.lab)
    on.exit({graphics::par(par_opt)}, add = TRUE)

    graphics::matplot(time_shift + Time, t(r), type = "l",
                      col = col, lty = 1, lwd = lwd, frame.plot = FALSE,
                      yaxt = "n", xlab = "", ylab = "", ylim = space * c(0.5, ns + 0.5),
                      xlim = xlim, cex = cex, cex.main = par_opt$cex.main * cex,
                      cex.lab = par_opt$cex.lab * cex,
                      cex.axis = par_opt$cex.axis * cex,
                      xaxs = xaxs, yaxs = yaxs,
                      tck = tck, ...)
    graphics::axis(
      side = 2, at = y0, labels = channel_names, pos = start_time +
        time_shift, las = 1, tck = tck,
      cex = cex, cex.main = par_opt$cex.main * cex,
      cex.lab = par_opt$cex.lab * cex,
      cex.axis = par_opt$cex.axis * cex
    )
    graphics::mtext(side = 2, text = ylab, line = yline,
                    cex = par_opt$cex.lab * cex)
    graphics::mtext(side = 1, text = xlab, line = xline,
                    cex = par_opt$cex.lab * cex)
    # graphics::title(ylab = ylab, line = 1, cex.lab = cex * 0.8)
  }
  else {
    graphics::matpoints(time_shift + Time, t(r), type = "l",
                        col = col, lty = 1, lwd = lwd,
                        xlim = xlim, cex = cex,
                        cex.main = par_opt$cex.main * cex,
                        cex.lab = par_opt$cex.lab * cex,
                        cex.axis = par_opt$cex.axis * cex)
  }
  return(invisible(list(space = space, space_mode = space_mode, compress = compress,
                        time_range = time_range)))
}
