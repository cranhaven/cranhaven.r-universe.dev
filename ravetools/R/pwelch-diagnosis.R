#' Show channel signals with diagnostic plots
#' @description The diagnostic plots include 'Welch Periodogram'
#' (\code{\link{pwelch}}) and histogram (\code{\link[graphics]{hist}})
#' @param s1 the main signal to draw
#' @param s2 the comparing signal to draw; usually \code{s1} after some filters;
#' must be in the same sampling rate with \code{s1}; can be \code{NULL}
#' @param sc decimated \code{s1} to show if \code{srate} is too high; will
#' be automatically generated if \code{NULL}
#' @param srate sampling rate
#' @param name name of \code{s1}, or a vector of two names of \code{s1} and
#' \code{s2} if \code{s2} is provided
#' @param try_compress whether try to compress (decimate) \code{s1} if
#' \code{srate} is too high for performance concerns
#' @param max_freq the maximum frequency to display in 'Welch Periodograms'
#' @param plim the y-axis limit to draw in 'Welch Periodograms'
#' @param window,noverlap see \code{\link{pwelch}}
#' @param cex,lwd,mar,cex.lab,mgp,xaxs,yaxs,tck,... graphical parameters; see
#' \code{\link[graphics]{par}}
#' @param xline,yline distance of axis labels towards ticks
#' @param nclass number of classes to show in histogram
#' (\code{\link[graphics]{hist}})
#' @param main the title of the signal plot
#' @param col colors of \code{s1} and \code{s2}
#' @param which \code{NULL} or integer from 1 to 4; if \code{NULL}, all plots
#' will be displayed; otherwise only the subplot will be displayed
#' @param start_time the starting time of channel (will only be used to draw
#' signals)
#' @param boundary a red boundary to show in channel plot; default is
#' to be automatically determined by \code{std}
#' @param std the standard deviation of the channel signals used to determine
#' \code{boundary}; default is plus-minus 3 standard deviation
#' @returns A list of boundary and y-axis limit used to draw the channel
#' @examples
#' library(ravetools)
#'
#' # Generate 20 second data at 2000 Hz
#' time <- seq(0, 20, by = 1 / 2000)
#' signal <- sin( 120 * pi * time) +
#'   sin(time * 20*pi) +
#'   exp(-time^2) *
#'   cos(time * 10*pi) +
#'   rnorm(length(time))
#'
#' signal2 <- notch_filter(signal, 2000)
#'
#' diagnose_channel(signal, signal2, srate = 2000,
#'                  name = c("Raw", "Filtered"), cex = 1)
#'
#' @export
diagnose_channel <- function(
  s1, s2 = NULL, sc = NULL, srate, name = '', try_compress = TRUE,
  max_freq = 300, window = ceiling(srate * 2), noverlap = window / 2, std = 3,
  which = NULL, main = 'Channel Inspection', col = c('black', 'red'),
  cex = 1.2, cex.lab = 1,
  lwd = 0.5, plim = NULL, nclass = 100, start_time = 0, boundary = NULL,
  mar = c(3.1, 4.1, 2.1, 0.8) * (0.25 + cex * 0.75) + 0.1,
  # mai = c(0.41, 0.51, 0.4, 0.1) * (0.5 + cex / 2),
  mgp = cex * c(2, 0.5, 0),
  xaxs = "i", yaxs = "i", xline = 1.66 * cex, yline = 2.66 * cex,
  tck = -0.005 * (3 + cex),
  ...){

  # is sc not specified, and srate is too high, compress s1
  if(try_compress && (is.null(sc) || (srate > 200 && length(s1) / srate > 300))){
    sratec <- 100
    sc <- s1[round(seq(1, length(s1), by = srate/sratec))]
  }else{
    sc %?<-% s1
    sratec <- srate / length(s1) * length(sc)
  }
  max_freq <- min(max_freq, floor(srate/ 2))
  xlim <- c(1, max_freq)

  # Calculate boundary to draw
  if(is.null(boundary)){
    boundary <- std* stats::sd(s1)
  }
  ylim <- max(abs(s1), boundary)

  # Grid layout

  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main", "cex.lab", "cex.axis", "cex.sub"))

  if( length(which) == 0 || prod(graphics::par("mfrow")) > 1 ) {
    par_opt$cex.lab <- cex.lab * 0.7
  } else {
    par_opt$cex.lab <- cex.lab
  }


  on.exit({graphics::par(par_opt)}, add = TRUE)
  graphics::par(mar = mar, mgp = mgp, cex.lab = par_opt$cex.lab)

  if(length(which) == 0){
    # grid::grid.newpage()
    lay <- rbind(c(1,1,1), c(2,3,4))
    graphics::layout(mat = lay)
  }

  # First plot: plot sc directly with col[1]
  if(length(which) == 0 || 1 %in% which){
    graphics::plot(
      start_time + (seq_along(sc) / sratec), sc, xlab = '', ylab = '',
      main = main, lwd = lwd,
      type = 'l', ylim = c(-ylim-1, ylim+1), yaxt="n", col = col[1],
      xaxs = xaxs, yaxs = yaxs, tck = tck, ...,
      cex = cex, cex.main = par_opt$cex.main * cex,
      cex.lab = par_opt$cex.lab * cex,
      cex.axis = par_opt$cex.axis * cex
    )
    graphics::abline(h = c(-1,1) * boundary, col = 'red')
    ticks<-c(-ylim,-boundary, 0, boundary, ylim)
    graphics::axis(2,at=ticks,labels=round(ticks), las = 1, tck = tck,
                   cex = cex, cex.main = par_opt$cex.main * cex,
                   cex.lab = par_opt$cex.lab * cex,
                   cex.axis = par_opt$cex.axis * cex)
    # graphics::title(main = main, cex.main = cex * graphics::par('cex.main'))
    graphics::mtext(side = 2, text = 'Voltage', line = yline,
                    cex = par_opt$cex.lab * cex)
    graphics::mtext(side = 1, text = 'Time (seconds)', line = xline,
                    cex = par_opt$cex.lab * cex)
  }

  # plot 2, 3 too slow, need to be faster - pwelch periodogram
  if(length(which) == 0 || any(c(2, 3) %in% which)){
    ps1 <- pwelch(s1, fs = srate, window = window, noverlap = noverlap, plot = FALSE)
    has_s2 <- !is.null(s2)
    if(has_s2) {
      ps2 <- pwelch(s2, fs = srate, window = window, noverlap = noverlap, plot = FALSE)
    } else {
      ps2 <- NULL
    }

    if( length(which) == 0 || 2 %in% which ) {

      if(has_s2) {
        plot(ps2, add = FALSE, col = col[2], cex = cex, ylim = plim,
             log = 'y', xlim = xlim, xaxs = xaxs, yaxs = yaxs, mar = mar,
             xline = xline, yline = yline, main = "Welch Periodogram",
             mgp = mgp, tck = tck)
        plot(ps1, add = TRUE, col = col[1], cex = cex, ylim = plim,
             log = 'y', xlim = xlim, xaxs = xaxs, yaxs = yaxs, mar = mar,
             xline = xline, yline = yline, main = "Welch Periodogram",
             mgp = mgp, tck = tck)
      } else {
        plot(ps1, add = FALSE, col = col[1], cex = cex, ylim = plim,
             log = 'y', xlim = xlim, xaxs = xaxs, yaxs = yaxs, mar = mar,
             xline = xline, yline = yline, main = "Welch Periodogram",
             mgp = mgp, tck = tck)
      }

    }

    if( length(which) == 0 || 3 %in% which ) {

      if(has_s2) {
        plot(ps2, add = FALSE, col = col[2], cex = cex, ylim = plim,
             log = 'xy', xlim = xlim, xaxs = xaxs, yaxs = yaxs, mar = mar,
             xline = xline, yline = yline, main = "Welch Periodogram",
             mgp = mgp, tck = tck)
        plot(ps1, add = TRUE, col = col[1], cex = cex, ylim = plim,
             log = 'xy', xlim = xlim, xaxs = xaxs, yaxs = yaxs, mar = mar,
             xline = xline, yline = yline, main = "Welch Periodogram",
             mgp = mgp, tck = tck)
      } else {
        plot(ps1, add = FALSE, col = col[1], cex = cex, ylim = plim,
             log = 'xy', xlim = xlim, xaxs = xaxs, yaxs = yaxs, mar = mar,
             xline = xline, yline = yline, main = "Welch Periodogram",
             mgp = mgp, tck = tck)
      }

    }

  }


  if(length(which) == 0 || 4 %in% which){
    # Plot 4:
    graphics::hist(s1, nclass = nclass, probability = FALSE,
                   xlab = '', ylab = "", main = paste0('Histogram ', name[[1]]),
                   tck = tck, las = 1,
                   cex = cex, cex.main = par_opt$cex.main * cex,
                   cex.lab = par_opt$cex.lab * cex,
                   cex.axis = par_opt$cex.axis * cex)
    graphics::mtext(side = 2, text = 'Frequency', line = yline,
                    cex = par_opt$cex.lab * cex)
    graphics::mtext(side = 1, text = 'Signal Voltage Histogram', line = xline,
                    cex = par_opt$cex.lab * cex)
  }

  return(invisible(list(
    ylim = ylim,
    boundary = boundary
  )))
}
