#' @title plotPeaks
#'
#' @description This function makes a graphic representation of the peaks over
#'   the second derivative and/or absorbance sum spectra.
#'
#' @param peaksWN A vector with the peaks wave numbers.
#' @param data_abs A data frame, which contains in the first column the wave
#'   numbers and in the following columns the samples absorbances.
#' @param data_ndd A data frame, which contains in the first column the wave
#'   numbers and in the following columns the samples second derivative values.
#' @param fontFamily The desired graphic font family.
#'
#' @seealso \code{\link{importSpectra}}, \code{\link{findPeaks}} and
#'   \code{\link{gOverview}}
#'
#' @examples
#' # Plot the peaks found by findPeaks()
#' # 1. Based on absorbance sum spectrum
#' fp.abs <- findPeaks(andurinhaData, ndd = FALSE)
#' plotPeaks(fp.abs[[3]]$WN, fp.abs$dataZ)
#'
#' # 2. Based on second derivative spectrum
#' fp.ndd <- findPeaks(andurinhaData, cutOff = 0.25)
#' plotPeaks(fp.ndd[[4]]$WN, fp.ndd$dataZ, fp.ndd$secondDerivative)
#'
#' @export

plotPeaks <- function(peaksWN, data_abs, data_ndd, fontFamily = NULL) {

  if (!is.vector(peaksWN)) {
    abort(paste0(
      "`peaksWN` must be a vector; not ", typeof(peaksWN), "."
    ))
  }

  if (!is.null(fontFamily) && !is.character(fontFamily)) {
    abort(paste0(
      "`fontFamily` must be a valid font family name; not ", typeof(fontFamily), "."
    ))
  }

  if (!is.data.frame(data_abs)) {

    abort(paste0(
      "`data_abs` must be a data frame; not ", typeof(data_abs), "."
    ))
  }

  # ------------------------------------------------------------------
  # SUM SPECTRUM - Absorbance:
  absSum <- data.frame("WN" = data_abs[, 1],
                       "sumSpectrum" = apply(data_abs[, -1], 1, sum))

  # ------------------------------------------------------------------
  # Second Derivative sum spectrum + peaks PLOT:
  absPlot <- ggplot(absSum, aes(absSum[, 1], absSum[, 2])) +
    geom_line() +
    geom_vline(xintercept = peaksWN, color = "orange") +
    scale_x_reverse(breaks = seq(max(absSum[, 1]),
                                 min(absSum[, 1]),
                                 -400)) +
    labs(x = bquote('Wavenumber (' ~ cm^-1*')'),
         y = "Absorbance units") +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.position = "none",
          text = element_text(size = 10, family = fontFamily),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                      size = 8),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),
                                      size = 8),
          axis.text.x  = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
          axis.text.y  = element_blank())

  if (!missing(data_ndd)) {

    if (!is.data.frame(data_ndd)) {

      abort(paste0(
        "`data_ndd` must be a data frame; not ", typeof(data_ndd), "."
      ))

      } else {

        # ------------------------------------------------------------------
        # SUM SPECTRUM - Second Derivative:
        sg39Sum <- data.frame("WN" = data_ndd[, 1],
                              "sumSpectrum" = apply(data_ndd[, -1], 1, sum))

        # ------------------------------------------------------------------
        # Second Derivative sum spectrum + peaks PLOT:
        sg39Plot <- ggplot(sg39Sum, aes(sg39Sum[, 1], sg39Sum[, 2])) +
          geom_line() +
          geom_vline(xintercept = peaksWN, color = "orange") +
          scale_x_reverse() +
          labs(x = NULL,
               y = "Second derivative") +
          theme(panel.grid = element_blank(),
                panel.background = element_blank(),
                legend.position = "none",
                text = element_text(size = 10, family = fontFamily),
                axis.line = element_line(colour = "black"),
                axis.line.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                            size = 8),
                axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),
                                            size = 8),
                axis.text.x  = element_blank(),
                axis.text.y  = element_blank())
      }
    }

    if (missing(data_ndd)) { absPlot } else {

      # ------------------------------------------------------------------
      # GRID:
      plot_grid(sg39Plot, absPlot,
                nrow = 2,
                align = "v")
      }
}
