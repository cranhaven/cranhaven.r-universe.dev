#' @title gOverview
#'
#' @description This function generates a graphic overview of the spectroscopic
#'   data.
#'
#' @param data_abs A data frame, which contains in the first column the wave
#'   numbers and in the following columns the samples absorbances.
#' @param data_ndd A data frame, which contains in the first column the wave
#'   numbers and in the following columns the samples second derivative values.
#' @param fontFamily The desired graphic font family.
#'
#' @return
#' \describe{
#'  \item{If \emph{data_ndd is provided}:}{It returns a grid with three plots:
#'   \itemize{
#'    \item The ensemble of all samples spectra.
#'    \item The ensemble of the second derivative spectra of all samples.
#'    \item The average and standard deviation spectra.
#'   }
#'  }
#'  \item{If \emph{data_ndd is omitted}:}{It returns a grid with two plots:
#'   \itemize{
#'    \item The ensemble of all samples spectra.
#'    \item The average and standard deviation spectra.
#'   }
#'  }
#' }
#'
#' @seealso \code{\link{importSpectra}}, \code{\link{findPeaks}} and
#'   \code{\link{plotPeaks}}
#'
#' @examples
#' # Grapic overview of your raw data
#' gOverview(andurinhaData)
#'
#' # Graphic overview of your processed data by findPeaks()
#' fp <- findPeaks(andurinhaData)
#' gOverview(fp$dataZ, fp$secondDerivative)
#'
#' @export

gOverview <- function(data_abs, data_ndd, fontFamily = NULL) {

  if (!is.data.frame(data_abs)) {
    abort(paste0(
      "`data_abs` must be a data frame; not ", typeof(data_abs), "."
    ))
  }

  if (!is.null(fontFamily) && !is.character(fontFamily)) {
    abort(paste0(
      "`fontFamily` must be a valid font family name; not ", typeof(fontFamily), "."
    ))
  }

  # ------------------------------------------------------------------
  # DATA PLOT:
  # reshape data to long table
  dataAbsLong <- gather(data_abs, sample, abs, names(data_abs)[-1], factor_key = TRUE)

  # plot
  dataAbsPlot <- ggplot(dataAbsLong, aes(x = dataAbsLong[, 1],
                                         y = abs,
                                         group = sample,
                                         color = sample)) +
    geom_line() +
    scale_color_grey(start = 0.5, end = 0.9) +
    scale_x_reverse(breaks = seq(max(dataAbsLong[, 1]),
                                 min(dataAbsLong[, 1]),
                                 -400),
                    position = "top") +
    scale_y_continuous(labels = NULL) +
    labs(x = NULL,
         y = "Absorbance units") +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.position = "none",
          text = element_text(size = 10, family = fontFamily),

          axis.line = element_line(colour = "black"),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),
                                      size = 8),
          axis.text.x  = element_text(margin = margin(t = 0, r = 0, b = 5, l = 0)))

  # ------------------------------------------------------------------
  # AVERAGE - SD PLOT:
  # data average and sd:
  avgSD <- data.frame("WN" = data_abs[, 1],
                      "average" = apply(data_abs[, -1], 1, mean),
                      "sd" = apply(data_abs[, -1], 1, sd))

  # Secondary axis scale factor
  axisScaleFactor <- (max(avgSD[, 2]) - min(avgSD[, 2])) / (max(avgSD[, 3]) - min(avgSD[, 3]))

  # plot
  avgSdPlot <- ggplot(avgSD, aes(x = .data$WN)) +
    geom_line(aes(y = .data$average, colour = "gray47")) +
    geom_line(aes(y = sd * axisScaleFactor, colour = "gray80")) +
    scale_x_reverse(breaks = seq(max(avgSD[, 1]),
                                 min(avgSD[, 1]),
                                 -400)) +
    scale_y_continuous(labels = NULL,
                       position = "right") +
    scale_color_manual(labels = c("Average", "SD"),
                       values = c("gray47", "gray80")) +
    labs(x = bquote("Wavenumber (" ~ cm^-1*")"),
         y = "Absorbance units") +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.position = "top",
          legend.justification = "right",
          legend.text = element_text(size = 10),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          text = element_text(size = 10, family = fontFamily),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                      size = 8),
          axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                                            size = 8),
          axis.text.x  = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

  if (missing(data_ndd)) {

    # ------------------------------------------------------------------
    # GRID:
    plot_grid(dataAbsPlot, avgSdPlot,
              nrow = 2,
              align = "v")

  } else if (!is.data.frame(data_ndd)) {

    abort(paste0(
      "`data_ndd` must be a data frame; not ", typeof(data_ndd), "."
    ))

  } else {

    # ------------------------------------------------------------------
    # SECOND DERVATIVE PLOT:
    # reshape data to long table
    dataNddLong <- gather(data_ndd, sample, abs, names(data_ndd)[-1], factor_key = TRUE)

    # plot
    dataNddPlot <- ggplot(dataNddLong, aes(x = dataNddLong[, 1],
                                           y = abs,
                                           group = sample,
                                           color = sample)) +
      geom_line() +
      scale_color_grey(start = 0.5, end = 0.9) +
      scale_x_reverse() +
      scale_y_continuous(labels = NULL) +
      labs(x = NULL,
           y = "Second derivative") +
      theme(panel.grid = element_blank(),
            panel.background = element_blank(),
            legend.position = "none",
            plot.title = element_text(margin = margin(t = 5, r = 0, b = 15, l = 0),
                                      hjust = 0.5,
                                      face = "bold"),
            text = element_text(size = 10, family = fontFamily),

            axis.line = element_line(colour = "black"),
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),
                                        size = 8),
            axis.text.x  = element_blank())

    # ------------------------------------------------------------------
    # GRID:
    plot_grid(dataAbsPlot, dataNddPlot, avgSdPlot,
              nrow = 3,
              align = "v")
  }
}
