#' generate ggplot objects for each of the curve fits in a MALDIassay object
#'
#' @param object    object of class MALDIassay
#' @param mzIdx     numeric, indicies of mz values to plot (see \code{getPeakStatistics()}). Note, fc_thresh and R2_thresh filters do not apply if mzIdx is set!
#' @param errorbars character, add error bars to plot. Either standard error of the mean (`sem`) or standard deviation (`sd`) in regards to the measurement replicates or no errorbars (`none`).
#'
#' @return
#' list of ggplot objects
#'
#' @importFrom ggplot2 geom_errorbar geom_point ggplot aes geom_line scale_x_continuous theme element_text labs
#' @importFrom dplyr group_by summarise n
#' @importFrom tibble tibble
#' @importFrom nplr getGoodness getEstimates getXcurve getYcurve getX getY convertToProp
#' @export
#' @examples
#' 
#' data(Blank2022res)
#' plotCurves(Blank2022res, mzIdx = 2, errorbars = "sd")
plotCurves <- function(object, mzIdx = NULL, errorbars = c("none", "sd", "sem")) {
  stopIfNotIsMALDIassay(object)
  errorbars <- match.arg(errorbars)

  if (is.null(mzIdx)) {
    res_list <- getCurveFits(object)
  } else {
    res_list <- getCurveFits(object)[mzIdx]
  }
  len <- length(names(res_list))
  mz_vals <- as.numeric(names(res_list))

  p_list <- vector("list", length = len)
  for (i in 1:len) {
    mz <- mz_vals[i]
    model <- res_list[[as.character(mz)]]$model
    df <- res_list[[as.character(mz)]]$df

    ic50 <- .getEstimates(model, 0.5)
    min <- min(df$value)
    max <- max(df$value)

    fc <- calculateFC(model)
    R2 <- getGoodness(model)[[1]]

    df_C <- tibble(xC = getXcurve(model), yC = getYcurve(model))
    df_P <- tibble(x = getX(model), y = getY(model))

    int <- vapply(getSinglePeaks(object), function(x) {
      targetmass <- mz
      mass <- mass(x)
      idx <- match.closest(targetmass, mass, tolerance = 0.01)
      int <- intensity(x)
      return(int[idx])
    }, numeric(1))

    df_singlePeaks <- tibble(
      x = getConc(object),
      int = int) %>%
      group_by(.data$x) %>%
      summarise(
        sd = sd(.data$int),
        sem = .data$sd/sqrt(n())
      )

    df_P <- df_P %>%
      mutate(sem = pull(df_singlePeaks, .data$sem),
             sd = pull(df_singlePeaks, .data$sd))

    p <- ggplot(data = df_P, aes(x = .data$x, y = .data$y)) +
      geom_line(data = df_C, aes(x = .data$xC, y = .data$yC), size = 1, alpha = 0.75) +
      geom_point(size = 3) +
      scale_x_continuous(labels = scales::scientific(10^df_P$x), breaks = df_P$x) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        x = "Conc.",
        y = "Intensity",
        title = paste0(
          "mz ", round(mz, 2), " Da, R\u00B2=", round(R2, 3), "\n",
          "pIC50=", round(ic50, 3),
          " min=", round(min, 3),
          " max=", round(max, 3),
          " FC=", round(fc, 2)
        )
      )

    if (errorbars == "sem") {
      p <- p +
        geom_errorbar(
          aes(
            y = .data$y,
            ymin = .data$y - .data$sem,
            ymax = .data$y + .data$sem
          ),
          alpha = 0.5,
          width = 0.1,
          size = 0.8
        )
    }

    if (errorbars == "sd") {
      p <- p +
        geom_errorbar(
          aes(
            y = .data$y,
            ymin = .data$y - .data$sd,
            ymax = .data$y + .data$sd
          ),
          alpha = 0.5,
          width = 0.1,
          size = 0.8
        )
    }

    p_list[[i]] <- p
    names(p_list) <- mz_vals

  }
  # check for empty entries (result of filtering for FC or R2) and remove them
  idx <- vapply(p_list, function(x) {
    length(x) > 0
  }, FUN.VALUE = TRUE)
  if (sum(!idx) == len) {
    stop("Nothing to plot. Condsider decreasing fc_thresh or R2_tresh.\n")
  }

  if (length(p_list) == 1) {
    return(p_list[[1]])
  }

  return(p_list[idx])
}
