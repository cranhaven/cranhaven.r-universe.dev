#'  Plot a peak of interest from a MALDIassay object
#'
#' @param object object of class MALDIassay
#' @param mzIdx  numeric, index of mass of interest (see \code{getPeakStatistics()})
#' @param tol    numeric, tolerance around peak to plot
#'
#' @return
#' ggplot object
#'
#' @importFrom ggplot2 ggplot aes geom_line scale_y_continuous scale_color_viridis_d labs geom_rect
#' @importFrom dplyr filter between bind_rows .data
#' @importFrom tibble tibble
#' @importFrom MALDIquant mass intensity
#' @importFrom forcats fct_reorder
#' @export
#' @examples
#' 
#' data(Blank2022res)
#' plotPeak(Blank2022res, mzIdx = 2)
plotPeak <- function(object, mzIdx, tol = 0.8) {
  if (missing(mzIdx)) {
    stop("No mzIdx supplied.\n")
  }
  mz <- as.numeric(names(getCurveFits(object))[mzIdx])
  spec <- getAvgSpectra(object)

  df <- map(spec, function(x) {
    tibble(
      mass = mass(x),
      intensity = intensity(x)
    )
  }) %>%
    bind_rows(.id = "conc") %>%
    filter(between(.data$mass, mz - tol, mz + tol)) %>%
    mutate(conc = as.numeric(.data$conc)) %>%
    mutate(conc = fct_reorder(scales::scientific(.data$conc), .data$conc))

  title <- paste0("Profile of m/z ", round(mz, 2), " \u00B1 ", round(tol, 2), "Da")

  p <-ggplot() +
    geom_rect(aes(xmin = mz - getBinTol(object) * mz,
                  xmax = mz + getBinTol(object) * mz,
                  ymin = 0,
                  ymax = max(pull(df, .data[["intensity"]]))*1.05),
              alpha=0.2,
              fill="black") +
    geom_line(data = df, aes(x = .data[["mass"]], y = .data[["intensity"]], col = .data[["conc"]])) +
    scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
    scale_color_viridis_d(end = 0.75, option = "C") +
    labs(
      x = "m/z",
      y = "Intensity",
      col = "Conc.",
      title = title
    )

  return(p)
}

