
#' Interactive Contour Plot
#'
#' This function creates an interactive contour plot for one or more
#' `wblr` objects, each assumed to have confidence contours generated via
#' `method.conf = 'lrb'`. The function overlays all contours in a single plot and displays
#' their respective MLE point estimates.
#'
#' @param wblr_obj A single `wblr` object or a list of `wblr` objects. Each object
#' must have contours generated using `method.conf = 'lrb'`.
#' @param main Main title for the plot.
#' @param xlab X-axis label (typically Eta or Sigmalog).
#' @param ylab Y-axis label (typically Beta or Mulog).
#' @param showGrid Logical; whether to show grid lines (default TRUE).
#' @param cols Optional vector of colors for each contour/estimate pair. If not provided,
#'   colors are chosen from a default palette.
#' @param gridCol Color of the grid lines (default 'lightgray').
#' @param signif Number of significant digits to display for estimates and contour coordinates.
#' Defaults to 3.
#' @return A `plotly` object representing the interactive contour plot.
#'
#' @examples
#' library(WeibullR)
#' library(WeibullR.plotly)
#'
#' failures1 <- c(30, 49, 82, 90, 96)
#' failures2 <- c(20, 40, 60, 80, 100)
#' obj1 <- wblr.conf(wblr.fit(wblr(failures1), method.fit = 'mle'), method.conf = 'lrb')
#' obj2 <- wblr.conf(wblr.fit(wblr(failures2), method.fit = 'mle'), method.conf = 'lrb')
#' plotly_contour(list(obj1, obj2), main = "Overlayed Contours")
#'
#' @importFrom plotly plot_ly add_trace layout toRGB
#' @export
plotly_contour <- function(wblr_obj,
                           main = 'Contour Plot',
                           xlab = 'Eta',
                           ylab = 'Beta',
                           showGrid = TRUE,
                           cols = NULL,
                           gridCol = 'lightgray',
                           signif = 3) {

  # Handle input as a list if not already
  if (!is.list(wblr_obj) || inherits(wblr_obj, "wblr")) {
    wblr_obj <- list(wblr_obj)
  }

  # Validate all inputs
  lapply(wblr_obj, function(obj) {
    if (!inherits(obj, "wblr")) {
      stop("All inputs must be of class 'wblr'.")
    }
    if (!identical(obj$fit[[1]]$conf[[1]]$options$method.conf, "lrb")) {
      stop("Each wblr object must have contours generated using method.conf='lrb'.")
    }
  })

  # Set colors
  if (is.null(cols)) {
    default_colors <- c(
      "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
      "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
      "#bcbd22", "#17becf"
    )
    cols <- rep(default_colors, length.out = length(wblr_obj))
  }

  # Start plotly object
  p <- plot_ly()

  for (i in seq_along(wblr_obj)) {
    obj <- wblr_obj[[i]]
    col <- cols[i]

    dist_type <- obj$fit[[1]]$options$dist
    fit_vec <- as.numeric(obj$fit[[1]]$fit_vec)
    contour <- obj$fit[[1]]$conf[[1]]$contour

    if (dist_type %in% c("weibull", "weibull3p")) {
      param1 <- 'Beta'; param2 <- 'Eta'
      val1 <- round(fit_vec[2], signif); val2 <- round(fit_vec[1], signif)
    } else if (dist_type == "lognormal") {
      param1 <- 'Mulog'; param2 <- 'Sigmalog'
      val1 <- round(fit_vec[1], signif); val2 <- round(fit_vec[2], signif)
    } else {
      next
    }

    fillcolor <- toRGB(col, 0.2)

    # Add contour
    p <- p %>%
      add_trace(
        x = round(contour[[1]], signif),
        y = round(contour[[2]], signif),
        type = 'scatter',
        mode = 'lines',
        fill = 'toself',
        fillcolor = fillcolor,
        line = list(color = col),
        name = paste("Contour", i),
        hoverinfo = 'text',
        text = ~paste0("Contour (", round(contour[[1]], signif), ", ", round(contour[[2]], signif), ")")
      ) %>%
      add_trace(
        x = val2,
        y = val1,
        type = 'scatter',
        mode = 'markers',
        marker = list(color = col, size = 10, symbol = "x"),
        name = paste("Estimate", i),
        hoverinfo = 'text',
        text = ~paste0("Estimate: (", val2, ", ", val1, ")")
      )
  }

  # Add layout
  p <- p %>%
    layout(
      title = main,
      xaxis = list(title = xlab, showgrid = showGrid, gridcolor = gridCol),
      yaxis = list(title = ylab, showgrid = showGrid, gridcolor = gridCol),
      showlegend = TRUE
    )

  return(p)
}
