#' Interactive Reliability Growth Plot.
#'
#' The function creates an interactive reliability growth plot for an `rga` object.
#' The plot includes cumulative failures over time, the model fit, and optional confidence bounds.
#' Vertical lines indicate change points if breakpoints are specified in the rga object.
#'
#'
#' @param rga_obj An object of class 'rga'. This object is created using the
#' `rga()` function from the `ReliaGrowR` package.
#' @param showConf Show the confidence bounds (TRUE) or not (FALSE).
#' @param showGrid Show grid (TRUE) or hide grid (FALSE).
#' @param main Main title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param pointCol Color of the point values.
#' @param fitCol Color of the model fit.
#' @param confCol Color of the confidence bounds.
#' @param gridCol Color of the grid.
#' @param breakCol Color of the breakpoints.
#' @return The function returns no value. It generates an interactive plotly plot.
#' @examples
#' library(ReliaGrowR)
#' times<-c(100, 200, 300, 400, 500)
#' failures<-c(1, 2, 1, 3, 2)
#' rga<-rga(times, failures)
#' plotly_rga(rga)
#'
#' times <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
#' failures <- c(1, 2, 1, 1, 1, 2, 3, 1, 2, 4)
#' breakpoints <- 400
#' rga2 <- rga(times, failures, model_type = "Piecewise NHPP", breaks = breakpoints)
#' plotly_rga(rga2, fitCol = "blue", confCol = "blue", breakCol = "red")
#' @import ReliaGrowR plotly
#' @importFrom graphics text
#' @importFrom stats runif qnorm
#' @export

plotly_rga <- function(rga_obj,
                       showConf = TRUE,
                       showGrid = TRUE,
                       main = "Reliability Growth Plot",
                       xlab = "Cumulative Time",
                       ylab = "Cumulative Failures",
                       pointCol = "black",
                       fitCol = "black",
                       confCol = "black",
                       gridCol = "lightgray",
                       breakCol = "black") {


  # Validate inputs
  validate_inputs <- function() {
    if (!identical(class(rga_obj), "rga")) {
      stop("Argument 'rga_obj' is not of class 'rga'.")
    }
  }
  validate_inputs()

  # Extract times and cumulative failures from the rga object
  times <- exp(rga_obj$model$model$log_times)
  cum_failures <- exp(rga_obj$model$model$log_cum_failures)

  vline <- function(x = 0, color = breakCol) {
    list(
      type = "line",
      y0 = 0,
      y1 = 1,
      yref = "paper",
      x0 = x,
      x1 = x,
      line = list(color = color, dash="dot")
    )
  }

  # Create the  plot
  plotGrowth <- function() {

    # Set up the plot layout
    fillcolor <- plotly::toRGB(confCol, 0.2)
    xgrid <- ifelse(is.null(showGrid) || isTRUE(showGrid), TRUE, FALSE)
    ygrid <- xgrid

    growthPlot <- plot_ly(x=times, y=cum_failures, type='scatter', mode='markers',
                        marker=list(color=pointCol), showlegend=FALSE,
                        name="", text=~paste0("Failures: (",times,", ",cum_failures,")"), hoverinfo = 'text'
    ) %>%

      # Set up the main probability plot layout
      layout(title=main,
             xaxis = list(title=xlab, showline=TRUE, mirror='ticks',
                          showgrid=xgrid, gridcolor=gridCol),
             yaxis = list(title=ylab, showline=TRUE, mirror = 'ticks',
                          size=text, showgrid=ygrid, gridcolor=gridCol)
      ) %>%

      # Add best fit
      add_trace(x=times, y=rga_obj$fitted_values, mode='markers+lines',
                marker=list(color='transparent'), line = list(color = fitCol),
                text=~paste0("Fit: ",times,", ",rga_obj$fitted_values,")"), hoverinfo = 'text'
      ) %>%

      # Add lower confidence bound
      add_trace(x=times, y=rga_obj$lower_bounds, mode='markers+lines',
                marker=list(color='transparent'), line=list(color='transparent'),
                text=~paste0("Lower: ",times,", ",rga_obj$lower_bounds,")"), hoverinfo = 'text'

      ) %>%

      # Add upper confidence bound
      add_trace(x=times, y=rga_obj$upper_bounds, mode='markers+lines',
                fill='tonexty',
                fillcolor=fillcolor,
                marker=list(color='transparent'), line=list(color='transparent'),
                text=~paste0("Upper: ",times,", ",rga_obj$upper_bounds,")"), hoverinfo='text'
      ) %>%

      # Add vertical lines at the change points
      layout(shapes = list(
        vline(exp(as.numeric(rga_obj$breakpoints)))
      )
      )

    return(growthPlot)
  }
  final_plot <- plotGrowth()

  return(final_plot)
}
