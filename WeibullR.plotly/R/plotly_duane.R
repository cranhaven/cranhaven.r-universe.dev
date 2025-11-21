#' Interactive Duane Plot.
#'
#' This function creates an interactive Duane plot for a duane object. The plot
#' includes options to customize the appearance, such as colors and grid visibility.
#'
#'
#' @param duane_obj An object of class 'duane'. This object is created
#' using the `duane` function from the ReliaGrowR package.
#' @param showGrid Show grid (TRUE) or hide grid (FALSE). Default is TRUE.
#' @param main Main title. Default is "Duane Plot".
#' @param xlab X-axis label. Default is "Cumulative Time".
#' @param ylab Y-axis label. Default is "Cumulative MTBF".
#' @param pointCol Color of the point values. Default is "black".
#' @param fitCol Color of the model fit. Default is "black".
#' @param gridCol Color of the grid. Default is "lightgray".
#' @return The function returns no value. It generates an interactive Duane plot.
#' @examples
#' library(ReliaGrowR)
#' times<-c(100, 200, 300, 400, 500)
#' failures<-c(1, 2, 1, 3, 2)
#' fit<-duane(times, failures)
#' plotly_duane(fit)
#' @import ReliaGrowR plotly
#' @importFrom graphics text
#' @importFrom stats runif qnorm
#' @export

plotly_duane <- function(duane_obj,
                       showGrid = TRUE,
                       main = "Duane Plot",
                       xlab = "Cumulative Time",
                       ylab = "Cumulative MTBF",
                       pointCol = "black",
                       fitCol = "black",
                       gridCol = "lightgray") {

  # Validate inputs
  validate_inputs <- function() {
    if (!identical(class(duane_obj), "duane")) {
      stop("Argument 'duane_obj' is not of class 'duane'.")
    }
  }
  validate_inputs()

  # Create the  plot
  plotDuane <- function() {

    # Set up the plot layout
    xgrid <- ifelse(is.null(showGrid) || isTRUE(showGrid), TRUE, FALSE)
    ygrid <- xgrid

    # Extract data from duane_obj
    times <- duane_obj$Cumulative_Time
    mtbf <- duane_obj$Cumulative_MTBF
    fitted <- duane_obj$Fitted_Values

    duanePlot <- plot_ly(x=times, y=mtbf, type='scatter', mode='markers',
                         marker=list(color=pointCol), showlegend=FALSE, name="",
                         text=~paste0("MTBF: (",times,", ",mtbf,")"), hoverinfo = 'text'
    ) %>%

      # Set up the main probability plot layout
      layout(title=main,
             xaxis = list(type='log', title=xlab, showline=TRUE, mirror='ticks',
                          showgrid=TRUE, gridcolor=gridCol),
             yaxis = list(type='log', title=ylab, showline=TRUE, mirror = 'ticks',
                          size=text, showgrid=TRUE, gridcolor=gridCol)
      ) %>%

      # Add best fit
      add_trace(x=times, y=fitted, mode='markers+lines',
                marker=list(color='transparent'), line = list(color = fitCol),
                text=~paste0("Fit: ",times,", ",fitted,")"), hoverinfo = 'text'
      )

    return(duanePlot)
  }
  final_plot <- plotDuane()

  return(final_plot)
}
