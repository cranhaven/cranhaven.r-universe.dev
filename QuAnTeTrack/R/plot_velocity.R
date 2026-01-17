#' Plot trajectories colored by velocity or relative stride length
#'
#' \code{plot_velocity()} creates a plot of trajectories, colored by either velocity or relative stride length from \code{track} and \code{track velocity} R objects. The function uses \pkg{ggplot2} package for visualization and allows customization of line width and color gradients.
#'
#' @param data A \code{track} R object, which is a list consisting of two elements:
#'    * \strong{\code{Trajectories}}: A list of interpolated trajectories, where each trajectory is a series of midpoints between consecutive footprints.
#'    * \strong{\code{Footprints}}: A list of data frames containing footprint coordinates, metadata (e.g., image reference, ID), and a marker indicating whether the footprint is actual or inferred.
#' @param trackvel A \code{track velocity} R object consisting of a list where each element corresponds to a track and contains velocity or relative stride length data.
#' @param param A character string specifying the parameter to plot. Options are:
#'    - \code{"V"} for velocity.
#'    - \code{"RSL"} for relative stride length.
#'    If \code{NULL}, the default value \code{"V"} will be used.
#' @param lwd Numeric. Line width for the plotted trajectories. Default is \code{1}.
#' @param colours A vector of colors to use for the gradient. Default is a predefined set of colors.
#' @param legend Logical. If \code{TRUE}, the legend will be shown. If \code{FALSE}, the legend will be removed. Default is \code{TRUE}.
#'
#' @details
#' The function creates a plot where each trajectory is colored based on the specified parameter (\code{"V"} for velocity or \code{"RSL"} for relative stride length). The \pkg{ggplot2} package is used for plotting.
#'
#' The color gradient for the parameter is applied using \code{scale_color_gradientn()}. The color palette can be customized via the \code{colours} argument.
#'
#' @return A \code{ggplot} object showing the trajectories colored by the specified parameter.
#'
#' @section Logo:
#' \if{html}{\figure{Logo.png}{options: width=30\%}}
#'
#' @author Humberto G. Ferrón
#' @author humberto.ferron@uv.es
#' @author Macroevolution and Functional Morphology Research Group (www.macrofun.es)
#' @author Cavanilles Institute of Biodiversity and Evolutionary Biology
#' @author Calle Catedrático José Beltrán Martínez, nº 2
#' @author 46980 Paterna - Valencia - Spain
#' @author Phone: +34 (9635) 44477
#'
#' @examples
#' # Example 1: Plot Trajectories Colored by Velocity with Default Settings (MountTom dataset)
#'
#' # Hip heights for each track in the MountTom dataset
#' H_mounttom <- c(
#'   1.380, 1.404, 1.320, 1.736, 1.364, 1.432, 1.508, 1.768, 1.600, 1.848,
#'   1.532, 1.532, 0.760, 1.532, 1.688, 1.620, 0.636, 1.784, 1.676, 1.872,
#'   1.648, 1.760, 1.612
#' )
#'
#' # Calculate velocities using the default Method "A"
#' V_mounttom <- velocity_track(MountTom, H = H_mounttom)
#'
#' # Plot trajectories colored by velocity
#' plot1 <- plot_velocity(MountTom, V_mounttom, param = "V")
#' print(plot1)
#'
#' # Example 2: Plot Trajectories Colored by Relative Stride Length with Default Settings
#' # (PaluxyRiver dataset)
#'
#' # Hip heights for each track in the PaluxyRiver dataset
#' H_paluxyriver <- c(3.472, 2.200)
#'
#' # Specify different methods for different tracks
#' Method_paluxyriver <- c("A", "B")
#'
#' # Calculate velocities using specified methods
#' V_paluxyriver <- velocity_track(PaluxyRiver, H = H_paluxyriver, method = Method_paluxyriver)
#'
#' # Plot trajectories colored by relative stride length
#' plot2 <- plot_velocity(PaluxyRiver, V_paluxyriver, param = "RSL")
#' print(plot2)
#'
#' # Example 3: Plot Trajectories Colored by Velocity with Custom Line Width and Colors
#' # (MountTom dataset)
#'
#' # Custom colors and line width
#' custom_colours <- c("blue", "green", "yellow", "red")
#' custom_lwd <- 2
#'
#' # Plot trajectories with custom colors and line width
#' plot3 <- plot_velocity(MountTom, V_mounttom,
#'   param = "V", lwd = custom_lwd,
#'   colours = custom_colours
#' )
#' print(plot3)
#'
#' # Example 4: Plot Trajectories Colored by Relative Stride Length with Custom Line Width
#' # and No Legend (PaluxyRiver dataset)
#'
#' # Custom colors and line width
#' custom_colours_rsl <- c("purple", "orange", "pink", "gray")
#' custom_lwd_rsl <- 1.5
#'
#' # Plot trajectories with custom colors, line width, and no legend
#' plot4 <- plot_velocity(PaluxyRiver, V_paluxyriver,
#'   param = "RSL", lwd = custom_lwd_rsl,
#'   colours = custom_colours_rsl, legend = FALSE
#' )
#' print(plot4)
#'
#' @importFrom ggplot2 scale_color_gradientn
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 labs
#'
#' @seealso \code{\link{tps_to_track}}, \code{\link{velocity_track}}, \code{\link[ggplot2]{scale_color_gradientn}}
#'
#' @export

plot_velocity <- function(data, trackvel, param = NULL, lwd = NULL, colours = NULL, legend = NULL) {

  ## Set default values if arguments are NULL ----
  if (is.null(param)) param <- "V" # Default to "V" if 'param' is NULL
  if (is.null(lwd)) lwd <- 1 # Default line width for tracks
  if (is.null(colours)) colours <- c("#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#fffdbf", "#fee090", "#fdae61", "#f46d43", "#d73027") # Default colors
  if (is.null(legend)) legend <- TRUE # Default to "TRUE" if 'legend' is NULL


  ## Errors and Warnings----

  # Check if 'data' is a list with two elements
  if (!is.list(data) || length(data) < 2) {
    stop("The 'data' argument must be a 'track' R object, which is a list consisting of two elements.")
  }

  # Check if the two elements of 'data' are lists
  if (!is.list(data[[1]]) || !is.list(data[[2]])) {
    stop("The two elements of 'data' must be lists.")
  }

  # Check that 'trackvel' is a lists
  if (!is.list(trackvel)) {
    stop("'trackvel' must be a list.")
  }

  # Ensure 'param' is one of the valid options or NULL
  if (!is.null(param) && !param %in% c("V", "RSL")) {
    stop("Invalid value for 'param'. Choose 'V' for velocity, 'RSL' for relative stride length, or NULL.")
  }

  # Ensure 'colours' is a character vector
  if (!is.character(colours)) {
    stop("'colours' must be a character vector of color specifications.")
  }


  ## Code----

  # Extract and prepare data for plotting
  data <- data[[1]]

  # Create an empty data frame with appropriate column names
  M <- data.frame(matrix(nrow = length(do.call(rbind, data)[, 1]), ncol = 4))
  colnames(M) <- c("x", "y", "col", "track")

  # Populate 'x' and 'y' columns from 'data'
  M[, 1] <- do.call(rbind, data)[, 1]
  M[, 2] <- do.call(rbind, data)[, 2]

  # Handle parameter-specific color assignments
  if (param == "V") {
    n <- c(trackvel[[1]][[1]], trackvel[[1]][[1]][[length(trackvel[[1]][[1]])]])
    if (length(data) > 1) {
      for (i in 2:length(data)) {
        n <- c(n, c(trackvel[[i]][[1]], trackvel[[i]][[1]][[length(trackvel[[i]][[1]])]]))
      }
    }
    M[, 3] <- n
  } else if (param == "RSL") {
    n <- c(trackvel[[1]][[6]], trackvel[[1]][[6]][[length(trackvel[[1]][[6]])]])
    if (length(data) > 1) {
      for (i in 2:length(data)) {
        n <- c(n, c(trackvel[[i]][[6]], trackvel[[i]][[6]][[length(trackvel[[i]][[6]])]]))
      }
    }
    M[, 3] <- n
  }

  # Create a vector for the 'track' column
  n <- rep(names(data[1]), length(data[[1]][, 1]))
  if (length(data) > 1) {
    for (i in 2:length(data)) {
      n <- c(n, rep(names(data[i]), length(data[[i]][, 1])))
    }
  }
  M[, 4] <- n

  # Generate the plot using ggplot2
  plot <- ggplot(data = M, aes(x = x, y = y, group = track, color = col)) +
    geom_path(linewidth = lwd) +
    coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) +
    theme_light()

  # Customize color scale and labels based on the parameter
  if (param == "V") {
    plot <- plot + scale_color_gradientn(name = "Velocity (m/s)", colours = colours) + labs(y = "m", x = "m")
  } else if (param == "RSL") {
    plot <- plot + scale_color_gradientn(name = "Relative stride length", colours = colours) + labs(y = "m", x = "m")
  }

  # Remove the legend if specified
  if (!legend) {
    plot <- plot + theme(legend.position = "none")
  }

  return(plot)
}
