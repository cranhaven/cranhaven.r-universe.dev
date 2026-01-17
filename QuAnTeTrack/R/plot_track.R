#' Plot tracks and footprints
#'
#' \code{plot_track()} visualizes track and footprint data in various ways, allowing for the plotting of trajectories, footprints, or both combined, with customizable aesthetics.
#'
#' @param data A \code{track} R object, which is a list consisting of two elements:
#'    * \strong{\code{Trajectories}}: A list of interpolated trajectories, where each trajectory is a series of midpoints between consecutive footprints.
#'    * \strong{\code{Footprints}}: A list of data frames containing footprint coordinates, metadata (e.g., image reference, ID), and a marker indicating whether the footprint is actual or inferred.
#' @param plot Type of plot to generate. Options are \code{"FootprintsTracks"} (default), \code{"Tracks"}, or \code{"Footprints"}. Determines what elements are included in the plot.
#' @param colours A vector of colors to be used for different tracks. If \code{NULL}, defaults to black. The length of this vector should match the number of tracks in the data.
#' @param cex.f The size of the footprint points. Default is \code{2.5}.
#' @param shape.f A vector of shapes to be used for footprints in different tracks. If \code{NULL}, defaults to \code{19} (solid circle). The length of this vector should match the number of tracks in the data.
#' @param alpha.f The transparency of the footprint points. Default is \code{0.5}.
#' @param cex.t The size of the track lines. Default is \code{0.5}.
#' @param alpha.t The transparency of the track lines. Default is \code{1}.
#' @param plot.labels Logical indicating whether to add labels to each track. Default is \code{FALSE}.
#' @param labels A vector of labels for each track. If \code{NULL}, labels are automatically generated from track names.
#' @param box.p Padding around label boxes, used only if \code{plot.labels} is \code{TRUE}. Adjusts the spacing around the label text.
#' @param cex.l The size of the labels. Default is \code{3.88}.
#' @param alpha.l The transparency of the labels. Default is \code{0.5}.
#'
#' @return A \code{ggplot} object that displays the specified plot type, including tracks, footprints, or both, from \code{track} R objects. The \pkg{ggplot2} package is used for plotting.
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
#' # Example 1: Basic Plot with Default Settings - MountTom Dataset
#' plot_track(MountTom)
#'
#' # Example 2: Basic Plot with Default Settings - PaluxyRiver Dataset
#' plot_track(PaluxyRiver)
#'
#' # Example 3: Plot Tracks Only - MountTom Dataset
#' plot_track(MountTom, plot = "Tracks")
#'
#' # Example 4: Plot Footprints Only - PaluxyRiver Dataset
#' plot_track(PaluxyRiver, plot = "Footprints")
#'
#' # Example 5: Custom Colors for Tracks - MountTom Dataset
#' custom_colors <- c(
#'   "#008000", "#0000FF", "#FF0000", "#800080", "#FFA500", "#FFC0CB", "#FFFF00",
#'   "#00FFFF", "#A52A2A", "#FF00FF", "#808080", "#000000", "#006400", "#00008B",
#'   "#8B0000", "#FF8C00", "#008B8B", "#A9A9A9", "#000080", "#808000", "#800000",
#'   "#008080", "#FFD700"
#' )
#' plot_track(MountTom, colours = custom_colors)
#'
#' # Example 6: Larger Footprints and Track Lines - PaluxyRiver Dataset
#' plot_track(PaluxyRiver, cex.f = 5, cex.t = 2)
#'
#' # Example 7: Semi-Transparent Footprints and Tracks - MountTom Dataset
#' plot_track(MountTom, alpha.f = 0.5, alpha.t = 0.5)
#'
#' # Example 8: Different Shapes for Footprints - PaluxyRiver Dataset
#' plot_track(PaluxyRiver, shape.f = c(16, 17))
#'
#' # Example 9: Plot with Labels for Tracks - MountTom Dataset
#' labels <- paste("Track", seq_along(MountTom[[1]]))
#' plot_track(MountTom, plot.labels = TRUE, labels = labels, cex.l = 4, box.p = 0.3, alpha.l = 0.7)
#'
#' # Example 10: Custom Colors and Shapes for Footprints Only - PaluxyRiver Dataset
#' plot_track(PaluxyRiver, plot = "Footprints", colours = c("purple", "orange"), shape.f = c(15, 18))
#'
#' # Example 11: Larger Line Size & Custom Colors for Tracks Only - MountTom Dataset
#' plot_track(MountTom, plot = "Tracks", cex.t = 1.5, colours = custom_colors)
#'
#' # Example 12: Black Footprints and Tracks with Labels - PaluxyRiver Dataset
#' plot_track(PaluxyRiver,
#'   colours = NULL, shape.f = c(16, 16), plot.labels = TRUE,
#'   labels = c("Saurpod", "Theropod"), cex.l = 2, alpha.l = 0.5
#' )
#'
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 alpha
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_light
#' @importFrom ggrepel geom_label_repel
#' @importFrom dplyr bind_rows
#'
#' @seealso \code{\link{tps_to_track}}
#'
#' @export


plot_track <- function(data, plot = "FootprintsTracks", colours = NULL, cex.f = NULL, shape.f = NULL, alpha.f = NULL, cex.t = NULL, alpha.t = NULL, plot.labels = NULL, labels = NULL, box.p = NULL, cex.l = NULL, alpha.l = NULL) {

  ## Set default values if arguments are NULL----
  if (is.null(cex.f)) cex.f <- 2.5 # Set default size for footprint points if 'cex.f' is NULL
  if (is.null(cex.t)) cex.t <- 0.5 # Set default size for track lines if 'cex.t' is NULL
  if (is.null(cex.l)) cex.l <- 3.88 # Set default size for labels if 'cex.l' is NULL

  if (is.null(alpha.f)) alpha.f <- 0.5 # Set default transparency for footprint points if 'alpha.f' is NULL
  if (is.null(alpha.t)) alpha.t <- 1 # Set default transparency for track lines if 'alpha.t' is NULL
  if (is.null(alpha.l)) alpha.l <- 0.5 # Set default transparency for labels if 'alpha.l' is NULL

  if (is.null(colours)) colours <- c(rep("#000000", length(data[[1]]))) # Set default color (black) for all elements if 'colours' is NULL

  if (is.null(shape.f)) shape.f <- c(rep(19, length(data[[1]]))) # Set default shape for footprint points if 'shape.f' is NULL

  if (is.null(plot.labels)) plot.labels <- FALSE # Set default if 'plot.labels' is NULL

  if (is.null(box.p)) box.p <- 0.25 # Set default padding for label boxes if 'box.p' is NULL


  ## Errors and Warnings----

  # Check if 'data' is a list with at least two elements
  if (!is.list(data) || length(data) < 2) {
    stop("The 'data' argument must be a 'track' R object, which is a list consisting of two elements.")
  }

  # Check if the two elements of 'data' are lists
  if (!is.list(data[[1]]) || !is.list(data[[2]])) {
    stop("The two elements of 'data' must be lists.")
  }

  # Check if the length of 'colours' matches the number of tracks if 'colours' is provided
  if (!is.null(colours) && length(colours) != length(data[[1]])) {
    stop("Error: The length of 'colours' must match the number of tracks in the data.")
  }

  # Check if 'plot' argument is valid
  if (!plot %in% c("FootprintsTracks", "Tracks", "Footprints")) {
    stop("Error: The 'plot' argument must be one of 'FootprintsTracks', 'Tracks', or 'Footprints'.")
  }

  # Warning if 'labels' is provided but 'plot.labels' is FALSE
  if (!is.null(labels) && !plot.labels) {
    warning("Warning: 'labels' are provided but 'plot.labels' is set to FALSE. Labels will not be displayed.")
  }

  # Check if the length of 'shape.f' matches the number of tracks if 'colours' is provided
  if (!is.null(shape.f) && length(shape.f) != length(data[[1]])) {
    stop("Error: The length of 'shape.f' must match the number of tracks in the data.")
  }

  # Warning if 'box.p' is not provided when 'plot.labels' is TRUE
  if (plot.labels && is.null(box.p)) {
    warning("Warning: 'box.p' padding around label boxes is NULL. Default padding will be used.")
  }

  # Check if the length of 'labels' matches the number of tracks if 'labels' is provided
  if (!is.null(labels) && length(labels) != length(data[[1]])) {
    stop("Error: The length of 'labels' must match the number of tracks in the data.")
  }

  # Check if 'cex.f', 'cex.t', 'cex.l' are positive numeric values
  if (!is.null(cex.f) && (!is.numeric(cex.f) || cex.f <= 0)) {
    stop("Error: 'cex.f' size of footprint points must be a positive numeric value.")
  }
  if (!is.null(cex.t) && (!is.numeric(cex.t) || cex.t <= 0)) {
    stop("Error: 'cex.t' size of track lines must be a positive numeric value.")
  }
  if (!is.null(cex.l) && (!is.numeric(cex.l) || cex.l <= 0)) {
    stop("Error: 'cex.l' size of labels must be a positive numeric value.")
  }

  # Check if 'alpha.f', 'alpha.t', 'alpha.l' are between 0 and 1
  if (!is.null(alpha.f) && (!is.numeric(alpha.f) || alpha.f < 0 || alpha.f > 1)) {
    stop("Error: 'alpha.f' transparency of footprint points must be a numeric value between 0 and 1.")
  }
  if (!is.null(alpha.t) && (!is.numeric(alpha.t) || alpha.t < 0 || alpha.t > 1)) {
    stop("Error: 'alpha.t' transparency of track lines must be a numeric value between 0 and 1.")
  }
  if (!is.null(alpha.l) && (!is.numeric(alpha.l) || alpha.l < 0 || alpha.l > 1)) {
    stop("Error: 'alpha.l' transparency of labels must be a numeric value between 0 and 1.")
  }

  # Warning if 'plot.labels' is TRUE but 'labels' is NULL
  if (plot.labels && is.null(labels)) {
    warning("Warning: 'plot.labels' is TRUE but 'labels' is NULL. Automatically generated labels will be used.")
  }

  # Check if 'box.p' (padding around label boxes) is numeric and positive
  if (!is.null(box.p) && (!is.numeric(box.p) || box.p <= 0)) {
    stop("Error: 'box.p' padding around label boxes must be a positive numeric value.")
  }

  ## Code----

  # Combine data from all tracks and footprints into single data frames----
  footprints <- bind_rows(data[[2]]) # Combine all footprint data into one data frame
  tracks <- bind_rows(data[[1]]) # Combine all track data into one data frame

  # Create a data frame for plotting labels----
  mat <- data.frame(matrix(nrow = length(data[[1]]), ncol = 3)) # Initialize a data frame with three columns for X, Y coordinates, and Name
  colnames(mat) <- c("X", "Y", "Name") # Name the columns

  # Populate 'mat' with the first X and Y coordinates from each track----
  for (i in 1:length(data[[1]])) {
    mat[i, 1] <- data[[1]][[i]]$x[[1]] # Set X coordinate for each track
    mat[i, 2] <- data[[1]][[i]]$y[[1]] # Set Y coordinate for each track
  }

  # Assign labels if provided, otherwise use track names----
  if (is.null(labels) == FALSE) {
    mat$Name <- labels # Assign provided labels to 'Name' column
  }
  if (is.null(labels) == TRUE) {
    for (i in 1:length(data[[1]])) {
      mat$Name[i] <- gsub("_", " ", names(data[[1]][i])) # Generate labels by removing underscores from track names
    }
  }

  # Plot Footprints and Tracks together----
  if (plot == "FootprintsTracks") {
    plotfig <- ggplot() +
      geom_path(data = tracks, linewidth = cex.t, aes(x = x, y = y, group = IMAGE, colour = IMAGE), alpha = alpha.t) + # Plot track lines
      geom_point(cex = cex.f, data = footprints, aes(x = X, y = Y, colour = IMAGE, shape = IMAGE), alpha = alpha.f) + # Plot footprint points
      coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) + # Set equal scaling for X and Y axes
      theme_light() +
      labs(y = "m", x = "m") +
      theme(legend.position = "none") + # Light theme and remove legend
      scale_colour_manual(values = colours) + # Set custom colors
      scale_shape_manual(values = shape.f) # Assign specific shapes
  }

  # Plot only Tracks----
  if (plot == "Tracks") {
    plotfig <- ggplot() +
      geom_path(data = tracks, linewidth = cex.t, aes(x = x, y = y, group = IMAGE, colour = IMAGE), alpha = alpha.t) + # Plot track lines
      coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) +
      theme_light() +
      labs(y = "m", x = "m") + # Light theme
      theme(legend.position = "none") +
      scale_colour_manual(values = colours) # Remove legend and set custom colors
  }

  # Plot only Footprints----
  if (plot == "Footprints") {
    plotfig <- ggplot() +
      geom_point(data = footprints, cex = cex.f, aes(x = X, y = Y, colour = IMAGE, shape = IMAGE), alpha = alpha.f) + # Plot footprint points
      coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) +
      theme_light() +
      labs(y = "m", x = "m") +
      theme(legend.position = "none") + # Light theme and remove legend
      scale_colour_manual(values = colours) + # Set custom colors
      scale_shape_manual(values = shape.f)
  }

  # Add labels to the plot if 'plot.labels' is TRUE----
  if (plot.labels == TRUE) {
    options(ggrepel.max.overlaps = Inf) # Allow infinite overlaps for label placement
    plotfig <- plotfig + geom_label_repel(data = mat, aes(x = X, y = Y, label = Name), box.padding = box.p, point.padding = 0, segment.color = "grey50", fill = alpha(c("white"), alpha.l), cex = cex.l) # Add labels with custom settings
  }

  # Return the final plot----
  return(plotfig)
}
