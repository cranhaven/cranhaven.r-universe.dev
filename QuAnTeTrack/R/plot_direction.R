#' Plot direction data in tracks.
#'
#' \code{plot_direction()}  generates different types of plots to visualize the direction data from \code{track} R objects. The function allows for the creation of boxplots, polar histograms of step directions, polar histograms of average directions per track, and faceted polar histograms.
#'
#' @param data A \code{track} R object, which is a list consisting of two elements:
#'    * \strong{\code{Trajectories}}: A list of interpolated trajectories, where each trajectory is a series of midpoints between consecutive footprints.
#'    * \strong{\code{Footprints}}: A list of data frames containing footprint coordinates, metadata (e.g., image reference, ID), and a marker indicating whether the footprint is actual or inferred.
#' @param plot_type A character string indicating the type of plot to generate.
#'   The options are \code{"boxplot"}, \code{"polar_steps"}, \code{"polar_average"}, and \code{"faceted"}.
#'   Default is \code{"boxplot"}.
#' @param angle_range A numeric value specifying the width of the bins (in degrees)
#'   used for polar plots. Default is \code{30} degrees.
#' @param y_labels_position A numeric value specifying the position (in degrees)
#'   of the y-axis labels in the polar plots. Default is \code{90} degrees.
#' @param y_breaks_manual A numeric vector specifying manual breaks for the y-axis
#'   in polar plots. If \code{NULL}, the breaks are calculated automatically. Default is \code{NULL}.
#'
#' @details
#' The \code{plot_direction()} function provides four types of plots:
#'
#' \itemize{
#'   \item \code{"boxplot"}: A boxplot showing the distribution of step direction values
#'   for each track.
#'   \item \code{"polar_steps"}: A polar plot showing the frequency of step
#'   in different direction bins.
#'   \item \code{"polar_average"}: A polar plot showing the frequency of average
#'   directions per track in different direction bins.
#'   \item \code{"faceted"}: A polar plot similar to \code{"polar_steps"} but
#'   faceted by track.
#' }
#'
#' The \code{angle_range} parameter defines the bin width for the polar plots,
#' and \code{y_labels_position} allows for adjusting the position of y-axis labels.
#' The \code{y_breaks_manual} parameter lets users manually specify the breaks
#' on the y-axis for finer control over the appearance of the polar plots.
#'
#' @return A \code{ggplot} object that displays the specified plot type. The \pkg{ggplot2} package is used for plotting.
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
#' # Example 1: Boxplot of Direction Data in MountTom Dataset
#' plot_direction(MountTom, plot_type = "boxplot")
#'
#' # Example 2: Polar Plot of Step Directions in MountTom Dataset
#' plot_direction(MountTom, plot_type = "polar_steps")
#'
#' # Example 3: Polar Plot of Average Directions Per Track in MountTom Dataset
#' plot_direction(MountTom, plot_type = "polar_average")
#'
#' # Example 4: Faceted Polar Plot of Step Directions in MountTom Dataset
#' plot_direction(MountTom, plot_type = "faceted")
#'
#' # Example 5: Polar Plot with Custom Angle Range in MountTom Dataset
#' plot_direction(MountTom, plot_type = "polar_steps", angle_range = 90)
#'
#' # Example 6: Polar Plot with Custom Y-Axis Labels and Breaks in MountTom Dataset
#' plot_direction(MountTom,
#'   plot_type = "polar_steps", y_labels_position = 0,
#'   y_breaks_manual = c(0, 15, 30, 45, 60)
#' )
#'
#' # Example 7: Boxplot of Direction Data in PaluxyRiver Dataset
#' plot_direction(PaluxyRiver, plot_type = "boxplot")
#'
#' # Example 8: Polar Plot of Step Directions in PaluxyRiver Dataset
#' plot_direction(PaluxyRiver, plot_type = "polar_steps")
#'
#' # Example 9: Polar Plot of Average Directions Per Track with Custom Breaks in PaluxyRiver Dataset
#' plot_direction(PaluxyRiver,
#'   plot_type = "polar_average",
#'   y_breaks_manual = c(1, 2)
#' )
#'
#' # Example 10: Faceted Polar Plot of Step Directions in PaluxyRiver Dataset
#' plot_direction(PaluxyRiver, plot_type = "faceted")
#'
#' # Example 11: Polar Plot of Average Directions Per Track with Custom Breaks in PaluxyRiver Dataset
#' plot_direction(PaluxyRiver,
#'   plot_type = "polar_average",
#'   y_breaks_manual = c(1, 2)
#' )
#'
#' # Example 12: Polar Plot with Custom Y-Axis Labels in PaluxyRiver Dataset
#' plot_direction(PaluxyRiver,
#'   plot_type = "polar_steps",
#'   y_labels_position = -90
#' )
#'
#' @importFrom ggplot2 ggplot aes element_blank geom_boxplot geom_point geom_text theme_classic labs scale_y_continuous theme element_text geom_bar coord_polar scale_x_continuous scale_y_continuous theme_minimal facet_wrap annotate
#' @importFrom dplyr group_by summarise ungroup n
#' @importFrom magrittr %>%
#'
#' @seealso \code{\link{tps_to_track}}, \code{\link{test_direction}}
#'
#' @export

plot_direction <- function(data, plot_type = "boxplot", angle_range = 30, y_labels_position = 90, y_breaks_manual = NULL) {

  ## Errors and Warnings ----
  if (!is.list(data) || length(data) < 2) {
    stop("The 'data' argument must be a 'track' R object, which is a list consisting of two elements: 'Trajectories' and 'Footprints'.")
  }
  if (!is.list(data[[1]]) || !is.list(data[[2]])) {
    stop("Both 'Trajectories' and 'Footprints' elements within 'data' must be lists.")
  }
  if (any(sapply(data, function(x) length(x) == 0))) {
    stop("The elements within 'data' must not be empty.")
  }
  if (!is.character(plot_type) || !(plot_type %in% c("boxplot", "polar_steps", "polar_average", "faceted"))) {
    stop("The 'plot_type' must be one of 'boxplot', 'polar_steps', 'polar_average', or 'faceted'.")
  }
  if (!is.numeric(angle_range) || angle_range <= 0 || angle_range > 180) {
    stop("The 'angle_range' must be a numeric value between 1 and 180 degrees.")
  }
  if (!is.numeric(y_labels_position) || y_labels_position < -180 || y_labels_position > 180) {
    stop("The 'y_labels_position' must be a numeric value between -180 and 180 degrees.")
  }
  if (!is.null(y_breaks_manual) && (!is.numeric(y_breaks_manual) || any(y_breaks_manual < 0))) {
    stop("The 'y_breaks_manual' must be a numeric vector with positive values.")
  }
  if (plot_type == "boxplot" && (!missing(angle_range) || !missing(y_labels_position) || !missing(y_breaks_manual))) {
    warning("Arguments 'angle_range', 'y_labels_position', and 'y_breaks_manual' are not used when 'plot_type' is set to 'boxplot'.")
  }

  ## Code ----

  # Extract trajectory parameters
  track_param <- track_param(data)
  data <- data[[1]]

  # Combine direction data from all tracks into a single data frame
  n <- c(track_param[[1]][[1]], track_param[[1]][[1]][[length(track_param[[1]][[1]])]])
  if (length(data) > 1) {
    for (i in 2:length(data)) {
      n <- c(n, c(track_param[[i]][[1]], track_param[[i]][[1]][[length(track_param[[i]][[1]])]]))
    }
  }

  M <- data.frame(dir = n, track = rep(names(data), sapply(data, nrow)))

  # Replace underscores with spaces in track names
  M$track <- gsub("_", " ", M$track)
  M_original <- M

  # Function to get y-axis breaks, considering manual input
  get_y_breaks <- function(max_value) {
    if (!is.null(y_breaks_manual)) {
      return(sort(unique(y_breaks_manual)))
    }

    # Automatically generate breaks with a minimum of three equidistant values
    n_breaks <- 3
    if (max_value == 0) {
      return(c(0, 1, 2))
    } else {
      return(pretty(c(0, max_value), n = n_breaks))
    }
  }

  # Generate plots based on plot_type
  if (plot_type == "boxplot") {
    p_boxplot <- ggplot(M, aes(x = track, y = dir)) +
      geom_boxplot(color = "black") +
      geom_point(alpha = 0.5, position = position_jitter(), color = "grey30") +
      theme_classic() +
      labs(y = "Direction (degrees)", x = "") +
      scale_y_continuous(limits = c(-180, 180), breaks = seq(-180, 180, by = 30)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    return(p_boxplot)
  }

  if (plot_type == "polar_steps") {
    # Cut the direction data into bins of angle_range and create a new column in M
    M$bin <- cut(M$dir, breaks = seq(-180, 180, by = angle_range), include.lowest = TRUE, right = FALSE)

    # Extract the bin intervals
    bin_intervals <- levels(M$bin)

    # Calculate midpoints for each bin
    bin_midpoints <- sapply(
      strsplit(gsub("\\[|\\)|\\]", "", bin_intervals), ","),
      function(x) mean(as.numeric(x))
    )

    # Map each bin to its midpoint
    M$bin_avg <- bin_midpoints[as.numeric(M$bin)]

    # Summarize data for the plot, now ignoring tracks and just counting steps per bin
    M_summary <- M %>%
      group_by(bin_avg) %>%
      summarise(count = n()) %>%
      ungroup()

    # Determine the maximum count to set the annotation values
    max_count <- max(M_summary$count, na.rm = TRUE)

    # Generate y-axis breaks
    y_breaks <- get_y_breaks(max_count)

    # Ensure y-axis limits accommodate the y-breaks
    y_limits <- c(0, max(y_breaks))

    # Create the polar plot
    p_polar_steps <- ggplot(M_summary, aes(x = bin_avg, y = count)) +
      geom_bar(stat = "identity", fill = "grey", color = "black", width = angle_range) +
      coord_polar(theta = "x", start = pi / 2, direction = -1) +
      scale_x_continuous(
        breaks = seq(-180, 180, by = angle_range),
        labels = seq(-180, 180, by = angle_range),
        limits = c(-180, 180)
      ) +
      scale_y_continuous(
        breaks = y_breaks, # Use the generated breaks for y-axis
        limits = y_limits # Ensure limits accommodate breaks
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()
      ) +
      labs(x = "Direction (degrees) vs Steps count") +
      annotate("text", x = rep(y_labels_position, length(y_breaks)), y = y_breaks, label = as.character(y_breaks), size = 3)

    return(p_polar_steps)
  }

  if (plot_type == "polar_average") {
    # Calculate average direction for each track
    M_avg <- M %>%
      group_by(track) %>%
      summarise(avg_dir = mean(dir, na.rm = TRUE)) %>%
      ungroup()

    # Cut the average directions into bins of angle_range
    M_avg$bin <- cut(M_avg$avg_dir, breaks = seq(-180, 180, by = angle_range), include.lowest = TRUE, right = FALSE)

    # Extract the bin intervals
    bin_intervals <- levels(M_avg$bin)

    # Calculate midpoints for each bin
    bin_midpoints <- sapply(
      strsplit(gsub("\\[|\\)|\\]", "", bin_intervals), ","),
      function(x) mean(as.numeric(x))
    )

    # Map each bin to its midpoint
    M_avg$bin_avg <- bin_midpoints[as.numeric(M_avg$bin)]

    # Summarize data for the plot
    M_avg_summary <- M_avg %>%
      group_by(bin_avg) %>%
      summarise(count = n()) %>%
      ungroup()

    # Determine the maximum count to set the annotation values
    max_count <- max(M_avg_summary$count, na.rm = TRUE)

    # Generate y-axis breaks
    y_breaks <- get_y_breaks(max_count)

    # Ensure y-axis limits accommodate the y-breaks
    y_limits <- c(0, max(y_breaks))

    # Create the polar plot
    p_polar_average <- ggplot(M_avg_summary, aes(x = bin_avg, y = count)) +
      geom_bar(stat = "identity", fill = "grey", color = "black", width = angle_range) +
      coord_polar(theta = "x", start = pi / 2, direction = -1) +
      scale_x_continuous(
        breaks = seq(-180, 180, by = angle_range),
        labels = seq(-180, 180, by = angle_range),
        limits = c(-180, 180)
      ) +
      scale_y_continuous(
        breaks = y_breaks, # Use the generated breaks for y-axis
        limits = y_limits # Ensure limits accommodate breaks
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()
      ) +
      labs(x = "Direction (degrees) vs Trackways count") +
      annotate("text", x = rep(y_labels_position, length(y_breaks)), y = y_breaks, label = as.character(y_breaks), size = 3)

    return(p_polar_average)
  }

  if (plot_type == "faceted") {
    # Cut the direction data into bins of angle_range and create a new column in M
    M$bin <- cut(M$dir, breaks = seq(-180, 180, by = angle_range), include.lowest = TRUE, right = FALSE)

    # Extract the bin intervals
    bin_intervals <- levels(M$bin)

    # Calculate midpoints for each bin, handling non-numeric cases
    bin_midpoints <- sapply(
      strsplit(gsub("\\[|\\)|\\]", "", bin_intervals), ","),
      function(x) mean(as.numeric(x), na.rm = TRUE)
    )

    # Map each bin to its midpoint
    M$bin_avg <- bin_midpoints[as.numeric(M$bin)]

    # Summarize data for the plot
    M_summary <- M %>%
      group_by(track, bin_avg) %>%
      summarise(count = n()) %>%
      ungroup()

    # Determine the maximum count to set the annotation values
    max_count <- max(M_summary$count, na.rm = TRUE)

    # Generate y-axis breaks
    y_breaks <- get_y_breaks(max_count)

    # Ensure y-axis limits accommodate the y-breaks
    y_limits <- c(0, max(y_breaks))

    # Create the faceted polar plot
    p_faceted <- ggplot(M_summary, aes(x = bin_avg, y = count)) +
      geom_bar(stat = "identity", fill = "grey", color = "black", width = angle_range) +
      coord_polar(theta = "x", start = pi / 2, direction = -1) +
      scale_x_continuous(
        breaks = seq(-180, 180, by = angle_range),
        labels = seq(-180, 180, by = angle_range),
        limits = c(-180, 180)
      ) +
      scale_y_continuous(
        breaks = y_breaks, # Use the generated breaks for y-axis
        limits = y_limits # Ensure limits accommodate breaks
      ) +
      facet_wrap(~track) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()
      ) +
      labs(x = "Direction (degrees) vs Trackways count")

    # Annotate the y-axis labels, this time handling multiple facets
    p_faceted <- p_faceted +
      geom_text(
        data = data.frame(
          y = rep(y_breaks, length(unique(M_summary$track))),
          x = rep(y_labels_position, length(y_breaks) * length(unique(M_summary$track))),
          label = rep(y_breaks, length(unique(M_summary$track))),
          track = rep(unique(M_summary$track), each = length(y_breaks))
        ),
        aes(x = x, y = y, label = label),
        size = 3,
        inherit.aes = FALSE
      )

    return(p_faceted)
  }
}
