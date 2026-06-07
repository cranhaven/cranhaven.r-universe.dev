#' @title Common Parameters
#' @param df The dataframe containing the variable you wish to plot.
#' @param value.name The name of the variable you wish to plot.
#' @param num_colors The number of colors you want in your graph when plotting
#'   continuous data. If num_colors > 1, the variable in question will be
#'   divided into quantiles and converted into a factor with that many levels.
#'   If num_colors = 1, a continuous color gradient will be used; if num_colors
#'   = 0, a diverging color gradient will be used (useful for visualizing
#'   negative and positive numbers).  Use color.max and color.min to control the
#'   range of colors displayed. num_colors is ignored when plotting categorical
#'   data.
#' @param color.max The color of the highest value in your data. Ignored if the
#'   plotted variable is categorical.
#' @param color.min The color of the lowest value in your data. Ignored if the
#'   plotted variable is categorical.
#' @param na.color The color you want to assign for regions with missing data
#' @param nbreaks The number of breaks you wish to show in the legend when using
#'   a continuous color scale. Ignored if num_colors > 1.
#' @param custom.colors A vector of valid R color terms of the to use for the
#'   map when plotting factor variables. The length of this vector must match
#'   the number of levels in your factor variable, or num_colors for a
#'   continuous variable that will be discretized by the function, and the order
#'   should match the order of the levels in your factor variable.
#' @param projection One of the following: "cartesian", "mercator", "robinson",
#'   or "albers", for the equirectangular, Mercator, Robinson, and Albers Equal
#'   Area projections, respectively. When using the Mercator projection for
#'   world maps, setting limits_lon is recommended to prevent exaggeration of
#'   the size of Antarctica.
#' @param limits_lat A length two vector giving the minimum and maximum latitude
#'   you wish to include in your map.
#' @param limits_lon A length two vector giving the minimum and maximum
#'   longitude you wish to include in your map.
#' @param border_color The color of the borders on your map
#' @param border_thickness The thickness of the borders on your map
#' @param background_color The background color of your map
#' @param gridlines Should gridlines appear on your map?
#' @param latlon_ticks Should lat/lon tick marks appear on the edge of your map?
#' @param whitespace Add some blank space to the sides of your map? For some
#'   projections, this must be set to FALSE in order for lat/lon ticks and
#'   display correctly.
#' @param label The name of variable you wish to use to label your map; must be
#'   one of the variables that appears in the spatial dataframe just prior
#'   plotting (use return = 'sf' to see this dataframe), and in general, can be
#'   any of the allowed geoid.type. This function uses ggplot2::geom_label_repel
#'   to create the labels and ensure that they do not overlap.
#' @param label_text_size The size of the text that will appear in each label
#' @param label_text_color The color of the text that will appear in each label
#' @param label_box_color The color of the box around each label
#' @param ggrepel_options A list containing additional arguments to be passed to
#'   geom_label_repel (see ?ggplot2::geom_label_repel)
#' @param legend A title for your legend; if NULL, value.name will be used.
#' @param legend_position The position of your legend relative to the rest of
#'   the map; can be "top", "bottom", "left", or "right".
#' @param title A title for your plot; if NULL, no title will be added.
#' @param return If "plot", the function will return the requested map as a
#'   ggplot object. If "sf", the function will return the spatial dataframe used
#'   to draw the map (useful if you wish to customize the map yourself).
#' @keywords internal

common_args <- function(df, geoid.name, geoid.type, value.name, num_colors,
                        color.max, color.min, na.color, nbreaks, custom.colors,
                        projection, limits_lat, limits_lon, reproject,
                        border_color, border_thickness, background_color, gridlines, latlon_ticks,
                        label, label_text_size, label_text_color, label_box_color,
                        ggrepel_options, whitespace, legend_position,
                        legend, legend.position, title, return) {}

