# classify — aboveR
# Mining/environmental point classification helpers (internal)

#' Classify Terrain Change Categories
#'
#' Internal helper to assign descriptive labels to change magnitudes.
#'
#' @param values Numeric vector of change values.
#' @param breaks Numeric vector of breakpoints for classification.
#' @param labels Character vector of labels (length = length(breaks) - 1).
#' @returns Character vector of class labels.
#' @noRd
classify_change <- function(values,
                            breaks = c(-Inf, -2, -0.5, 0.5, 2, Inf),
                            labels = c("major_cut", "minor_cut", "stable",
                                       "minor_fill", "major_fill")) {
  cut(values, breaks = breaks, labels = labels, include.lowest = TRUE)
}

#' Classify Slope into Categories
#'
#' @param slope_raster A [terra::SpatRaster] of slope values in degrees.
#' @param breaks Numeric vector of degree breakpoints.
#' @param labels Character labels.
#' @returns A classified [terra::SpatRaster].
#' @noRd
classify_slope <- function(slope_raster,
                           breaks = c(0, 5, 15, 30, 60, 90),
                           labels = c("flat", "gentle", "moderate",
                                      "steep", "very_steep")) {
  rcl <- cbind(
    breaks[-length(breaks)],
    breaks[-1],
    seq_along(labels)
  )
  cls <- terra::classify(slope_raster, rcl = rcl, include.lowest = TRUE)
  levels(cls) <- data.frame(id = seq_along(labels), label = labels)
  cls
}
