#' Plot a Cofeature Matrix
#'
#' Generates a ggplot2::geom_tile plot of features by sample.
#' It is able to deal with multiple types affecting the same sample.
#'
#' @param in.df A 3 column (feature, sampleID, type) data.frame object
#' @param feature.order character vector indicating the order of the features 
#'   in the final plot on the y-axis. If not set, then function will set it 
#'   automatically
#' @param sample.id.order character vector indicating the order of the samples 
#'   in the final plot on the x-axis. If not set, then function will set it 
#'   automatically
#' @param fill.colors character vector indicating the colors of the different 
#'   "types". The names should be the types with the value being the color
#' @param type.display.mode Specify whether multiple or a single feature type 
#'   can appear in the same feature/sample cell
#' @param type.order Specify the "priority" of the feature types. This only 
#'   has an effect when type.display.mode is set to single.
#' @param tile.col Border color of each cell. If not set, no border color is 
#'   used.
#' @param rotate.x.labels Rotate the x-axes labels by a certain degree
#' @param missing.fill.col Color of the cell that has missing values
#' @param dot.flag Boolean to turn on/off dots (dot.flag)
#' @param dot.size Column name indicating the size of the dots. Only takes
#'   effect if dot.flag is TRUE.
#' @param tile.flag Boolean to turn on/off tiles (tile.flag)
#' @param tile.border.size Integer to indicate the size of the tile borders. 
#' @param drop.x Boolean to drop levels (from a factor) in the x dimension.
#' @export
#' @examples
#' \dontrun{
#' v1 <- c("RCOR1", "NCOR1", "LCOR", "RCOR1", "RCOR1", "RCOR1", "RCOR1")
#' v2 <- c("sampleA", "sampleC", "sampleB", "sampleC", "sampleA", "sampleC", "sampleC")
#' v3 <- c("Deletion", "Deletion", "SNV", "Rearrangement", "SNV", "Rearrangement", "SNV")
#' v4 <- c(0.05, 0.5, 0.25, 0.01, 0.03, 0.24, 0.89)
#' v5 <- c(1, 2, 1, 1, 2, 2, 1)
#' feature.order <- c("RCOR1", "NCOR1", "LCOR")
#' sample.id.order <- c("sampleA", "sampleB", "sampleC")
#' in.df <- dplyr::data_frame(feature = v1, sampleID = v2, type = v3, 
#'   p_value = -log10(v4), dir_flag = v5)
#' fill.colors <- c("Deletion" = "Blue", "Rearrangement" = "Green", "SNV" = "Red")
#'  
#' plot_cofeature_mat(in.df)
#' 
#' # With black tile color
#' plot_cofeature_mat(in.df, tile.col = "black")
#'
#' # Fill in missing values with a lightgrey color
#' plot_cofeature_mat(in.df, tile.col = "black", missing.fill.col = "lightgrey")
#'
#' # Rotate x-axes labels by 90 degrees
#' plot_cofeature_mat(in.df, rotate.x.labels = 90)
#' 
#' # Specify order of features, samples, and colors
#' plot_cofeature_mat(in.df, feature.order, sample.id.order, 
#'    fill.colors = fill.colors)
#'
#' # Specify each cell can only have one "feature type"
#' plot_cofeature_mat(in.df, feature.order, sample.id.order, fill.colors = fill.colors,
#'   type.display.mode = "single")
#'
#' # Specify the specific priority of the "feature type" for cells with
#' # multiple features
#' plot_cofeature_mat(in.df, feature.order, sample.id.order, fill.colors = fill.colors,
#'   type.display.mode = "single", type.order = c("Rearrangement", "SNV", "Deletion"))
#'
#' # Add dots to tiles for an additional layer of information
#' plot_cofeature_mat(in.df, dot.size = "p_value")
#' 
#' # Only display dots
#' plot_cofeature_mat(in.df, dot.flag = TRUE, dot.size = "p_value", 
#'   tile.flag = FALSE)
#'
#' # Samples will not be dropped
#' sample.id.order.new <- c("sampleA", "sampleB", "sampleC", "sampleD")
#' plot_cofeature_mat(in.df, tile.col = "black", 
#'   sample.id.order = sample.id.order.new)
#'
#' # Samples can be dropped by setting drop.x = TRUE
#' plot_cofeature_mat(in.df, tile.col = "black", 
#'   sample.id.order = sample.id.order.new, drop.x = TRUE)
#' }
plot_cofeature_mat <- function(
  in.df, 
  feature.order, sample.id.order, 
  fill.colors,
  type.display.mode = c("multiple", "single"), type.order, 
  tile.col = NA, 
  rotate.x.labels, missing.fill.col, 
  dot.flag = FALSE, dot.size, 
  tile.flag = TRUE, 
  drop.x = FALSE,
  tile.border.size = 1
) {

  if (!"feature" %in% colnames(in.df)) {
    stop("Missing required column name feature in input data.frame")
  }

  if (!missing(missing.fill.col)) {
    message("Detected missing.fill.col parameter")
    in.df.grid <- expand.grid(feature = unique(in.df[["feature"]]),
                              sampleID = unique(in.df[["sampleID"]]),
                              stringsAsFactors = FALSE)

    in.df <- dplyr::right_join(in.df, in.df.grid)
  }

  type.display.mode <- match.arg(type.display.mode)

  # arg parameter cannot be a numeric vector
  if (missing(rotate.x.labels)) {
    rotate.x.labels <- 0
  } else if (!rotate.x.labels %in% c(45, 90)) {
      stop("rotate.x.labels must be either 45 or 90")
  }

  if (missing(feature.order)) {
    message("Detected no feature.order. Specifying feature.order")
    feature.order <- unique(in.df[["feature"]])
  } 

  if (length(unique(feature.order)) != length(feature.order)) {
    warning("There may be duplicates in your feature.order. This may cause issues. ")
  }
  
  feature.order <- rev(feature.order)

  if (missing(sample.id.order)) {
    message("Detected no sample.id.order. Specifying sample.id.order")
    sample.id.order <- unique(in.df[["sampleID"]])
  } else {
    missing.samples <- setdiff(unique(in.df[["sampleID"]]), sample.id.order)
    if (length(missing.samples) > 0) {
      warning(paste("sampleID in in.df not found in sample.id.order:", 
                    paste(missing.samples, collapse = ", ")))
    }
  }

  if (missing(type.order)) {
    message("Detected no type.order. Specifying type.order")
    type.order <- unique(in.df[["type"]])
  }

  if (type.display.mode == "single") {
    message("Using type.display.mode single")
    in.df <- dplyr::distinct_(in.df)

    mutate.call <- lazyeval::interp(~ factor(type, levels = rev(type.order)), 
                                    type = as.name("type"),
                                    type.order = as.name("type.order"))

    in.df <- dplyr::mutate_(in.df, 
                            .dots = setNames(list(mutate.call), "type"))

    in.df <- dplyr::group_by_(in.df, .dots = c("sampleID", "feature"))
    in.df <- dplyr::arrange_(in.df, .dots = c("type"))
    in.df <- dplyr::top_n(in.df, n = 1)
    in.df <- dplyr::ungroup(in.df)
  }

  message("Setting feature order")
  mutate.call <- lazyeval::interp(~ as.numeric(
                                      factor(feature, 
                                             levels = feature.order)),
                                  feature = as.name("feature"))
  in.df <- dplyr::mutate_(in.df, 
                          .dots = setNames(list(mutate.call), "feature"))

  # Set sample order
  message("Setting sample order")
  mutate.call <- lazyeval::interp(~ factor(sampleID, 
                                           levels = sample.id.order),
                                  sampleID = as.name("sampleID"))
  in.df <- dplyr::mutate_(in.df, 
                          .dots = setNames(list(mutate.call), "sampleID"))

  # Calculate shift
  in.df <- dplyr::group_by_(in.df, .dots = c("feature", "sampleID"))
  mutate.call <- lazyeval::interp(~ (1:n())/n() - 
                                  1/(2 * n()) - 1/2)
  in.df <- dplyr::mutate_(in.df, 
                          .dots = setNames(list(mutate.call), "shift"))

  # Calculate height
  mutate.call <- lazyeval::interp(~ 1/n())
  in.df <- dplyr::mutate_(in.df, 
                          .dots = setNames(list(mutate.call), "height"))

  # Calculate feature_shift
  mutate.call <- lazyeval::interp(~ feature + shift, 
                                  feature = as.name("feature"),
                                  shift = as.name("shift"))
  in.df <- dplyr::mutate_(in.df, 
                          .dots = setNames(list(mutate.call), "feature_shift"))

  p1 <- ggplot2::ggplot(in.df, 
                        ggplot2::aes_string(x = "sampleID", 
                          y = "feature_shift", 
                          height = "height",
                          fill = "type")) +
    ggplot2::scale_x_discrete(drop = drop.x) +
    ggplot2::scale_y_discrete(limits = 1:length(feature.order), 
                              labels = feature.order) +
    ggplot2::ylab("Feature") +
    ggplot2::xlab("Sample ID")

  if (!missing(fill.colors)) {
      p1 <- p1 +
        ggplot2::scale_fill_manual(values = fill.colors)
  }

  if (missing(missing.fill.col)) {
    if (tile.flag) {
      p1 <- p1 +
        ggplot2::geom_tile(color = tile.col, size = 1)
    }
  } else {
    p1 <- add_tiles(p1, in.df, tile.col, missing.fill.col, tile.border.size)
  }

  if (rotate.x.labels == 90) {
    p1 <- p1 +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, 
                                                         hjust = 1, 
                                                         vjust = 0.5))
  } else if (rotate.x.labels == 45) {
    p1 <- p1 +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                                         hjust = 1, 
                                                         vjust = 1))
  }

  if (dot.flag) {
    if (!missing(dot.size)) {
      p1 <- p1 +
        ggplot2::geom_point(ggplot2::aes_string(size = dot.size))
    } else {
      p1 <- p1 +
        ggplot2::geom_point()
    }
  }
  p1
}

#' Add tiles to the ggplot2 
#'
#' @param p1 Existing ggplot2
#' @inheritParams plot_cofeature_mat
add_tiles <- function(p1, in.df, tile.col, missing.fill.col, tile.border.size) {

  # Plot two geom_tile. 1 for data present and 1 for data missing
  filter.crit.1 <- lazyeval::interp(~ !is.na(type), 
                                    .values = list(type = as.name("type")))
  filter.crit.2 <- lazyeval::interp(~ is.na(type), 
                                    .values = list(type = as.name("type")))

  # No borders for missing data.
  p1 <- 
    p1 +
    ggplot2::geom_tile(
      data = dplyr::filter_(in.df, filter.crit.1), 
      color = tile.col, size = tile.border.size
    ) +
    ggplot2::geom_tile(
      data = dplyr::filter_(in.df, filter.crit.2), 
      fill = missing.fill.col, color = tile.col
    )

  p1
}
