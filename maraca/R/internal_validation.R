
.create_validation_tte <- function(layers, x, arms) {

  tte_layers <- which(layers == "GeomStep")

  if (length(tte_layers) != 0) {
    tte_data <-
      do.call("rbind",
              lapply(tte_layers,
                     function(i) {
                       dat <- ggplot2::layer_data(plot = x,
                                                  i = i)[, c("x", "y",
                                                             "group")]
                       dat <- utils::head(dat, -2)
                       if (i == tte_layers[1]) {
                         dat <- utils::tail(dat, -2)
                       }
                       return(dat)
                     }))

    tte_data$group <- factor(tte_data$group, labels = arms)

  } else {
    tte_data <- NULL
  }

  return(tte_data)
}

.create_validation_binary_step <- function(layers, x, arms) {

  `%>%` <- dplyr::`%>%`

  binary_layers <- which(layers == "GeomSegment")

  if (length(binary_layers) != 0) {
    binary_step_data <-
      do.call("rbind",
              lapply(binary_layers,
                     function(i) {
                       dat <- ggplot2::layer_data(plot = x,
                                                  i = i)[, c("x", "y",
                                                             "yend",
                                                             "group",
                                                             "linetype")]
                       return(dat)
                     }))

    binary_step_data <- binary_step_data %>%
      dplyr::filter(linetype == 2) %>%
      dplyr::mutate(proportion = yend - y) %>%
      dplyr::select(x, y, proportion, group)

    binary_step_data$group <- factor(binary_step_data$group, labels = arms)

  } else {
    binary_step_data <- NULL
  }

  return(binary_step_data)
}

.create_validation_binary_last <- function(layers, x, arms) {

  `%>%` <- dplyr::`%>%`

  polygon_layers <- which(layers == "GeomPolygon")
  point_layers <- which(layers == "GeomPoint")

  if (length(polygon_layers) == 1 &&
        length(point_layers) == 1) {

    point_data <- ggplot2::layer_data(x, point_layers) %>%
      dplyr::select(x, y, group)

    polygon_data <- unique(ggplot2::layer_data(x, polygon_layers))
    polygon_data <- polygon_data %>%
      dplyr::filter(y %in% point_data$y) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise("lower_ci" = base::min(x, na.rm = TRUE),
                       "upper_ci" = base::max(x, na.rm = TRUE))

    binary_data <- dplyr::left_join(point_data, polygon_data,
                                    by = "group")
    binary_data$group <- factor(binary_data$group, labels = arms)

  } else {

    binary_data <- NULL

  }

  return(binary_data)
}



.create_validation_scatter <- function(layers, x, arms) {
  scatter_data <- do.call("rbind", lapply(which(layers == "GeomPoint"),
                                          ggplot2::layer_data, plot = x))
  if (!is.null(scatter_data) && nrow(scatter_data) > 2) {
    scatter_data <- scatter_data[, c("group", "x", "y")]
    scatter_data$group <- factor(scatter_data$group, labels = arms)
  }

  return(scatter_data)
}

.create_validation_violin <- function(layers, x, arms) {
  violin_data <- do.call("rbind", lapply(which(layers == "GeomViolin"),
                                         ggplot2::layer_data, plot = x))
  if (!is.null(violin_data)) {
    violin_data <- violin_data[, c("group", "x", "y", "density", "width")]
    violin_data$group <- factor(violin_data$group, labels = arms)
  }

  return(violin_data)
}

.create_validation_box <- function(layers, x, arms) {

  `%>%` <- dplyr::`%>%`

  boxstat_data <- do.call("rbind", lapply(which(layers == "GeomBoxplot"),
                                          ggplot2::layer_data, plot = x))

  if (!is.null(boxstat_data)) {
    boxstat_data <- boxstat_data %>%
      dplyr::select(group, "x_lowest" = xmin_final,
                    "whisker_lower" = xmin,
                    "hinge_lower" = xlower, "median" = xmiddle,
                    "hinge_upper" = xupper, "whisker_upper" = xmax,
                    "x_highest" = xmax_final, outliers)
    boxstat_data$outliers <- lapply(boxstat_data$outliers, sort)
    boxstat_data$group <- factor(boxstat_data$group, labels = arms)
  }

  return(boxstat_data)
}
