#' Generates a polar plot with elliptical confidence intervals
#'
#' @param x An object of class \code{cglmm}
#' @param ci_level The level for calculated confidence ellipses.
#' Defaults to 0.95.
#' @param n_breaks The number of concentric circles that will be plotted using
#' the \code{scales::breaks_pretty()} function. By default, 5 breaks will be
#' used. The number of breaks may be adjusted to result in an even interval.
#' For example, if n_breaks is 3, but the maximum plot radius is 8, instead of
#' plotting circles in intervals in 1.6, this interval will be rounded to 2
#' to result in the sequence: 0, 2, 4, 6, 8. See \code{?scales::breaks_pretty}
#' for more details.
#' @param component_index A number that corresponds to a particular component
#' from the \code{cglmm()} object that will be used to create polar plot.
#' If missing (default), then plots for all components will be arranged in the
#' returned plot. If a single or multiple values are provided, then these
#' components will be returned. (for example \code{component_index = 1},
#' \code{component_index = c(1, 3)}).
#' @param grid_angle_segments An \code{integer}. Determines the total number of
#' segments in the background of the polar plot. For example, a value of 4 will
#' create quadrants around the origin. Defaults to 8.
#' @param radial_units A \code{character} specifying the angular units of the
#' plot. Possible values are one of \code{c('radians', 'degrees', 'period')}.
#' These units relate to the period of the component being visualized.
#' \describe{
#'   \item{\code{'radians'}: \eqn{[0, 2\pi]}}{}
#'   \item{\code{'degrees'}: \eqn{[0, 360]}}{}
#'   \item{\code{'period'}: \eqn{[0, period]}}{}
#'  }
#' @param clockwise A \code{logical}. If \code{TRUE}, the angles increase in a
#' clockwise fashion. If \code{FALSE}, anti-clockwise. Defaults to \code{FALSE}.
#' @param text_size A number controlling the font size of the text labels.
#' Defaults to 3.
#' @param text_opacity A \code{numeric} between 0 and 1 inclusive that
#' controls the opacity of the text labels.
#' @param fill_colors A \code{character} vector containing colors that will
#' be mapped to levels within a group. If the model has components with
#' different number of levels per factor, the length of this input should match
#' the greatest number of levels. If not, or if the number of levels exceeds the
#' length of the default argument (8), colors are generated using
#' \code{rainbow()}.
#' @param ellipse_opacity A \code{numeric} between 0 and 1 inclusive that
#' controls the opacity of the confidence ellipses. Defaults to 0.3.
#' @param circle_linetype A \code{character} or \code{numeric} that determines
#' the \code{linetype} of the radial circles in background of the polar plot.
#' See \code{?linetype}
#' for more details.
#' @param xlims A vector of length two containing the limits for the x-axis.
#' @param ylims A vector of length two containing the limits for the y-axis.
#' @param start A \code{character}, within
#' \code{c("right", "left", "top", "bottom")} that determines where angle 0 is
#' located. If \code{start = "top"}, and \code{clockwise = TRUE}, the angle
#' will rotate clockwise, starting at the '12 o-clock' position on a clock.
#' @param view A \code{character}, within
#' \code{c("full", "zoom", "zoom_origin")} that controls the view of the plots.
#' \describe{
#'   \item{\code{'full'}: maintains a full view of the polar plot, including
#'   the background radial circles.}{}
#'   \item{\code{'zoom'}: finds the minimum view window which contains all
#'   confidence ellipses.}{}
#'   \item{\code{'zoom_origin'}: zooms into the confidence ellipses (like
#'   "zoom"), but also keeps the origin within frame.}{}
#' }
#' @param overlay_parameter_info A \code{logical} argument. If \code{TRUE},
#' more information about the acrophase and amplitude are displayed on the
#' polar plots.
#' @param quietly Analogous to verbose, this \code{logical} argument controls
#' whether messages are displayed in the console.
#' @param show_component_labels Logical argument, TRUE by default. When TRUE,
#' the polar plots have labels corresponding to their components.
#' @param ... Additional, ignored arguments.
#'
#' @srrstats {G1.4}
#'
#' @return Returns a \code{ggplot} object.
#' @export
#'
#' @examples
#' data(vitamind)
#' model <- cglmm(
#'   vit_d ~ X + amp_acro(time, group = "X", period = 12),
#'   data = vitamind
#' )
#' polar_plot(model, radial_units = "period")
polar_plot <- function(x,
                       ci_level = 0.95,
                       n_breaks = 5,
                       component_index = NULL,
                       grid_angle_segments = 8,
                       radial_units = c("radians", "degrees", "period"),
                       clockwise = FALSE,
                       text_size = 3,
                       text_opacity = 0.5,
                       fill_colors,
                       ellipse_opacity = 0.3,
                       circle_linetype = "dotted",
                       start = c("right", "left", "top", "bottom"),
                       view = c("full", "zoom", "zoom_origin"),
                       overlay_parameter_info = FALSE,
                       quietly = TRUE,
                       show_component_labels = TRUE,
                       xlims,
                       ylims,
                       ...) {
  UseMethod("polar_plot")
}

#' Generates a polar plot with elliptical confidence intervals
#'
#' @param x An object of class \code{cglmm}
#' @param ci_level The level for calculated confidence ellipses.
#' Defaults to 0.95.
#' @param n_breaks The number of concentric circles that will be plotted using
#' the \code{scales::breaks_pretty()} function. By default, 5 breaks will be
#' used. The number of breaks may be adjusted to result in an even interval.
#' For example, if n_breaks is 3, but the maximum plot radius is 8, instead of
#' plotting circles in intervals in 1.6, this interval will be rounded to 2
#' to result in the sequence: 0, 2, 4, 6, 8. See \code{?scales::breaks_pretty}
#' for more details.
#' @param component_index A number that corresponds to a particular component
#' from the \code{cglmm()} object that will be used to create polar plot.
#' If missing (default), then plots for all components will be arranged in the
#' returned plot. If a single or multiple values are provided, then these
#' components will be returned. (for example \code{component_index = 1},
#' \code{component_index = c(1, 3)}).
#' @param grid_angle_segments An \code{integer}. Determines the total number of
#' segments in the background of the polar plot. For example, a value of 4 will
#' create quadrants around the origin. Defaults to 8.
#' @param radial_units A \code{character} specifying the angular units of the
#' plot. Possible values are one of \code{c('radians', 'degrees', 'period')}.
#' These units relate to the period of the component being visualized.
#' \describe{
#'   \item{\code{'radians'}: \eqn{[0, 2\pi]}}{}
#'   \item{\code{'degrees'}: \eqn{[0, 360]}}{}
#'   \item{\code{'period'}: \eqn{[0, period]}}{}
#'  }
#' @param clockwise A \code{logical}. If \code{TRUE}, the angles increase in a
#' clockwise fashion. If \code{FALSE}, anti-clockwise. Defaults to \code{FALSE}.
#' @param text_size A number controlling the font size of the text labels.
#' Defaults to 3.
#' @param text_opacity A \code{numeric} between 0 and 1 inclusive that
#' controls the opacity of the text labels.
#' @param fill_colors A \code{character} vector containing colors that will
#' be mapped to levels within a group. If the model has components with
#' different number of levels per factor, the length of this input should match
#' the greatest number of levels. If not, or if the number of levels exceeds the
#' length of the default argument (8), colors are generated using
#' \code{rainbow()}.
#' @param ellipse_opacity A \code{numeric} between 0 and 1 inclusive that
#' controls the opacity of the confidence ellipses. Defaults to 0.3.
#' @param circle_linetype A \code{character} or \code{numeric} that determines
#' the \code{linetype} of the radial circles in background of the polar plot.
#' See \code{?linetype}
#' for more details.
#' @param xlims A vector of length two containing the limits for the x-axis.
#' @param ylims A vector of length two containing the limits for the y-axis.
#' @param start A \code{character}, within
#' \code{c("right", "left", "top", "bottom")} that determines where angle 0 is
#' located. If \code{start = "top"}, and \code{clockwise = TRUE}, the angle
#' will rotate clockwise, starting at the '12 o-clock' position on a clock.
#' @param view A \code{character}, within
#' \code{c("full", "zoom", "zoom_origin")} that controls the view of the plots.
#' \describe{
#'   \item{\code{'full'}: maintains a full view of the polar plot, including
#'   the background radial circles.}{}
#'   \item{\code{'zoom'}: finds the minimum view window which contains all
#'   confidence ellipses.}{}
#'   \item{\code{'zoom_origin'}: zooms into the confidence ellipses (like
#'   "zoom"), but also keeps the origin within frame.}{}
#' }
#' @param overlay_parameter_info A \code{logical} argument. If \code{TRUE},
#' more information about the acrophase and amplitude are displayed on the
#' polar plots.
#' @param quietly Analogous to verbose, this \code{logical} argument controls
#' whether messages are displayed in the console.
#' @param show_component_labels Logical argument, TRUE by default. When TRUE,
#' the polar plots have labels corresponding to their components.
#' @param ... Additional, ignored arguments.
#'
#' @srrstats {G1.4}
#'
#' @return Returns a \code{ggplot} object.
#' @export
#'
#' @examples
#' model <- cglmm(
#'   vit_d ~ X + amp_acro(time, group = "X", period = 12),
#'   data = vitamind
#' )
#' polar_plot(model, radial_units = "period")
polar_plot.cglmm <- function(x,
                             ci_level = 0.95,
                             n_breaks = 5,
                             component_index = NULL,
                             grid_angle_segments = 8,
                             radial_units = c(
                               "radians",
                               "degrees",
                               "period"
                             ),
                             clockwise = FALSE,
                             text_size = 3.5,
                             text_opacity = 1,
                             fill_colors,
                             ellipse_opacity = 0.3,
                             circle_linetype = "dotted",
                             start = c(
                               "right",
                               "left",
                               "top",
                               "bottom"
                             ),
                             view = c(
                               "full",
                               "zoom",
                               "zoom_origin"
                             ),
                             overlay_parameter_info = FALSE,
                             quietly = TRUE,
                             show_component_labels = TRUE,
                             xlims,
                             ylims,
                             ...) {
  # checking the quality of inputs
  assertthat::assert_that(inherits(x, "cglmm"),
    msg = "'x' must be of class cglmm"
  )

  validate_ci_level(ci_level)
  radial_units <- match.arg(radial_units)
  start <- match.arg(start)
  view <- match.arg(view)


  assertthat::assert_that(
    grid_angle_segments == floor(grid_angle_segments) & grid_angle_segments > 0,
    msg = "'grid_angle_segments' must be an integer greater than 0"
  )
  assertthat::assert_that(is.logical(quietly),
    msg = "'quietly' must a logical argument, either TRUE or FALSE"
  )
  assertthat::assert_that(is.logical(clockwise),
    msg = "'clockwise' must be a logical argument, either TRUE or FALSE "
  )

  assertthat::assert_that(is.logical(show_component_labels),
    msg = paste(
      "'show_component_labels' must be a",
      " logical argument, either TRUE or FALSE"
    )
  )

  assertthat::assert_that(is.numeric(text_size) & text_size > 0,
    msg = "'text_size' must be a number greater than 0"
  )
  assertthat::assert_that(
    is.numeric(text_opacity) & text_opacity >= 0 & text_opacity <= 1,
    msg = "'text_opacity' must be a number between 0 and 1 inclusive"
  )
  assertthat::assert_that(
    is.numeric(ellipse_opacity) & ellipse_opacity >= 0 & ellipse_opacity <= 1,
    msg = "'ellipse_opacity' must be a number between 0 and 1 inclusive"
  )

  if (!missing(component_index)) {
    assertthat::assert_that(
      all(component_index == floor(component_index)) &
        all(component_index > 0) &
        all(component_index <= x$n_components),
      msg = paste(
        "'component_index' must be an integer between 1 and",
        "n_components (total number of components in model)",
        "inclusive"
      )
    )
  }

  if (!missing(xlims)) {
    assertthat::assert_that(
      length(xlims) == 2 & is.numeric(xlims) & xlims[1] < xlims[2],
      msg = paste(
        "'xlims' must be a vector with the first element being the",
        "lower x coordinate, and the second being the upper",
        "x coordinate"
      )
    )
    xlims_check <- TRUE
  } else {
    xlims_check <- FALSE
  }

  if (!missing(ylims)) {
    assertthat::assert_that(
      length(xlims) == 2 & is.numeric(ylims) & ylims[1] < ylims[2],
      msg = paste(
        "'ylims' must be a vector with the first element being the",
        "lower y coordinate, and the second being the upper",
        "y coordinate"
      )
    )
    ylims_check <- TRUE
  } else {
    ylims_check <- FALSE
  }

  assertthat::assert_that(is.character(circle_linetype) || is.numeric(circle_linetype),
    msg = paste(
      "'circle_linetype' must be a character or numeric. See ?linetype",
      "for more details"
    )
  )
  assertthat::assert_that(is.logical(overlay_parameter_info),
    msg = paste(
      "'overlay_parameter_info' must be a logical argument,",
      "either TRUE or FALSE"
    )
  )

  # get summary statistics of cglmm object
  sum <- summary(x, ci_level = ci_level)


  # convert user input for zoom level into logical arguments
  if (view == "full") {
    zoom <- FALSE
    zoom_origin <- FALSE
  } else if (view == "zoom_origin") {
    zoom <- TRUE
    zoom_origin <- TRUE
  } else if (view == "zoom") {
    zoom <- TRUE
    zoom_origin <- FALSE
  }

  # check if there is a contour argument & store this check in local environment
  n_components <- x$n_components

  fill_colors_check <- !missing(fill_colors)
  # set direction of increasing angle based on user input of clockwise argument
  direction <- ifelse(clockwise, -1, 1)

  # convert user input for starting position (on Cartesian plane) into logical
  # arguments by default, ggplot() ellipse and circle functions use unit circle
  # angles where 0 degrees starts at the '3pm' position and rotates
  # counterclockwise. However, the ggforce function geom_arc() starts at upwards
  # 12pm position, also rotating coutnerclockwise. Hence, offset, and
  # overlay_param_offset are different to ultimately describe the same
  # angle position
  if (start == "top") {
    offset <- pi / 2
    overlay_start <- 0
  } else if (start == "left") {
    offset <- pi
    overlay_start <- -pi / 2
  } else if (start == "bottom") {
    offset <- 3 * pi / 2
    overlay_start <- pi
  } else if (start == "right") {
    offset <- 0
    overlay_start <- pi / 2
  }

  if (!is.null(component_index)) {
    make_cowplot <- FALSE
  } else {
    make_cowplot <- TRUE
  }

  # remove component labels if there is only one component
  if (n_components == 1) {
    show_component_labels <- FALSE
  }

  # get ggplot for a single component. Function will then be looped for
  # multiple components
  sub_ggplot.cglmm.polar <- function(comp, ...) {
    # get the component that is going to plotted
    component_index <- comp
    # get the arguments from the function wrapping this function
    args <- match.call()[-1]
    period <- x$period[component_index]
    max_period <- period
    group_check <- (x$group[component_index] != 0)
    if (group_check) {
      x_str <- x$group_original[component_index]
      group <- x_str
      level <- x$group_stats[[group]]

      # create an index that will be used to grab the correct transformed
      # summary stats
      string_index <- paste0("[", group, "=")
      # create an index that grabs the corresponding raw summary stats
      string_index_raw <- paste0(group)
    } else {
      group <- NULL
      string_index <- ""
      string_index_raw <- ""
    }

    amp_index <- paste0("amp", component_index)
    acr_index <- paste0("acr", component_index)

    # grab and store the summary statistics for amp and acr
    amp_row_idx <- which(
      grepl(string_index, rownames(sum$transformed.table), fixed = TRUE) &
        grepl(amp_index, rownames(sum$transformed.table), fixed = TRUE)
    )

    est_amp <- sum$transformed.table$estimate[amp_row_idx]
    l_est_amp <- sum$transformed.table$lower.CI[amp_row_idx]
    u_est_amp <- sum$transformed.table$upper.CI[amp_row_idx]

    acr_row_idx <- which(
      grepl(string_index, rownames(sum$transformed.table), fixed = TRUE) &
        grepl(acr_index, rownames(sum$transformed.table), fixed = TRUE)
    )

    est_acr <- sum$transformed.table$estimate[acr_row_idx]
    l_est_acr <- sum$transformed.table$lower.CI[acr_row_idx]
    u_est_acr <- sum$transformed.table$upper.CI[acr_row_idx]

    # an index of the names of the summary statistics used in iteration of loop
    name_index <- rownames(sum$transformed.table)[amp_row_idx]
    group_level <- array(dim = length(name_index))

    # obtain an index of group levels
    if (group_check) {
      for (i in level) {
        group_ind <- paste0(group, "=", i)
        group_level[which(grepl(group_ind, name_index))] <- paste(i)
      }
    }


    # determine the estimated rrr and sss used parameter estimates
    est_rrr <- est_amp * cos(direction * (est_acr) + offset)
    est_sss <- est_amp * sin(direction * (est_acr) + offset)

    # for confidence ellipses, get the long_axis width "a_trans" (amplitude),
    # and the short-axis height "b_trans" (acrophase)
    # Note that both values are halved because they are technically radii
    a_trans <- est_amp - l_est_amp
    b_trans <- tan((u_est_acr - l_est_acr) / 2) * est_amp

    # determine the maximum radius in a single plot. This will be used
    # for formatting plot features
    max_radius <- max(abs(u_est_amp), abs(l_est_amp))


    # change 'max_period' to correspond to units specified by the user
    # conversion_factor is used to convert from radians (
    # default acrophase output) to whatever radial_units is
    if (radial_units == "radians") {
      max_period <- 2 * pi
      conversion_factor <- 1
    } else if (radial_units == "degrees") {
      max_period <- 360
      conversion_factor <- (1 / (2 * pi)) * 360
    } else if (radial_units == "period") {
      max_period <- max_period
      conversion_factor <- (1 / (2 * pi)) * max_period
    }

    # create a sequence of labels for time (to be inserted around
    # the polar plot)
    time_labels <- signif(
      seq(from = 0, to = max_period, by = max_period / grid_angle_segments), 3
    )


    # create a sequence of labels for the contours.
    contour_labels <- scales::breaks_pretty(n = n_breaks)(c(0, max_radius))


    # determine largest contour, and use this as a plot limit
    max_plot_radius <- max(contour_labels)

    # convert time_labels to polar coordinates to determine position of
    # where they should be placed
    dial_pos_full_x <- round(
      max_plot_radius * cos(
        direction * time_labels * 2 * pi / max_period + offset
      ),
      digits = 5
    )

    dial_pos_full_y <- round(
      max_plot_radius * sin(
        direction * time_labels * 2 * pi / max_period + offset
      ),
      digits = 5
    )

    # determining the bounds to plot if zoom = TRUE
    # designed to find the minimum plot window that contains
    # all confidence ellipses.
    if (zoom) {
      xmax_zoom <- max(est_rrr) + max(max(a_trans), max(b_trans))
      xmin_zoom <- min(est_rrr) - max(max(a_trans), max(b_trans))
      ymax_zoom <- max(est_sss) + max(max(a_trans), max(b_trans))
      ymin_zoom <- min(est_sss) - max(max(a_trans), max(b_trans))

      # if view = "zoom_origin", anchor the view window to the origin
      if (zoom_origin) {
        xmin_zoom <- min(xmin_zoom, 0)
        xmax_zoom <- max(xmax_zoom, 0)
        ymin_zoom <- min(ymin_zoom, 0)
        ymax_zoom <- max(ymax_zoom, 0)
      }


      # ensure that contour labels are always within the view window
      contour_x_zoom <- cos(direction * mean(est_acr) + offset) * contour_labels
      contour_y_zoom <- sin(direction * mean(est_acr) + offset) * contour_labels
    }

    # adding special symbols to time_labels (π for radians, ° for degrees )
    if (radial_units == "radians") {
      pi_string <- paste(round(time_labels / pi, 1))
      time_labels <- paste0(pi_string, "\U03C0")
      acr_overlay <- paste0(
        signif(conversion_factor * est_acr / pi, 2),
        "\U03C0"
      )
    } else if (radial_units == "degrees") {
      time_labels <- paste0(time_labels, "\U00B0")
      acr_overlay <- paste0(
        signif(conversion_factor * est_acr, 2),
        "\U00B0"
      )
    } else if (radial_units == "period") {
      acr_overlay <- paste0(
        signif(conversion_factor * est_acr, 2)
      )
    }


    # create the main plot object
    if (is.na(x$group_original[component_index])) {
      group_level <- NULL
      group_level_colour_index <- 1
    } else {
      group_level_colour_index <- length(group_level)
    }

    # generates the background grid for the polar plot
    get_background_grid <- function(n_breaks,
                                    max_plot_radius,
                                    circle_linetype,
                                    dial_pos_full_x,
                                    dial_pos_full_y,
                                    time_labels,
                                    text_size,
                                    text_opacity,
                                    contour_labels,
                                    grid_angle_segments) {
      plot_background <- ggplot2::ggplot() +
        ggforce::geom_circle( # plots the background circles
          ggplot2::aes(
            x0 = 0,
            y0 = 0,
            r = scales::breaks_pretty(n = n_breaks)(c(0, max_plot_radius))
          ),
          alpha = 0.01,
          linetype = circle_linetype
        ) +
        ggplot2::geom_segment( # plots the background grid
          ggplot2::aes(
            x = dial_pos_full_x,
            y = dial_pos_full_y,
            xend = -dial_pos_full_x,
            yend = -dial_pos_full_y
          ),
          linetype = 10,
          alpha = 0.4
        ) +
        ggplot2::geom_text( # adds the radial labels (amplitude)
          ggplot2::aes(label = time_labels[-length(time_labels)]),
          x = 1.05 * dial_pos_full_x[-length(dial_pos_full_x)],
          y = 1.05 * dial_pos_full_y[-length(dial_pos_full_y)],
          size = text_size,
          alpha = text_opacity
        ) +
        ggplot2::geom_text( # adds the angle labels (acrophase)
          ggplot2::aes(
            label = contour_labels,
            x = contour_labels * (cos(pi / grid_angle_segments)),
            y = contour_labels * (sin(pi / grid_angle_segments))
          ),
          size = text_size, alpha = text_opacity
        ) +
        ggplot2::guides(colour = "none") +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )

      return(plot_background)
    }

    # get the plot background
    plot_background <- get_background_grid(
      n_breaks,
      max_plot_radius,
      circle_linetype,
      dial_pos_full_x,
      dial_pos_full_y,
      time_labels,
      text_size,
      text_opacity,
      contour_labels,
      grid_angle_segments
    )

    # generates the point estimates and confidence ellipses. If you wish to add
    # multiple estimates and ellipses on the same plot, run this function with
    # the appropriate inputs, with 'plot_background' being set to the plot
    # you wish to layer upon.
    get_point_estimate_plot <- function(est_rrr, # the rrr estimate
                                        est_sss, # the sss estimate
                                        a_trans, # ellipse long dimension
                                        b_trans, # ellipse short dimension
                                        offset, # determines where angle starts
                                        direction, #-1 for clockwise, 1 for anti
                                        est_acr, # acrophase estimate
                                        group_level, # a vector of group levels
                                        ellipse_opacity, # 0 to 1, alpha value
                                        plot_background # the plot to layer upon
    ) {
      if (group_check) {
        plot_estimate <- plot_background + ggforce::geom_ellipse( # plots the confidence ellipse
          ggplot2::aes(
            x0 = est_rrr,
            y0 = est_sss,
            a = a_trans,
            b = b_trans,
            angle = offset + direction * est_acr,
            fill = group_level,
            colour = group_level
          ),
          alpha = ellipse_opacity
        ) +
          ggplot2::geom_point( # plots the parameter estimates
            ggplot2::aes(x = est_rrr, y = est_sss)
          )
      } else {
        plot_estimate <- plot_background + ggforce::geom_ellipse( # plots the confidence ellipse
          ggplot2::aes(
            x0 = est_rrr,
            y0 = est_sss,
            a = a_trans,
            b = b_trans,
            angle = offset + direction * est_acr,
            fill = grDevices::rainbow(group_level_colour_index),
            colour = grDevices::rainbow(group_level_colour_index)
          ),
          alpha = ellipse_opacity
        ) + ggplot2::theme(legend.position = "none") +
          ggplot2::geom_point( # plots the parameter estimates
            ggplot2::aes(x = est_rrr, y = est_sss)
          )
      }
      return(plot_estimate)
    }
    plot_obj <- get_point_estimate_plot(
      est_rrr,
      est_sss,
      a_trans,
      b_trans,
      offset,
      direction,
      est_acr,
      group_level,
      ellipse_opacity,
      plot_background
    )

    if (x$group_check) {
      plot_obj <- plot_obj + ggplot2::labs(fill = x_str, colour = NULL)
    }
    # OPTIONAL: overlays lines connecting the parameter estimates to the
    # origin, and displays estimates in plot
    if (overlay_parameter_info) {
      radius_sequence <- seq(
        0.65 * min(l_est_amp),
        0.80 * min(l_est_amp),
        length.out = length(est_amp)
      )
      overlay_labels <- paste(
        paste0("A = ", signif(est_amp, 2)),
        paste0("\U03D5 = ", acr_overlay),
        sep = "\n"
      )
      plot_obj <- plot_obj +
        ggplot2::geom_segment(
          ggplot2::aes(
            x = 0,
            y = 0,
            xend = est_rrr,
            yend = est_sss,
            colour = group_level
          )
        ) +
        ggforce::geom_arc(
          ggplot2::aes(
            x0 = 0,
            y0 = 0,
            r = radius_sequence,
            start = overlay_start,
            end = (overlay_start - direction * est_acr),
            colour = group_level
          )
        ) +
        ggplot2::geom_text(
          ggplot2::aes(
            label = overlay_labels, est_rrr, y = est_sss
          ),
          size = text_size,
          alpha = text_opacity
        )
    }


    # apply colors chosen by user input to the fill and colour aesthetics
    if (fill_colors_check) {
      plot_obj <- plot_obj +
        ggplot2::scale_fill_manual(
          values = fill_colors,
          aesthetics = c("fill", "colour")
        )
    } else {
      plot_obj <- plot_obj +
        ggplot2::scale_fill_manual(
          values = grDevices::rainbow(group_level_colour_index),
          aesthetics = c("fill", "colour")
        )
    }

    # if the view argument is 'zoom', or 'zoom_origin', apply
    # transformed view_limits
    if (zoom) {
      plot_obj <- plot_obj +
        ggplot2::geom_text(
          ggplot2::aes(
            label = contour_labels,
            x = contour_x_zoom,
            y = contour_y_zoom
          ),
          size = text_size,
          alpha = text_opacity
        )
      plot_obj <- plot_obj +
        ggplot2::coord_equal(
          xlim = c(xmin_zoom, xmax_zoom),
          ylim = c(ymin_zoom, ymax_zoom)
        )
    } else {
      # plot full polar plot if view = "full"
      plot_obj <- plot_obj + ggplot2::coord_fixed()
      # if an xlims argument was passed, set the xlims accordingly
      if (xlims_check && !ylims_check) {
        plot_obj <- plot_obj + ggplot2::coord_fixed(xlim = c(xlims[1], xlims[2]))
      }

      # if a ylims argument was passed, set the xlims accordingly
      if (ylims_check && !xlims_check) {
        plot_obj <- plot_obj + ggplot2::coord_fixed(ylim = c(ylims[1], ylims[2]))
      }

      # if both xlims and ylims are passed, set coordinate accordingly
      if (ylims_check && xlims_check) {
        plot_obj <- plot_obj + ggplot2::coord_fixed(
          xlim = c(xlims[1], xlims[2]),
          ylim = c(ylims[1], ylims[2])
        )
      }
    }

    # OPTIONAL: print information about the polar grid
    if (!quietly & length(contour_labels) > 1) {
      message(
        "Concentric circles every ",
        contour_labels[2] - contour_labels[1],
        " unit(s)"
      )
      message("Angle in units of ", radial_units)
    }

    # return the plot object
    plot_obj
  }


  # plot multiple component plots in cowplot or plot a single component plot
  if (make_cowplot == TRUE & n_components > 1) {
    plot_list <- NULL
    for (i in seq_len(n_components)) {
      plot_obj <- sub_ggplot.cglmm.polar(i)
      assign(paste0("plot_obj", i), plot_obj)

      # show labels for each component
      if (show_component_labels) {
        plot_obj <- plot_obj + ggplot2::ggtitle(paste("Component", i))
      }

      plot_list[[i]] <- ggplot2::ggplotGrob(plot_obj)
    }

    final_obj <- cowplot::plot_grid(
      plotlist = plot_list
    )
    final_obj
  }
  if (make_cowplot == TRUE & n_components == 1) {
    plot_list <- NULL
    for (i in seq_len(n_components)) {
      plot_obj <- sub_ggplot.cglmm.polar(i)
      assign(paste0("plot_obj", i), plot_obj)
      # show labels for each component
      if (show_component_labels) {
        plot_obj <- plot_obj + ggplot2::ggtitle(paste("Component", i))
      }
      plot_list[[i]] <- ggplot2::ggplotGrob(plot_obj)
    }
    final_obj <- cowplot::plot_grid(
      plotlist = plot_list
    )
    final_obj
  }
  if (make_cowplot == FALSE) {
    if (length(component_index) == 1) {
      plot_obj <- sub_ggplot.cglmm.polar(component_index)

      # show labels
      if (show_component_labels) {
        final_obj <- plot_obj + ggplot2::ggtitle(paste("Component", component_index))
      } else {
        final_obj <- plot_obj
      }
      final_obj
    } else {
      plot_list <- NULL
      for (i in component_index) {
        plot_obj <- sub_ggplot.cglmm.polar(i)
        assign(paste0("plot_obj", i), plot_obj)
        # show labels for each component
        if (show_component_labels) {
          plot_obj <- plot_obj + ggplot2::ggtitle(paste("Component", i))
        }

        plot_list[[i]] <- ggplot2::ggplotGrob(plot_obj)
      }

      final_obj <- cowplot::plot_grid(
        plotlist = plot_list,
        labels = NULL
      )

      final_obj
    }
  }

  return(final_obj)
}
