#' Change default plotting behavior
#'
#' This function alternates the default graphics behavior (both, base R graphics
#' and ggplot2 graphics) to follow Profinit visual guide.
#' Be were, the recent setup is not stored anywhere before changing the values.
#'
#' @param pal_name Profinit palette name to be used. Defaults to `blue-red`.
#' @param pal_name_discrete (Optional). Palette name for discrete color scales to be used by `ggplot`. Defaults to `discrete`.
#' @param exact Indicates whether discrete values will be treated exactly as present in the palette OR
#'  whether and interpolation can take a place.
#' @export
#' @returns The function returns nothing, it edits default behavior. Recent default configs are stored in a backup option.
#' @describeIn set_theme Sets default plotting theme to Profinit.
#' @examples
#' # Example 1 - BaseR
#'
#' # As a starting point, there is a plot i base R graphic:
#' sample_df <- data.frame(x=1:8, y=1:8, category=as.character(1:8), col_cont = 1:8)
#' barplot(sample_df$y, col=sample_df$category)
#'
#' # Now, by applying hte set_theme() we can change the default behavior:
#' profiplots::set_theme("blue-red", "blue-red")
#' barplot(sample_df$y, col=sample_df$category)
#'
#' # To turn the theming off, just call:
#' profiplots::unset_theme()
#'
#' # Example 2 - GGplot
#' library("ggplot2")
#' plot_gg <- ggplot(sample_df, aes(x=x, y=y, fill=category)) + stat_identity(geom="bar")
#' plot_gg
#'
#' # Now, let's trun it into Profinit graphics.
#' profiplots::set_theme("blue-red", "blue-red")
#' plot_gg
#'
#' # To remove the Profinit theme from defaults, just call:
#' profiplots::unset_theme()
#'
#'
set_theme <- function(pal_name = "blue-red", pal_name_discrete = "discrete", exact = NULL) {

  if (is.null(pal_name_discrete)) pal_name_discrete <- pal_name

  # set base R palette to profinit palette
  n_colors <- 8
  grDevices::palette(value = profinit_pal(pal_name_discrete)(n_colors))

  # redefine default ggplot color palettes
  .backup_default_scales <- options(
    ggplot2.continuous.colour = function(...) scale_color_profinit(palette = pal_name, discrete = FALSE, reverse = TRUE, ...),
    ggplot2.discrete.colour = function(...) scale_color_profinit(palette = pal_name_discrete, discrete = TRUE, exact = exact, ...),
    ggplot2.continuous.fill = function(...) scale_fill_profinit(palette = pal_name, discrete = FALSE, reverse = TRUE, ...),
    ggplot2.discrete.fill = function(...) scale_fill_profinit(palette = pal_name_discrete, discrete = TRUE, exact = exact, ...)
  )
  options(backup_default_scales = .backup_default_scales)

  # redefine default ggplot theme
  if (requireNamespace("ggplot2")) ggplot2::theme_set(theme_profinit())
}


#' Reverts theme setup to defaults
#'
#' The function sets to defaults color and fill scales in ggplot2, R4 color
#' palette (base graphics) and greyish color theme in ggplot2.
#'
#' @describeIn set_theme Resets default plotting theme.
#' @returns The function returns nothing, it just changes default behaviour.
#' @export
#'
unset_theme <- function() {

  # set the base R palette
  grDevices::palette("R4")

  # sets the ggplot2 defaults back to original values
  .backup_default_scales <- options()$backup_default_scales
  if (is.null(.backup_default_scales)) {
    .backup_default_scales <- list(
      ggplot2.continuous.colour = NULL,
      ggplot2.discrete.colour = NULL,
      ggplot2.continuous.fill = NULL,
      ggplot2.discrete.fill = NULL
    )
  }
  options(.backup_default_scales)

  if (requireNamespace("ggplot2")) ggplot2::theme_set(ggplot2::theme_gray())
}


