#' Profinit color scale constructor
#'
#' @export
#' @param palette Character name of palette in `profinit_palettes`. Use `profinit_pal.pals()` to list available palettes.
#' @param discrete Boolean indicating whether color aesthetic is discrete or not. Defaults to `TRUE`.
#' @param reverse Boolean indicating whether the palette should be reversed. Defaults to `FALSE`.
#' @param exact Indicates whether the color scale is supposed to be followed exactly.
#'  Be ware you may run out of colors. Defaults to `TRUE` for discrete palette names, `FALSE` otherwise.
#' @param na.value What value is going to be used for missings. Defaults to profinit's grey with some transparency added.
#' @param ... Additional arguments passed to `ggplot2::discrete_scale()` or
#'            `ggplot2::scale_color_gradientn()`, used respectively when `discrete` is set to `TRUE` or `FALSE`
#' @returns Ggplot2 color scale constructor based on Profinit color palette.
#' @importFrom ggplot2 discrete_scale
#' @importFrom ggplot2 scale_fill_gradientn
#' @family scale_color_profinit
#' @examples
#' library(ggplot2)
#'
#' iris_plt <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
#'   geom_point() +
#'   theme_profinit()
#'
#' iris_plt + scale_color_profinit()
#'
#' # Now, let's use another Profinit palette:
#' # (see `profinit_pal.pals()` for all the options)
#' iris_plt + scale_color_profinit(palette = "reds-dark")
#'
#'
#'
scale_color_profinit <- function(palette = "blue-red", discrete = TRUE, reverse = FALSE, exact = NULL, na.value = NULL, ...) {
  pal <- profinit_pal(pal_name = palette, reverse = reverse, exact = exact)
  if (is.null(na.value))  na.value <- paste0(profinit_cols("grey"), "99")
  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("profinit_", palette), palette = pal, na.value = na.value, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), na.value = na.value, ...)
  }
}

#' @rdname scale_color_profinit
#' @export
#'
scale_colour_profinit <- scale_color_profinit

#' `scale_color_profinit_d` - Discrete Profinit color scale (similar to
#' `scale_color_viridis_d`). It can't be used with continuous variables. There
#' is BrE equivalent, too: `scale_colour_profinit_d`.
#'
#' @rdname scale_color_profinit
#' @family scale_color_profinit
#' @export
#'
scale_color_profinit_d <- function(palette = "blue-red", ...) {
  scale_color_profinit(palette = palette, discrete = TRUE, ...)
}

#' @rdname scale_color_profinit
#' @export
#'
scale_colour_profinit_d <- scale_color_profinit_d


#' `scale_color_profinit_c` - Continuous Profinit color scale (similar to
#' `scale_color_viridis_c`). It can't be used with discrete variables. There is
#'  BrE equivalent, too: `scale_colour_profinit_c`.
#'
#' @rdname scale_color_profinit
#' @family scale_color_profinit
#' @export
#'
scale_color_profinit_c <- function(palette = "blue-red", ...) {
  scale_color_profinit(palette = palette, discrete = FALSE, ...)
}

#' @rdname scale_color_profinit
#' @export
scale_colour_profinit_c <- scale_color_profinit_c


#' Fill scale constructor for Profinit colors.
#'
#' @export
#' @param palette Character name of palette in `profinit_palettes`
#' @param discrete Boolean, indicating whether color aesthetic is discrete or not
#' @param reverse Boolean, indicating whether the palette should be reversed
#' @param exact Indicates whether the color scale is supposed to be followed exactly.
#'  Be ware, you may run out of colors. Defaults to `TRUE` for discrete palette names, `FALSE` otherwise.
#' @param ... Additional arguments passed to `discrete_scale()` or
#'            `scale_fill_gradientn()`, used respectively when discrete is `TRUE` or `FALSE`
#' @returns Ggplot2 fill scale constructor based on Profinit color palette.
#' @importFrom ggplot2 discrete_scale
#' @importFrom ggplot2 scale_fill_gradientn
#' @family scale_fill_profinit
#' @examples
#' library(ggplot2)
#' plt <- ggplot(ggplot2::diamonds, ggplot2::aes(x = clarity, y = price, fill = clarity)) +
#'   geom_boxplot() +
#'   theme_profinit()
#'
#' plt + scale_fill_profinit()
#'
#' # Discrete scale, follow exact color codes (default for the `discrete` palette).
#' # ! You may run out of colors - see the example below.
#' # Use either exact = FALSE (next example) or `discrete-full` palette.
#' plt + scale_fill_profinit("discrete")
#'
#' # Now, the colors are approximated
#' plt + scale_fill_profinit("discrete", exact = FALSE)
#'
scale_fill_profinit <- function(palette = "blue-red", discrete = TRUE, reverse = FALSE, exact = NULL, ...) {
  pal <- profinit_pal(pal_name = palette, reverse = reverse, exact = exact)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("profinit_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}

#' `scale_fill_profinit_d` - Discrete Profinit fill scale (similar to
#' `scale_fill_viridis_d`). It can't be used with continuous variables.
#'
#' @rdname scale_fill_profinit
#' @family scale_fill_profinit
#' @export
#'
scale_fill_profinit_d <- function(palette = "blue-red", ...) {
  scale_fill_profinit(palette = palette, discrete = TRUE, ...)
}


#' `scale_fill_profinit_c` - Continuous Profinit fill scale (similar to
#' `scale_fill_viridis_c`). It can't be used with discrete variables.
#'
#' @rdname scale_fill_profinit
#' @family scale_fill_profinit
#' @export
#'
scale_fill_profinit_c <- function(palette = "blue-red", ...) {
  scale_fill_profinit(palette = palette, discrete = FALSE, ...)
}
