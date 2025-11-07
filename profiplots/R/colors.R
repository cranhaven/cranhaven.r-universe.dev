profinit_colors_definition <- c(
  `red` = "#E63C41",
  `blue` = "#465a9b",
  `grey` = "#282828",
  `dark blue` = "#2B436C",
  `purple` = "#7D4191",
  `pink` = "#B5578D",
  `yellow` = "#FFD21E",
  `orange` = "#F3943B",
  `green` = "#41C34B",
  `dark green` = "#007938",
  `azure` = "#3DADE5"
)


#' Profinit colors
#'
#' This function provides an access to standardized Profinit colors hex codes.
#'
#' @export
#' @param ... Either character name(s) or order of Profinit colors.
#' Leave empty for the full color set.
#' @param named Flag whether to produce named vector of profinit colours.
#' Defautls to `FALSE`.
#' @returns Named character vector the same length as input params.
#' @examples
#' # to get Profinit red hexcode
#' profinit_cols("red")
#'
#' # to get Profinit red and gray hexcodes
#' profinit_cols("red", "grey")
#'
#' # to get the first Profinit color
#' profinit_cols(1)
#'
#' # to get all Profinit colors
#' profinit_cols()
#' scales::show_col(profinit_cols())
#'
profinit_cols <- function(..., named = FALSE) {
  cols <- c(...)
  if (is.null(cols)) {
    return(profinit_colors_definition)
  }

  profinit_cols <- profinit_colors_definition[cols]
  if (named) {
    return(profinit_cols)
  }
  unname(profinit_cols)
}


profinit_palettes_definition <- list(
  `discrete` = profinit_cols("red", "blue", "pink", "orange", "yellow", "dark green"),
  `discrete-full` = profinit_cols(),
  `blue-red` = profinit_cols("blue", "red"),
  `blue-white-red` = c(profinit_cols("blue"), "white", profinit_cols("red")),
  `reds` = c(profinit_cols("red"), "#F7C7C8"),
  `reds-dark` = c(profinit_cols("red"), "#390F10"),
  `blues` = c(profinit_cols("blue"), "#CACFE2"),
  `blues-dark` = c(profinit_cols("blue"), "#1A213A"),
  `greys` = c('#282828', '#BEBEBE')
)

#' List of available Profinit palettes
#'
#' Note: Function name has been chosen to match the `grDevices::palette.pals()`.
#'
#' @export
#' @returns A character vector of all available palettes in this package.
#' @examples
#' profinit_pal.pals()
#'
#' # Now you are able to use this information, e.g.:
#' reds_palette <- profinit_pal("reds")
#' reds_palette(4)
#' scales::show_col(reds_palette(4))
#'
profinit_pal.pals <- function() {
  names(profinit_palettes_definition)
}


#' Profinit color palettes
#'
#' Returns function that interpolates (if `exact=FALSE`) chosen Profinit color palette.
#' That is, the resulting function is able to give you desired number
#' of colors (hexes) within given Profinit color palette.
#'
#' See the examples for more details about its usecase.
#'
#' @export
#' @param pal_name Character name of chosen Profinit palette. See `profinit_pal.pals()` for available palettes.
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param exact Indicates whether the color scale is supposed to be followed exactly.
#'  Be ware you may run out of colors. Defaults to `TRUE` for discrete palette names, `FALSE` otherwise.
#' @param ... Additional arguments to pass to `grDevices::colorRampPalette()`
#' @returns A function providing desired number of colors (hex codes) from the specified palette.
#' @examples
#' # Example 1 - get colors from the 'discrete' Profinit palette
#' discrete_cols <- profinit_pal("discrete")
#' discrete_cols(3)
#' scales::show_col(discrete_cols(3))
#'
#' # .. The number of colors is limited in this palette. Once you reach the
#' # limit, Profinit's grey is used to fill the missings:
#' scales::show_col(discrete_cols(10))
#'
#' # .. You can bypass this via either enabling the interpolation (defaults
#' # to `FALSE` for `discrete`-like color palettes and `TRUE` for other palettes)
#' # or try your luck with full set of Profinit colors.
#' discrete_cols_int <- profinit_pal("discrete", exact = FALSE)
#' scales::show_col(discrete_cols_int(10))
#'
#'
#' # Example 2 - get colors from other Profinit palette
#' profinit_reds_cols <- profinit_pal("reds")
#' profinit_reds_cols(3)
#' scales::show_col(profinit_reds_cols(3))
#' # Again, we can interpolate
#' profinit_reds_cols(15)
#' scales::show_col(profinit_reds_cols(15))
#'
#' # Example 3 - using palette in baseR plots
#' plot(mtcars$mpg, mtcars$qsec, col=profinit_pal("discrete")(5), pch=16)
#'
profinit_pal <- function(pal_name = "blue-red", reverse = FALSE, exact = NULL, ...) {

  available_pal_names <- names(profinit_palettes_definition)
  is_profinit_palette <- pal_name %in% available_pal_names
  if (!is_profinit_palette) {
    msg <- paste0("Palette needs to be either '", paste0(available_pal_names, collapse = "', '"), "', but '", pal_name, "' recieved.")
    stop(msg)
  }

  pal <- profinit_palettes_definition[[pal_name]]

  if (reverse) pal <- rev(pal)

  if (is.null(exact)) exact <- grepl("discrete", pal_name)
  if (exact) {
    col_fun <- function(n) {
      if (n > length(pal)) warning(paste0("Profiplots palette'", pal_name, "' contains only '", length(pal), "' colors but '", n, "' needed."))

      cols <- pal[1:n]
      cols[is.na(cols)] <- profinit_cols("grey")
      unname(cols)
    }
    return(col_fun)
  }

  grDevices::colorRampPalette(pal, ...)
}
