#######################################

# LAPOP Visualization Templates #

#######################################

.lapop_font_paths <- function(pkg = "lapop") {
  regular_path <- system.file("fonts", "Inter-Regular.ttf", package = pkg)
  light_path <- system.file("fonts", "Inter-Light.ttf", package = pkg)

  if (identical(regular_path, "") || identical(light_path, "")) {
    local_fonts_dir <- file.path(getwd(), "inst", "fonts")
    regular_fallback <- file.path(local_fonts_dir, "Inter-Regular.ttf")
    light_fallback <- file.path(local_fonts_dir, "Inter-Light.ttf")

    if (file.exists(regular_fallback) && file.exists(light_fallback)) {
      regular_path <- regular_fallback
      light_path <- light_fallback
    }
  }

  list(
    regular = regular_path,
    light = light_path
  )
}

.lapop_register_fonts <- function(pkg = "lapop") {
  font_paths <- .lapop_font_paths(pkg = pkg)

  if (!file.exists(font_paths$regular) || !file.exists(font_paths$light)) {
    stop("Bundled Inter font files were not found.")
  }

  existing_families <- sysfonts::font_families()

  if (!("inter" %in% existing_families)) {
    sysfonts::font_add(family = "inter", regular = font_paths$regular)
  }

  if (!("inter-light" %in% existing_families)) {
    sysfonts::font_add(family = "inter-light", regular = font_paths$light)
  }

  if ("register_font" %in% getNamespaceExports("systemfonts")) {
    try(
      systemfonts::register_font(
        name = "inter",
        plain = font_paths$regular,
        bold = font_paths$regular,
        italic = font_paths$regular,
        bolditalic = font_paths$regular
      ),
      silent = TRUE
    )
    try(
      systemfonts::register_font(
        name = "inter-light",
        plain = font_paths$light,
        bold = font_paths$light,
        italic = font_paths$light,
        bolditalic = font_paths$light
      ),
      silent = TRUE
    )
  }

  invisible(TRUE)
}

#' LAPOP Fonts
#'
#' This function loads fonts needed for LAPOP graph formatting.
#' No arguments needed; just run lapop_fonts() at the beginning of your session.
#'
#'@return No return value, called for side effects
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu} & Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}
#'
#'@examples
#'\donttest{require(lapop); lapop_fonts()}
#'
#'@import showtext sysfonts
#'@export
lapop_fonts <- function() {
  .lapop_register_fonts()
  showtext::showtext_auto(enable = TRUE)
  message("LAPOP fonts loaded successfully: Inter and Inter Light.")
  invisible(TRUE)
}

