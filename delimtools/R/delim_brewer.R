#' Customize Delimitation Colors
#'
#' @description
#' `delim_brewer()` returns a set of colors created by interpolating or using
#' color palettes from [RColorBrewer][RColorBrewer::brewer.pal], 
#' [viridisLite][viridisLite::viridis.map] or [randomcoloR][randomcoloR::distinctColorPalette].
#'
#' @param delim Output from [delim_join].
#' @param package Package which contains color palettes. Available options are
#' "RColorBrewer", "viridisLite" or "randomcoloR".
#' @param palette A palette name. [brewer.pal][RColorBrewer::brewer.pal] for RColorBrewer
#' or [viridis][viridisLite::viridis.map] for viridisLite options.
#' @param seed Integer. Number to initialize random number generator.
#'
#' @details
#' `delim_brewer()` interpolates over a color palette and returns a vector of random colors
#' whose length is equal to the sum of unique species delimitation partitions in `delim`.
#' For reproducibility, make sure to provide a `seed`. If not provided, [Sys.time][base::Sys.time]
#' will be used as seed instead. One should also try different seeds to get best color combinations for plotting.
#'
#' @return
#' A `character` vector of hexadecimal color codes.
#'
#' @author
#' Rupert A. Collins, Pedro S. Bittencourt
#'
#' @examples
#'
#' # create a vector of colors
#' cols <- delim_brewer(geophagus_delims, package = "randomcoloR")
#'
#' @export
delim_brewer <- function(delim, package = NULL, palette = NULL, seed = NULL) {
  nclust <- delim |>
    tidyr::pivot_longer(cols = -labels, names_to = "method", values_to = "clusters") |>
    dplyr::summarise(n = dplyr::n_distinct(.data$clusters, na.rm = TRUE)) |>
    dplyr::pull(1)
  
  if (is.null(package)) {
    package <- "RColorBrewer"
    
    cli::cli_warn(c("{cli::col_yellow({cli::symbol$warning})} Argument {.arg package} not found. Using {.pkg RColorBrewer} package.",
                    "i" = "Available packages are {.pkg RColorBrewer}, {.pkg viridisLite} and {.pkg randomcoloR}"
    ))
  }
  
  # check if package is installed
    rlang::check_installed(package, reason = "to generate colors")
  
  if (package == "randomcoloR" && !is.null(palette)) {
    cli::cli_warn("{cli::col_yellow({cli::symbol$warning})} Argument {.arg palette} not required for {.pkg randomcoloR}")
  }

  if (is.null(palette) && package != "randomcoloR") {
    palette <- "Set1"

    cli::cli_warn(c("{cli::col_yellow({cli::symbol$warning})} Argument {.arg palette} not found. Using {.pkg RColorBrewer} {.strong Set1} palette.",
      "i" = "Check {.pkg RColorBrewer} and {.pkg viridisLite} packages for more palette options."
    ))
  }

  if (is.null(seed)) {
    seed <- Sys.time()

    cli::cli_warn(c("{cli::col_yellow({cli::symbol$warning})} Argument {.arg seed} not found. Using {.fn Sys.time} as seed.",
      "i" = "For reproducibility, you may want to set a custom {.arg seed} instead. {.arg seed} is printed below:",
      "{seed}"
    ))
  }
  
  if (package == "RColorBrewer") {
    mpal <- RColorBrewer::brewer.pal.info[palette, ]$maxcolors

    if (nclust <= mpal) {
      cpal <- RColorBrewer::brewer.pal(nclust, palette)
    }

    get_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(mpal, palette))

    cpal <- get_palette(nclust)
  } else if (package == "viridisLite") {
    
    # check if scales is installed
    rlang::check_installed("scales", reason = "to use `viridisLite` palette colors")
    
    cpal <- scales::pal_viridis(option = palette)(nclust)

    if (length(unique(cpal)) < nclust) {
      get_palette <- grDevices::colorRampPalette(scales::pal_viridis(option = palette)(nclust))

      cpal <- get_palette(nclust)
    }
  } else if (package == "randomcoloR") {
    cpal <- withr::with_seed(seed = seed, code = randomcoloR::distinctColorPalette(k = nclust))
  }

  cols <- withr::with_seed(seed = seed, code = sample(cpal))

  return(cols)
}
