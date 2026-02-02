# From https://github.com/tidyverse/tidyverse/blob/main/R/logo.R
#' @title The bupaverse logo
#'
#' @description
#' The [`bupaverse`] logo, using ASCII or Unicode characters.
#'
#' @param unicode [`logical`] (default `TRUE`): Whether to use Unicode symbols. Default is `TRUE` on UTF-8 platforms.
#'
#' @details
#' Use [`cli::ansi_strip()`] to get rid of the colors.
#'
#' @returns
#' Returns a [`character`] representing the [`bupaverse`] logo.
#'
#' @examples
#' bupaverse_logo()
#'
#' @export
bupaverse_logo <- function(unicode = l10n_info()$`UTF-8`) {
  logo <- "
.______    __    __  .______      ___   ____    ____  _______ .______          _______. _______
|   _  \\  |  |  |  | |   _  \\    /   \\  \\   \\  /   / |   ____||   _  \\        /       ||   ____|
|  |_)  | |  |  |  | |  |_)  |  /  ^  \\  \\   \\/   /  |  |__   |  |_)  |      |   (----`|  |__
|   _  <  |  |  |  | |   ___/  /  /_\\  \\  \\      /   |   __|  |      /        \\   \\    |   __|
|  |_)  | |  `--'  | |  |     /  _____  \\  \\    /    |  |____ |  |\\  \\----.----)   |   |  |____
|______/   \\______/  | _|    /__/     \\__\\  \\__/     |_______|| _| `._____|_______/    |_______|
                                                                                                "

  logo_style <- cli::make_ansi_style("#339999")

  structure(logo_style(logo), class = "bupaverse_logo")
}

#' @export
print.bupaverse_logo <- function(x, ...) {

  cat(x, ..., sep = "\n")
  invisible(x)
}