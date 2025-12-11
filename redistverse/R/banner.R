#' Print the 'redistverse' logo using ASCII characters
#'
#' @return `NULL`, invisibly
#' @export
#'
#' @examples
#' redistverse_logo()
redistverse_logo <- function() {
  logo <- c(
    r"(  __   ___  __  o  __  ___       ___  __   __   ___ )",
    r"( |__) |__  |  \ | /__`  |  \  / |__  |__) /__` |__  )",
    r"( |  \ |___ |__/ | .__/  |   \/  |___ |  \ .__/ |___ )"
  )

  logo <- paste0(
    cli::col_br_white(substring(logo, 1, 26)),
    cli::col_br_red(substring(logo, 27, 52))
  )
  logo <- sub("o", cli::col_yellow("\u2b22"), logo)

  cat(cli::style_bold(logo), sep = "\n")
  cat(cli::col_grey("----------------------------------------------------\n"))
  invisible(NULL)
}
