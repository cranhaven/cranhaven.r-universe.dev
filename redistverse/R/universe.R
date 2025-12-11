#' Use `r-universe` Packages for `redistverse`
#'
#' @return formatted code to use `r-universe`
#' @export
#'
#' @examples
#' use_redistverse_r_universe()
use_redistverse_r_universe <- function() {
  cli::cli_inform("Add the following to your profile, possibly through {.fn usethis::edit_r_profile}.")
  x <- "options(
    repos = c(
      `alarm-redist` = 'https://alarm-redist.r-universe.dev',
      cran = 'https://cran.r-project.org'
    )
  )"
  cli::cli_code(format(x))
}
