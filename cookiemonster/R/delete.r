#' Delete Cookies
#'
#' @param domain The domain for which the cookies should be deleted.
#' @param ask A logical value indicating whether the user should be asked to
#'   confirm the deletion.
#' @inheritParams get_cookies
#' @export
#'
#' @returns Nothing. Called to remove cookies from jar.
#'
#' @examples
#' # to conform with CRAN policies, examples use a temporary location. Do not use
#' # the options like this, except you want your cookies gone when closing R.
#' options(cookie_dir = tempdir())
#'
#' add_cookies(cookiefile = system.file("extdata", "cookies.txt", package = "cookiemonster"))
#' delete_cookies("example.com", ask = FALSE)
delete_cookies <- function(domain,
                           key = "",
                           jar = default_jar(),
                           fixed = FALSE,
                           ask = TRUE) {

  f <- file.path(jar, paste0("cookies.rds"))
  cookies <- readRDS(f)
  sel <- select_cookies(cookies, domain, key, fixed)
  out <- cookies[!sel, ]
  print(tibble::as_tibble(cookies[sel, c("domain", "name")]))
  choice <- TRUE
  if (ask) {
    choice <- utils::askYesNo(
      msg = "Are you sure that you want to delete these cookies?",
      default = FALSE
    )
  }
  if (isTRUE(choice)) {
    saveRDS(out, f)
    cli::cli_alert_success("Cookies deleted.")
  }
}
