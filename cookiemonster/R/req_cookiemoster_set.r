#' Set cookies from the cookiejar to a httr2 request
#'
#' Adds all cookies for the domain from the jar to a httr2 request.
#'
#' @param req A \link[httr2]{request} object.
#' @param ... not currently used.
#' @inheritParams get_cookies
#'
#' @returns A \link[httr2]{request} object (with cookies).
#' @export
#'
#' @examplesIf rlang::is_installed(c("httr2", "webfakes"))
#' library(httr2)
#' domain <- url_parse(example_url())$hostname
#' add_cookies(cookiestring = "snicker=doodle; password=secret", domain = domain)
#' request(example_url()) |>
#'   req_url_path("/cookies/set") |>
#'   req_cookiemonster_set() |>
#'   req_perform() |>
#'   resp_body_json()
req_cookiemonster_set <- function(req, jar = default_jar(), ...) {
  rlang::check_installed("httr2")
  # should fail with the httr2 message if not a req or invalid url
  url <- tryCatch(httr2::url_parse(req$url),
                  error = function(e) list(hostname = "&"))
  httr2::req_options(req, cookie = get_cookies(url$hostname, as = "string"))
}
