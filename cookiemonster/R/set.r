#' Add Cookies to the Browser
#'
#' This function allows you to add browser cookies to the cookie storage. It can
#' work with either a cookie file or a direct cookie string (e.g., copied from a
#' CURL call). But remember, just like in real life, you can't have your cookie
#' and eat it too - pick only one!
#'
#' @param cookiefile A character string indicating the path to the cookie file.
#' @param cookiestring A character string representing the cookie in string
#'   format.
#' @param session A live html session created with \link[rvest]{read_html_live}
#'   (only version >= 1.0.4).
#' @param domain An optional parameter that specifies the host/domain. It's only
#'   used when `cookiestring` is provided.
#' @param confirm If \code{TRUE}, you confirm to write the cookie jar to disk
#'   (if it does not exist yet) without seeing the interactive menu.
#'
#' @returns No explicit return. Instead, this function stores the cookies using
#'   the `store_cookies` function.
#'
#' @examples
#' # to conform with CRAN policies, examples use a temporary location. Do not use
#' # the options like this, except you want your cookies gone when closing R.
#' options(cookie_dir = tempdir())
#'
#' # Using a cookie file:
#' # to conform with CRAN policies, examples use a temporary location. Do not use
#' # the options like this, except you want your cookies gone when closing R.
#' add_cookies(cookiefile = system.file("extdata", "cookies.txt", package = "cookiemonster"))
#'
#' # Using a cookie string:
#' add_cookies(cookiestring = "username=johndoe; password=secret", domain = "www.example.com")
#'
#' @note You can't provide both a cookiefile and a cookiestring at the same
#'   time. That's like trying to dunk two cookies in a tiny cup of milk!
#'
#'   Your cookies are saved in an encrypted file. See \link{encrypt_vec} for
#'   more info.
#'
#' @seealso \code{\link{store_cookies}}
#'
#' @export
add_cookies <- function(cookiefile, cookiestring, session, domain = NULL, confirm = FALSE) {

  if (sum(!missing(cookiefile), !missing(cookiestring), !missing(session)) > 1) {
    cli::cli_abort("This function can either handle a cookie file or string, not both")
  } else if (!missing(cookiefile)) {
    cookies <- read_cookiefile(cookiefile)
  } else if (!missing(cookiestring)) {
    cookies <- parse_cookiestring(cookiestring, domain = domain)
  } else if (!missing(session)) {
    cookies <- parse_session(session)
  } else {
    cli::cli_abort("You must provide either a cookie file, cookie string or a live html session.")
  }

  store_cookies(cookies, confirm = confirm)
}


#' Store cookies in a jar
#'
#' @param cookies A data frame of cookies
#' @param jar The directory to store the cookies in. Defaults to
#'   \code{default_jar()}.
#' @param confirm If \code{TRUE}, you confirm to write the cookie jar to disk
#'   (if it does not exist yet) without seeing the interactive menu.
#'
#' @returns No return value, called to save (encrypted) cookies on disk.
#' @export
#'
#' @examples
#' # to conform with CRAN policies, examples use a temporary location. Do not use
#' # the options like this, except you want your cookies gone when closing R.
#' options(cookie_dir = tempdir())
#'
#' if (requireNamespace("curl", quietly = TRUE)) {
#'   # get cookies from a curl request
#'   library(curl)
#'   h <- new_handle()
#'   resp <- curl_fetch_memory("https://hb.cran.dev/cookies/set?new_cookies=moo", handle = h)
#'   cookies <- handle_cookies(h)
#'
#'   # then store them for future use
#'   store_cookies(cookies)
#'
#'   # then you can retrieve them and use in future calls
#'   get_cookies("hb.cran.dev")
#' }
store_cookies <- function(cookies,
                          jar = default_jar(),
                          confirm = FALSE) {

  cookies$value <- encrypt_vec(cookies$value)
  cookies$domain <- url_get_domain(cookies$domain)
  domains <- toString(unique(cookies$domain))
  dir.create(jar, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(jar, paste0("cookies.rds"))
  if (file.exists(f)) {
    cookies_old <- readRDS(f)
    # replace old cookies
    cookies_old <- cookies_old[!cookies_old$domain %in% cookies$domain, ]
    cookies <- vctrs::vec_rbind(cookies_old, cookies)
  } else if (!confirm) {
    utils::askYesNo(msg = paste0(
      "You are storing cookies for the first time. Is it okay to write them to ", f, "?"
    ))
  }
  saveRDS(cookies, f, compress = FALSE)
  cli::cli_alert_success("Cookies for {.emp {domains}} put in the jar!")
}


#' Get the default cookie storage directory (jar)
#'
#' This function returns the default directory (jar) for storing cookies. Users
#' can set their own cookie storage location by using \code{options(cookie_dir =
#' "your/directory/here")}. If no custom directory is specified, the default
#' directory used by the \code{rappdirs} package will be returned.
#'
#' @returns A string representing the path to the default cookie storage
#'   directory (jar).
#' @export
#'
#' @examples
#' # Get the default jar
#' default_jar()
#'
#' # Set a custom cookie storage directory
#' options(cookie_dir = "/path/to/your/cookie/directory")
#' # Get the custom cookie directory
#' default_jar()
#'
#' # revert to the package default
#' options(cookie_dir = rappdirs::user_cache_dir("r_cookies"))
default_jar <- function() {
  if (!is.null(getOption("cookie_dir"))) {
    return(getOption("cookie_dir"))
  } else {
    rappdirs::user_cache_dir("r_cookies")
  }
}


#' read a cookie file
#' @noRd
read_cookiefile <- function(cookiefile) {

  lines <- readLines(cookiefile, warn = FALSE)
  if (!any(grepl("HTTP Cookie File", lines, fixed = TRUE))) {
    cli::cli_alert_danger("This does not seem to be a valid cookiefile")
  }
  df <- utils::read.delim(text = lines[grep("\t", lines)], header = FALSE)
  colnames(df) <- c(
    "domain", "flag", "path", "secure", "expiration", "name", "value"
  )
  df$domain <- url_get_domain(sub("^\\.", "", df$domain))
  df$expiration <- as.POSIXct.numeric(df$expiration, origin = "1970-01-01")
  return(tibble::as_tibble(df))

}

#' parse a string containing cookies
#' @noRd
parse_cookiestring <- function(cookiestring, domain) {
  if (is.null(domain)) {
    cli::cli_abort("When parsing cookie strings, you need to provide a domain")
  }
  cookiestring <- stringi::stri_replace_first_regex(cookiestring, "^Cookie:\\s*", "")
  cookiestring <- stringi::stri_split_fixed(cookiestring, pattern = "; ")[[1]]
  tibble::tibble(
    domain = url_get_domain(domain),
    flag = NA,
    path = NA,
    secure = NA,
    expiration = NA,
    name = stringi::stri_extract_first_regex(cookiestring, "^.*?(?==)"),
    value = stringi::stri_extract_first_regex(cookiestring, "(?<==).*$")
  )
}


#' parse a live html session
#' @noRd
parse_session <- function(sess) {
  if (!inherits(sess, "LiveHTML")) {
    cli::cli_abort("{.code sess} must be an object created by {.code rvest::read_html_live()}.")
  }
  cookies <- sess$session$Network$getCookies()
  out <- dplyr::bind_rows(cookies)
  if (nrow(out) > 0) {
    out <- out |>
      dplyr::mutate(expiration = as.POSIXct(.data$expires)) |>
      dplyr::select("domain", flag = "sameParty", "path", "secure",
                    "expiration", "name", "value")
  }
  return(out)
}
