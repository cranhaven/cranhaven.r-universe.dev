#' Retrieve cookies from a jar
#'
#' Imagine you're reaching into a magical jar overflowing with those scrumptious
#' digital delights from websites you've visited. The flavour? Up to you! Just
#' select your desired output format.
#'
#' @param domain A character string of the domain to retrieve cookies for.
#'   Accepts regular expression depending on the value of \code{fixed}.
#' @param key An optional filter to retrieve only certain cookies by matching
#'   certain keys/names. Accepts regular expression depending on the value of
#'   \code{fixed}.
#' @param jar A character string of the path to the cookie jar (the default is
#'   to use \code{default_jar()} to get a suitable directory).
#' @param as A character string of the type of output to return.
#' @param fixed If \code{TRUE}, \code{domain} and \code{key} are matched as is.
#'   If either \code{domain} or \code{key}, only those values are treated as is.
#' @returns Depending on the value of \code{as}, returns either a data frame, a
#'   character string, or a named vector.
#'
#' @details The function returns cookies in one of three formats:
#'
#' \itemize{
#'   \item{\strong{data.frame:}} is how cookies are stored internally and can be used for manual inspection.
#'   \item{\strong{string:}} is used by \code{curl} and \code{httr2}.
#'   \item{\strong{vector:}} is used by \code{httr}.
#'   \item{\strong{list:}} is used by \code{chromote}.
#' }
#'
#'   See \code{vignette("cookies", "cookiemonster")} for how to use cookies with
#'   these packages.
#'
#' @note Your cookies are saved in an encrypted file. See \link{encrypt_vec} for
#'   more info.
#'
#'
#' @export
#'
#' @examples
#' # to conform with CRAN policies, examples use a temporary location. Do not use the options like this
#' options(cookie_dir = tempdir())
#'
#' # put some cookies in the jar
#' add_cookies(cookiestring = "chococookie=delicious", domain = "example.com")
#' # Reach into your cookie jar and enjoy!
#' get_cookies("example.com")
#' # put different cookies into the jar (overwrites previous)
#' add_cookies(cookiestring = "oatmeal=delicious; peanutbutter=delicious", domain = "example.com")
#' add_cookies(cookiestring = "snickerdoodle=delicious", domain = "another.example.com")
#' # only get cookies for example.com, not another.example.com
#' get_cookies("^example.com")
#' # only get some cookies from example.com
#' get_cookies(domain = "^example.com", key = "peanut")
#' @seealso \code{\link{add_cookies}}
get_cookies <- function(domain,
                        key = "",
                        jar = default_jar(),
                        as = c("data.frame", "string", "vector", "list"),
                        fixed = FALSE) {

  as <- match.arg(as)
  f <- file.path(jar, paste0("cookies.rds"))
  if (!file.exists(f)) {
   cli::cli_abort(paste(
     "The directory {jar} does not contain any cookies yet. Use {.fn add_cookies} to",
     "store cookies for a website (see {.code vignette(\"cookies\", \"cookiemonster\")} for details)."
   ))
  }
  cookies <- readRDS(f)

  sel <- select_cookies(cookies, domain, key, fixed)

  out <- cookies[sel, ]
  out$value <- decrypt_vec(out$value)

  switch (
    as,
    "data.frame" = return(out),
    "string" = return(prep_cookies(out)),
    "vector" = return(prep_cookies(out, as_list = TRUE)),
    "list" = return(prep_cookies_chromote(out))
  )

}


#' prepare the cookie df for usage in different packages
#' @noRd
prep_cookies <- function(tbl, as_list = FALSE) {
  if (nrow(tbl) > 0) {
    if (as_list) {
      return(stats::setNames(tbl$value, tbl$name))
    } else {
      return(paste0(tbl$name, "=", tbl$value, collapse = "; "))
    }
  } else {
    if (as_list) {
      return(list())
    } else {
      return("")
    }
  }
}


#' prepare the cookie df for usage in chromote
#' @noRd
prep_cookies_chromote <- function(tbl) {
  tbl$httpOnly <- FALSE
  tbl$expires <- as.integer(tbl$expiration)
  tbl$expiration <- NULL
  lapply(seq_len(nrow(tbl)), function(i) as.list(tbl[i, ]))
}


select_cookies <- function(cookies, domain, key, fixed) {
  if (length(domain) > 1L) {
    cli::cli_abort("{.code domain} must be a string of length 1")
  }

  if (isFALSE(fixed)) {
    sel <- grepl(domain, cookies$domain, perl = TRUE)
    sel[sel] <- grepl(key, cookies$name[sel], perl = TRUE) * sel[sel]
  } else if (fixed == "domain") {
    sel <- grepl(domain, cookies$domain, fixed = TRUE)
    sel[sel] <- grepl(key, cookies$name[sel], perl = TRUE) * sel[sel]
  } else if (fixed == "key") {
    sel <- grepl(domain, cookies$domain, perl = TRUE)
    sel[sel] <- grepl(key, cookies$name[sel], fixed = TRUE) * sel[sel]
  } else {
    sel <- grepl(domain, cookies$domain, fixed = TRUE)
    sel[sel] <- grepl(key, cookies$name[sel], fixed = TRUE) * sel[sel]
  }
  as.logical(sel)
}
