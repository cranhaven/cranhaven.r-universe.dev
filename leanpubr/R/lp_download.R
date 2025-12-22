#' Leanpub Download Latest Version
#'
#' @inheritParams lp_get_wrapper
#' @param format format of the book to download
#'
#' @note See \url{https://leanpub.com/help/api}
#'
#' @return List of the result of the \code{\link[httr]{GET}} call and
#' the content
#' @importFrom httr progress warn_for_status write_disk
#' @export
#' @examples
#' if (lp_have_api_key()) {
#' slug = "biostatmethods"
#' res = lp_download(slug, verbose = FALSE, nonstop = TRUE, error = FALSE,
#' format = "epub")
#' file.exists(res)
#' }
lp_download = function(
  slug,
  format = c("pdf", "epub"),
  api_key = NULL,
  secure = TRUE,
  verbose = TRUE,
  ...) {
  format = tolower(format)
  format = match.arg(format)

  L = lp_summary(
    slug = slug,
    api_key = api_key,
    secure = secure,
    verbose = verbose,
    ...)

  url_names = names(L$content)
  n = paste0(format, "_published_url")
  if (!(n %in% url_names)) {
    stop(paste0("Format ", format, " URL not in summary"))
  }
  file_url = L$content[[n]]
  tfile = tempfile(fileext = paste0(".", format))
  res = httr::GET(file_url,
                  httr::write_disk(path = tfile),
                  if (interactive() && verbose) httr::progress())
  httr::warn_for_status(res)
  ### need more here to follow the redirect
  return(tfile)
}
