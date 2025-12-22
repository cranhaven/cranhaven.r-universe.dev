#' Leanpub Links
#'
#' @inheritParams lp_get_wrapper
#'
#' @note See \url{https://leanpub.com/help/api}
#'
#' @return List of URLs
#' @export
#' @examples
#' if (lp_have_api_key()) {
#' slug = "biostatmethods"
#' res = lp_links(slug, nonstop = TRUE, error = FALSE)
#' }
lp_links = function(
  slug,
  api_key = NULL,
  secure = TRUE,
  verbose = TRUE,
  ...) {
  L = lp_summary(
    slug = slug,
    api_key = api_key,
    secure = secure,
    verbose = verbose,
    ...)

  url_names = names(L$content)
  url_names = url_names[grepl("(preview|published)_url$", url_names)]
  links = L$content[url_names]
  return(links)
}
