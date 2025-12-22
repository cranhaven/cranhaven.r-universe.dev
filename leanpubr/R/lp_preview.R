#' Leanpub Preview
#'
#' @inheritParams lp_get_wrapper
#'
#' @note See \url{https://leanpub.com/help/api}
#'
#' @return List of the result of the \code{\link[httr]{GET}} call and
#' the content
#' @export
#' @examples
#' \dontrun{
#' if (lp_have_api_key()) {
#' slug = "neuroimagingforstatisticians"
#' res = lp_preview(slug, nonstop = FALSE, error = FALSE)
#' }
#' }
lp_preview = function(
  slug,
  api_key = NULL,
  secure = TRUE,
  verbose = TRUE,
  ...) {
  L = lp_get_wrapper(
    slug = slug,
    endpoint = "/preview",
    verb = "POST",
    api_key = api_key,
    secure = secure,
    verbose = verbose,
    ...)
  return(L)

}
