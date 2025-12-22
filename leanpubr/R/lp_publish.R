#' Leanpub Publish
#'
#' @inheritParams lp_get_wrapper
#'
#' @note See \url{https://leanpub.com/help/api}
#'
#' @return List of the result of the \code{\link[httr]{GET}} call and
#' the content
#' @export
#' @examples
#' \donttest{
#' if (lp_have_api_key()) {
#' slug = "neuroimagingforstatisticians"
#' res = lp_publish(slug, nonstop = TRUE, error = FALSE)
#' }
#' }
lp_publish = function(
  slug,
  api_key = NULL,
  secure = TRUE,
  verbose = TRUE,
  ...) {
  L = lp_post_wrapper(
    slug = slug,
    endpoint = "/publish",
    api_key = api_key,
    secure = secure,
    verbose = verbose,
    ...)
  return(L)

}
