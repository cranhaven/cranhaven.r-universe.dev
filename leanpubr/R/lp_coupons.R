#' Leanpub Coupons
#'
#' @inheritParams lp_get_wrapper
#'
#' @note See \url{https://leanpub.com/help/api}
#'
#' @return List of the result of the \code{\link[httr]{GET}} call and
#' the content
#' @export
#'
lp_coupons = function(
  slug,
  api_key = NULL,
  secure = TRUE,
  verbose = TRUE,
  ...) {
  L = lp_get_wrapper(
    slug = slug,
    endpoint = "/coupons",
    api_key = api_key,
    secure = secure,
    verbose = verbose,
    error = FALSE,
    ...)
  return(L)

}
