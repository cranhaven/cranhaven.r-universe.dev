#' Leanpub Quiz Responses for a Course
#'
#' @inheritParams lp_post_wrapper
#' @param anonymized_id Anonymized ID for a single user.
#' For a single user's CSV, you will get the JSON response immediately.
#' @param body body sent to \code{\link[httr]{POST}}
#' @param query query information to send to \code{\link[httr]{POST}}
#'
#' @note See Author -> Courses -> Course Name -> Quiz Responses
#' on Leanpub
#'
#' @return List of the result of the \code{\link[httr]{POST}} call and
#' the content
#' @export
#' @importFrom assertthat is.string
#' @examples
#' \dontrun{
#' if (lp_have_api_key()) {
#' slug = "muschellitestcourse"
#' res = lp_quiz_responses(slug, error = FALSE, verbose = 2)
#' }
#' }
lp_quiz_responses = function(
  slug,
  anonymized_id = NULL,
  body = list(),
  api_key = NULL,
  secure = TRUE,
  verbose = TRUE,
  query = list(),
  ...) {
  full_slug = paste0("course_admin/leanpub/", slug, "/")
  full_slug = sub("//$", "/", full_slug)
  L = list(...)
  if ("error" %in% names(L)) {
    error = L$error
  } else {
    error = TRUE
  }
  if (!is.null(anonymized_id)) {
    assertthat::is.string(anonymized_id)
    assertthat::assert_that(is.list(body))
    user = list(anonymized_id)
    names(user) = paste0("user[", anonymized_id, "]")
    body = c(body, user)
  }

  api_key = lp_api_key(api_key = api_key, error = error)
  assertthat::assert_that(is.list(query))
  query$api_key = api_key

  L = lp_post_wrapper(
    slug = full_slug,
    endpoint = "quiz_response_data",
    api_key = api_key,
    secure = secure,
    verbose = verbose,
    query = query,
    body = body,
    ...)
  return(L)

}
