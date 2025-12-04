#' Get dataset full metadata by ID
#'
#' @param id string. The id of the dcat dataset.
#'
#' @return a list
#' @export
#'
#' @examples
#' i14y_get_dataset_metadata(id = "02e34f85-14df-45b5-a38b-2f063c999481")
i14y_get_dataset_metadata <- function(
  id = NULL
) {
  check_not_null(id)
  check_string(id)
  if (!curl::has_internet()) {
    message("No internet connection")
    return(NULL)
  }

  req <- httr2::request(paste0("https://input.i14y.admin.ch/api/Dataset/", id))
  req <- httr2::req_user_agent(
    req,
    "I14Y R package (https://github.com/lgnbhl/I14Y)"
  )
  req <- httr2::req_retry(req, max_tries = 2)
  req <- httr2::req_perform(req)
  resp <- httr2::resp_body_json(req, simplifyVector = TRUE, flatten = TRUE)
  return(resp)
}
