#' Download a summary of submitted data
#'
#' Download a summary of submitted data
#'
#' @param datacall integer year giving which data call year to inquire about.
#'   If NULL returns the a summary of the most recent approved data.
#'
#' @return a data.frame of VMS summary data
#' 
#' @examples
#' \dontrun{
#' upload_summary <- get_upload_summary()
#' }
#' 
#' @export
get_upload_summary <- function(datacall = NULL) {
  url <-
  if (!is.null(datacall)) {
    vms_api(glue("vmssummary/{datacall}"))
  } else {
    vms_api("vmssummary")
  }

  vms_get(url, use_token = TRUE)
}
