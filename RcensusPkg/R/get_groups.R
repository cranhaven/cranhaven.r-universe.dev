#' @title get_groups
#'
#' @description Get the names of Census Bureau variable groups and their
#'   descriptive parameters
#'
#' Function produces a \code{data.table} of variable groups/tables and their
#' descriptions.
#'
#' @param dataset A required string that sets the acronym name of the data set
#'   of interest (e.g. "acs/acs5")
#' @param vintage An required numeric that sets the year of interest.
#'
#' @return A \code{data.table}
#'
#' @examples
#' library(jsonlite)
#' library(data.table)
#' library(httr2)
#' library(RcensusPkg)
#'
#' acs5_groups_dt <- RcensusPkg::get_groups(
#'   dataset = "acs/acs5",
#'   vintage = 2019)
#'
#' @import data.table
#' @import httr2
#' @import jsonlite
#'
#' @export
get_groups <- function(
    dataset = NULL,
    vintage = NULL) {

  if(is.null(dataset)){
    stop("A dataset acronym name is required for get_groups()")
  }

  if(is.null(vintage)){
    stop("A vintage is required for get_groups()")
  }

  name <- description <- variables <- NULL

  # Create a string url based on the submitted parameters
  a_url <- .get_url(dataset, vintage)
  a_url <- paste(a_url, "groups.json", sep="/")

  # Make a web request
  tryCatch({
    resp <- httr2::request(a_url) |> httr2::req_perform()
    content_json <- resp |> httr2::resp_body_string()
    content_ls <- jsonlite::fromJSON(content_json)

    # Return a data.table
    select_cols <- c("name", "description", "variables")

    dt <- data.table::as.data.table(content_ls[["groups"]]) |>
      _[, select_cols, with = FALSE]

    return(dt)
  },error = function(err){
    stop("Error downloading raw json text: ", err$message, "\n")
  })
}
