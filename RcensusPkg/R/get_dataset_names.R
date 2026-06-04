#' @title get_dataset_names
#'
#' @description Get the acronym names and descriptions of the Census Bureau's datasets.
#'
#' Function produces a \code{data.table} of the Census Bureau's dataset acronym names
#' that can be used in other \code{RcensusPkg::} functions calling for a dataset acronym name.
#' See Census Bureau's publicly available \href{https://www.census.gov/data/developers/data-sets.html}{datasets}
#' for descriptions. Also see \href{https://www.census.gov/programs-surveys.html}{All Surveys and Programs}
#'
#' @param vintage An optional numeric for the year to select the datasets. If NULL, then all the years are returned.
#' @param filter_name_str An optional character string by which to filter the resultant \code{data.table} using the
#'    "name" column.
#' @param filter_title_str An optional character string by which to filter the resultant \code{data.table} using the
#'    "title" column.
#' @param ignore_case A logical which if \code{FALSE} will not ignore case in filtering the "title" column.
#' @param brief A logical which if \code{TRUE} will return a resultant \code{data.table} with just columns
#'    "name", "vintage", "title". The default is \code{TRUE}.
#'
#' @return A list with a \code{data.table} of dataset names (named "data")
#'   and a vector of unique vintages (named "vintages").
#'
#' @examples
#' \dontrun{
#'   # Requests for Census Bureau descriptions takes well over 10
#'   #  seconds in most cases.
#'   library(data.table)
#'   library(jsonlite)
#'   library(httr2)
#'   library(RcensusPkg)
#'
#'   # Get descriptions/vintages for 2020 datasets with "acs5" in their name.
#'   acs5_datasets_ls <- RcensusPkg::get_dataset_names(
#'     vintage = 2020,
#'     filter_name_str = "acs5/"
#'   )
#'}
#' @import jsonlite
#' @import data.table
#' @import httr2
#'
#' @export
get_dataset_names <- function(
  vintage = NULL,
  filter_name_str = NULL,
  filter_title_str = NULL,
  ignore_case = TRUE,
  brief = TRUE){

  name <- url <- NULL

  year <- vintage

  # Create the url
  a_url <- "https://api.census.gov/data.json"

  change_name <- function(x){
    paste(x[["dataset"]], collapse = "/")
  }
  change_url <- function(x){
    return(x[["distribution"]][["accessURL"]])
  }

  # Make a web request
  tryCatch({
    resp <- httr2::request(a_url) |> httr2::req_perform()
    content_json <- resp |> httr2::resp_body_string()

    content_ls <- jsonlite::fromJSON(content_json)

    datasets_df <- jsonlite::flatten(content_ls[["dataset"]])

    colnames(datasets_df) <- gsub("c_","",colnames(datasets_df))

    datasets_dt <- data.table::as.data.table(datasets_df)

    datasets_dt[, name := apply(datasets_dt, 1, change_name)]
    datasets_dt[, url := apply(datasets_dt, 1, change_url)]

    select_cols_v <- c("name","vintage","title","url","isTimeseries","description","modified")
    if(brief){
      select_cols_v <- c("name","vintage","title")
    }
    datasets_dt <- datasets_dt[, select_cols_v, with = FALSE]
    if(!is.null(year)){
      datasets_dt <- datasets_dt[vintage == year,]
    }

    if(!is.null(filter_name_str)){
      if(nchar(filter_name_str) != 0){
        datasets_dt <- datasets_dt[grepl(filter_name_str, datasets_dt$name, ignore.case = ignore_case, fixed = FALSE)]
      }
    }
    if(!is.null(filter_title_str)) {
      if(nchar(filter_title_str) != 0){
        datasets_dt <- datasets_dt[grepl(filter_title_str, datasets_dt$title, ignore.case = ignore_case, fixed = FALSE)]
      }
    }

    data.table::setorderv(datasets_dt, cols = c("name", "vintage"))

    return(
      list(
        data = datasets_dt,
        vintages = unique(datasets_dt$vintage)
      )
    )
  },error = function(err){
    stop("Error downloading raw json text: ", err$message, "\n")
  })
}
