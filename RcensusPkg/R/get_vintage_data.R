#' @title get_vintage_data
#'
#' @description Get Census Bureau data for a specific dataset, variables, and region
#'   in the form of a \code{data.table}.
#'
#' Function produces a \code{data.table} with selected Census Bureau variables
#'   as columns. The function requires an access key issued from the Bureau.
#'   Variables of interest can be specified individually or by group/table name.
#'   Predicate phrases can be specified for filtering the results.
#'
#' @details
#' See Census Bureau's publicly available
#'  \href{https://www.census.gov/data/developers/data-sets.html}{datasets} for
#'  dataset descriptions.
#'
#' Some of the more common Census Bureau datasets include:
#'  \itemize{
#'    \item\href{https://www.census.gov/data/developers/data-sets/acs-1year.html}{American Community Survey 1-year Data}
#'    \item\href{https://www.census.gov/data/developers/data-sets/ACS-supplemental-data.html}{American Community Survey 1-year Supplemental Data}
#'    \item\href{https://www.census.gov/data/developers/data-sets/acs-5year.html}{American Community Survey 5-Year Data}
#'    \item\href{https://www.census.gov/data/developers/data-sets/decennial-census.html}{Decennial Census}
#'    \item\href{https://www.census.gov/data/developers/data-sets/economic-census.html}{Economic Census}
#'    \item\href{https://www.census.gov/data/developers/data-sets/popest-popproj/popest.html}{Population Estimates and Projections}
#'    \item\href{https://www.census.gov/data/developers/data-sets/abs.html}{Annual Business Survey (ABS)}
#'  }
#'
#' @param dataset A required string that sets the name of the data set of interest (e.g. "acs/acs5").
#'  Descriptions/vintages for datasets can be found by running
#'  \code{RcensusPkg::get_dataset_names()}.
#' @param vintage An optional numeric that sets the vintage of interest. Available vintages
#'   for a specific dataset can be found by running \code{RcensusPkg::get_dataset_names()}.
#' @param vars A required string vector (if the parameter 'group' is \code{NULL}) of variable names to be acquired.
#'   Available variable names can be determined by running \code{RcensusPkg::get_variable_names()}.
#' @param NAME_GEOID A logical which if \code{TRUE} will add "NAME" and "GEO_ID" variables to the 'vars' string vector.
#'   The default is \code{TRUE}.
#' @param predicates An optional vector of strings that adds data filtering.
#'   See \href{https://www.census.gov/data/developers/guidance.html}{Census Data API User Guide} for
#'   forming predicates and filtering or limiting variables. As noted in the guide each predicate must
#'   start with an ampersand sign.
#' @param group An optional string that names an entire group of similar variables to be retrieved.
#'   For example the group value "B01001" would return values of all variables related to
#'  "SEX BY AGE". To find available groups submit a dataset and vintage to \code{RcensusPkg::get_groups}.
#' @param wide_to_long If 'group' is defined then the returned \code{data.table} is normally in a wide format with all the group variables as columns.
#'   If this logical parameter is \code{TRUE} then a long format is returned with group variable names in one column (named "estimate") and
#'   their respective values in another column (named "value").
#' @param region An optional string that specifies the geography of the request. See
#'   \href{https://www.census.gov/library/reference/code-lists/ansi.html}{Federal Information Processing Series (FIPS)}
#'   for a listing of codes for this and the 'regionin' parameter. Not all regions such as counties,
#'   blocks, or tracts are available for a specific dataset and vintage. Use
#'   \code{RcensusPkg::get_geography()} to check on both 'region' and 'regionin'.
#' @param regionin An optional string that sets a qualifier for 'region'.
#' @param na_cols If \code{TRUE} will remove all rows with missing values. If a
#'   vector of column names/integers will check only those columns for missing values.
#' @param key A required string that sets the access key. All Census Bureau API requests require an access key.
#'   Sign-up for a key is free and can be obtained \href{https://api.census.gov/data/key_signup.html}{here}.
#'   The function will check for a global setting of the key via \code{Sys.getenv("CENSUS_KEY")}.
#'   Run \code{usethis::edit_r_environ()} and edit your .Renviron file with the line: CENSUS_KEY=*your key*
#'   to create the global association.
#'
#' @return A \code{data.table}
#'
#' @examples
#' \dontrun{
#'   # Requires Census Bureau API key
#'   # ------American Community Survey 5-Year Data (2009-2021)-----------
#'   # Description: https://www.census.gov/data/developers/data-sets/acs-5year.html
#'
#'   library(data.table)
#'   library(httr2)
#'   library(jsonlite)
#'   library(stringr)
#'   library(RcensusPkg)
#'
#'   # Get the data for the total number of white females ("B01001A_017E") and
#'   # total number of white individuals ("B01001A_001E") across the US
#'
#'   white_females_dt <- RcensusPkg::get_vintage_data(
#'     dataset = "acs/acs5",
#'     vars = c("B01001A_017E", "B01001A_001E"),
#'     vintage = 2021,
#'     region = "state:*"
#'   )
#' }
#' @import data.table
#' @import httr2
#' @import jsonlite
#' @importFrom stringr str_ends str_sub
#' @importFrom stats na.omit
#'
#' @export
get_vintage_data <- function(
  dataset = NULL,
  vintage = NULL,
  vars = NULL,
  NAME_GEOID = TRUE,
  predicates = NULL,
  group = NULL,
  wide_to_long = FALSE,
  region = NULL,
  regionin = NULL,
  na_cols = NULL,
  key = Sys.getenv("CENSUS_KEY")
){

  # Check for key in environment
  key_from_env <- Sys.getenv("CENSUS_KEY")
  if(key_from_env == "" & key == key_from_env){
    stop("'key' argument is missing. A Census Bureau key is required.")
  }

  if(is.null(dataset)){
    stop("The parameter 'dataset' must be specified.")
  }
  if(is.null(group) & is.null(vars)){
    stop("The parameter 'vars' or 'group' must be specified")
  }

  GEO_ID <- variable <- NULL

  # Create a string url based on the submitted parameters
  a_url <- .get_url(dataset, vintage)

  a_url <- paste0(a_url, "?get=")

  if(!is.null(group)){
    a_url <- paste0(a_url, "group(", group, ")")
  }

  if(!is.null(vars)){
    if(NAME_GEOID){
      vars <- c("NAME", "GEO_ID", vars)
    }
    get_vars <- paste(vars, collapse = ",")
    if(!is.null(group)){
      a_url = paste0(a_url, ",", get_vars)
    }else {
      a_url = paste0(a_url, get_vars)
    }
  }

  if(!is.null(predicates)){
    predicates_collapse <- paste(predicates, collapse = "")
    a_url <- paste0(a_url, predicates_collapse)
  }

  if(!is.null(region)){
    a_url <- paste0(a_url, "&for=", region)
  }
  if(!is.null(regionin)){
    a_url <- paste0(a_url, "&in=", regionin)
  }

  a_url <- paste0(a_url, "&key=", key)
  url_coded <- utils::URLencode(a_url)

  # Make a web request
  tryCatch({
    resp <- httr2::request(url_coded) |> httr2::req_perform()
    content_json <- resp |> httr2::resp_body_string()
    content_mt <- jsonlite::fromJSON(content_json)

    # Create data,table
    dt <- data.table::as.data.table(content_mt)
    colnames(dt) <- content_mt[1,]
    dt <- dt[-1]

    if("GEO_ID" %in% names(dt)){
      dt <- dt[, c("pre", "GEOID") := tstrsplit(GEO_ID, "US")] |>
        _[,`:=`(pre = NULL, GEO_ID = NULL)]
    }

    if(!is.null(na_cols)){
      if(is.logical(na_cols)){
        dt <- na.omit(dt)
      }else if(is.vector(na_cols)){
        dt <- na.omit(dt, cols = na_cols)
      }
    }

    if(!is.null(group) & wide_to_long){
      long_dt <- data.table::melt(
        data = dt,
        id.vars = c("NAME","GEOID")
      )

      E_M_dt <- long_dt[stringr::str_ends(long_dt$variable,"E") | stringr::str_ends(long_dt$variable,"M"),]
      if(nrow(E_M_dt) == 0){
        return(long_dt)
      }else {
        E_dt <- E_M_dt[stringr::str_sub(variable, -1,-1) == "E",]
        M_dt <- E_M_dt[stringr::str_sub(variable, -1,-1) == "M",]

        return_dt <- data.table(
          NAME = E_dt$NAME,
          GEOID = E_dt$GEOID,
          variable = stringr::str_remove(E_dt$variable,"E"),
          estimate = E_dt$value,
          moe = M_dt$value
        )
        return(return_dt)
      }
    }
    return(dt)
  },error = function(err){
    stop("Error downloading raw json text: ", err$message, "\n")
  })
}

