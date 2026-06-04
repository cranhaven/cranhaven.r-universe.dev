
#' @title get_variable_names
#'
#' @description Get Census Bureau variable acronym names and their label descriptions.
#'
#' Function produces a `data.table` of variable acronym names and their
#'    descriptions. The function returns 4 columns:
#'    \tabular{ll}{
#'      \strong{name} \tab the name of the parameter\cr
#'      \strong{label} \tab the Bureau's description of the parameter\cr
#'      \strong{required} \tab a boolean indicating if the parameter is required\cr
#'      \strong{predicateType} \tab a string indicating the variable primitive type\cr
#'    }
#'
#'    Note that a variable with a \strong{required} character value "true" must be included
#'    in your data requests (i.e. `RcensusPkg::get_vintage_data()`) or
#'    the API will return an error.
#'
#' @details
#' The function's search for variable names depends on either specifying the parameters 'dataset'
#' name or a dataset 'category'. Entering an available 'vintage' also influences obtaining a
#' full dataframe of variable names and descriptions. To assist in using the function the user
#' should consult the Census Bureau's publicly available \href{https://www.census.gov/data/developers/data-sets.html}{datasets descriptions}.
#' Also of help is \code{Rcensus::get_dataset_names()} for available dataset acronym names and
#' their available years.
#'
#' @param dataset An optional string that sets the name of a dataset category of interest.
#' @param category An optional string that sets the category of datasets. The available
#'   categories are one of the following:
#'   \tabular{ll}{
#'     \strong{`acs1`} \tab American Community Survey 1-Year Data\cr
#'     \strong{`acs/acsse`} \tab American Community Survey Supplemental Data\cr
#'     \strong{`acs5`} \tab American Community Survey 5-Year Data\cr
#'     \strong{`dec`} \tab Decennial Census\cr
#'     \strong{`ecnbasic`} \tab Economy-Wide Key Statistics\cr
#'     \strong{`timeseries/idb`} \tab Time Series International Database\cr
#'     \strong{`abs`} \tab Annual Business Survey\cr
#'   }
#' @param vintage An required numeric that sets the year of interest. The default is 2020.
#' @param vars An optional vector of variable names whose descriptions are of interest.
#'   This parameter requires that either 'dataset' or 'category' had been defined.
#' @param group An optional string that sets the group name associated with a set of variables.
#'   This parameter requires that either 'dataset' or 'category' had been defined.
#'   See \code{Rcensus::get_groups()} for available group names under a specific dataset and vintage.
#' @param filter_group_est A logical which if \code{TRUE} will filter the variable names from 'group' and return
#'   only estimate related variable names. The default is \code{FALSE}.
#' @param filter_name_str A character string by which to filter the resultant \code{data.table}'s "name" column.
#' @param filter_label_str A character string by which to filter the resultant \code{data.table}'s "label" column.
#' @param filter_concept_str A character string by which to filter the resultant \code{data.table}'s "concept" column.
#' @param ignore_case A logical which if \code{FALSE} will not ignore case in filtering the "name", "label", "concept" column.
#' @param fixed A logical which if \code{TRUE}, then the above filter strings are used 'as is' in matching.
#'
#' @return A `data.table`
#'
#' @examples
#' # Get available variables that have the phrase "educational attainment"
#' # in "label" column of the resultant data.table.
#'
#' library(data.table)
#' library(httr2)
#' library(RcensusPkg)
#'
#' educational_attainment_2019_dt <- RcensusPkg::get_variable_names(
#'   dataset = "acs/acs1/profile",
#'   vintage = 2019,
#'   filter_label_str = "educational attainment"
#' )
#'
#' @import data.table
#' @importFrom httr2 request
#' @importFrom httr2 req_perform
#' @importFrom httr2 resp_body_json
#' @importFrom purrr map2
#'
#' @export
get_variable_names <- function(
  dataset = NULL,
  category = NULL,
  vintage = 2020,
  vars = NULL,
  group = NULL,
  filter_group_est = FALSE,
  filter_name_str = NULL,
  filter_label_str = NULL,
  filter_concept_str = NULL,
  ignore_case = TRUE,
  fixed = FALSE){

  if(is.null(dataset) & is.null(category)){
    stop("Please set either the 'dataset' or 'category' parameters.")
  }
  if(is.null(vintage)){
    stop("A vintage is required for get_variable_names()")
  }

  name <- NULL

  cb_datasets <- list(
    "acs1" = data.frame(
      code = c(
        "acs/acs1",
        "acs/acs1/subject",
        "acs/acs1/profile",
        "acs/acs1/cprofile",
        "acs/acs1/spp"
      ),
      name = c(
        "Detailed Tables",
        "Subject Tables",
        "Data Profiles",
        "Comparison Profile",
        "Selected Population Profiles"
      )
    ),
    "acs/acsse" = data.frame(
      code = c(
        "acs/acsse"
      ),
      name = c(
        "1-Year Supplemental Estimates"
      )
    ),
    "acs5" = data.frame(
      code = c(
        "acs/acs5",
        "acs/acs5/subject",
        "acs/acs5/profile",
        "acs/acs5/cprofile"
      ),
      name = c(
        "Detailed Tables",
        "Subject Tables",
        "Data Profiles",
        "Comparison Profiles"
      )
    ),
    "dec" = data.frame(
      code = c(
        "dec/sdhc",
        "dec/ddhcb",
        "dec/ddhca",
        "dec/dhc",
        "dec/dp"
      ),
      name = c(
        "Supplemental Demographic and Housing Characteristics",
        "Detailed Demographic and Housing Characteristics File B",
        "Detailed Demographic and Housing Characteristics File A",
        "Demographic and Housing Characteristics File",
        "Demographic Profile"
      )
    ),
    "ecnbasic" = data.frame(
      code = c(
        "ecnbasic"
      ),
      name = c(
        "Economy-Wide Key Statistics"
      )
    ),
    "timeseries/idb" = data.frame(
      code = c(
        "timeseries/idb/1year",
        "timeseries/idb/5year"
      ),
      name = c(
        "Time Series International Database by Single Year of Age and Sex",
        "Time Series International Database by 5-Year Age Groups and Sex"
      )
    ),
    "abs" = data.frame(
      code = c(
        "abscs",
        "abscb",
        "abscbo",
        "absmcb"
      ),
      name = c(
        "Company Summary",
        "Characteristics of Businesses",
        "Characteristics of Business Owners",
        "Module Business Characteristics"
      )
    )
  )

  return_dt <- NULL
  add_variable <- function(name, var) {
    concept <- NA
    if(!is.null(var$concept)){
      concept <- var$concept
    }
    required <- NA
    if(!is.null(var$required)){
      required <- var$required
    }
    predicateType = NA
    if(length(var$predicateType) > 0){
      predicateType <- var$predicateType
    }

    list(
      name = name,
      label = var$label,
      concept = concept,
      required = required,
      predicateType = predicateType
    )
  }
  make_request <- function(a_url, a_dataset){
    # Make a web request
    tryCatch({
      resp <- httr2::request(a_url) |> httr2::req_perform()
      content_json <- resp |> httr2::resp_body_json()
      variable_lst <- purrr::map2(names(content_json$variables), content_json$variables,  add_variable)
      dt <- data.table::rbindlist(variable_lst, fill = TRUE)
      dt[, dataset := a_dataset]
      return(dt)
    },error = function(err){
      stop("Error downloading raw json text: ", err$message, "\n")
    })
  }

  if(!is.null(dataset) & is.null(group)){
    a_url <- .get_url(dataset, vintage)
    a_url <- paste(a_url, "variables.json", sep = "/")
    return_dt <- make_request(a_url, dataset)
  }else if(!is.null(dataset) & !is.null(group)){
    a_url <- .get_url(dataset, vintage)
    a_url <- paste0(a_url, "/groups/", group, ".json")
    return_dt <- make_request(a_url, dataset)
  }else if(!is.null(category)){
    return_dt <- data.frame()
    codes <- cb_datasets[[category]]$code
    for(i in seq_along(codes)){
      a_url <- .get_url(codes[i], vintage)
      a_url <- paste(a_url, "variables.json", sep = "/")
      dt <- make_request(a_url, codes[i])
      return_dt <- rbind(return_dt, dt)
    }
  }

  # If variables were derived by group, do we filter their names
  #   to get just estimates and margin of error related variable names
  if(!is.null(group) & filter_group_est){
    return_dt <- return_dt[endsWith(name, "E"),] |>
      _[name != "NAME",]
  }

  # Order by name
  data.table::setorder(return_dt, name)

  # Look for specific variables?
  if(!is.null(vars)){
    return_dt <- return_dt[name %in% vars,]
  }

  # Filtering of "name" and/or "label", "concept" columns?
  if(!is.null(filter_name_str)){
    return_dt <- return_dt[grepl(filter_name_str, return_dt$name, ignore.case = ignore_case, fixed = fixed)]
  }
  if(!is.null(filter_label_str)){
    return_dt <- return_dt[grepl(filter_label_str, return_dt$label, ignore.case = ignore_case, fixed = fixed)]
  }
  if(!is.null(filter_concept_str)){
    return_dt <- return_dt[grepl(filter_concept_str, return_dt$concept, ignore.case = ignore_case, fixed = fixed)]
  }
  return(return_dt)
}
