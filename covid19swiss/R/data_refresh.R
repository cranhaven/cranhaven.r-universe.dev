#' Update the Package Datasets
#' @export
#' @description Checks if new data is available on the package dev version (Github).
#' In case new data is available the function will enable the user the update the datasets
#'
#' @examples
#' \donttest{
#'
#' update_swiss_data()
#'
#' }
update_swiss_data <- function(){
  flag <- FALSE

  current <- covid19swiss::covid19swiss


  git_df <- utils::read.csv("https://raw.githubusercontent.com/Covid19R/covid19swiss/master/csv/covid19swiss.csv",
                               stringsAsFactors = FALSE)




  git_df$date <- as.Date(git_df$date)



  if(!base::identical(current, git_df)){
    if(base::nrow(git_df) > base::nrow(current)){
      flag <- TRUE
    }
  }


  if(flag){
    q <- base::tolower(base::readline("Updates are available on the covid19swiss Dev version, do you want to update? n/Y"))

    if(q == "y" || q == "yes" || q == ""){

      base::tryCatch(
        expr = {
          devtools::install_github("covid19R/covid19swiss")

          base::message("The data was refresed, please restart your session to have the new data available")
        },
        error = function(e){
          message('Caught an error!')
          print(e)
        },
        warning = function(w){
          message('Caught an warning!')
          print(w)
        }

      )
    }
  } else {
    base::message("No updates are available")
  }


}

#' Get information about the datasets provided by covid19swiss
#'
#' @description Returns information about the datasets in this package for
#'   covid19R harvesting
#'
#' @return a data.frame of information about the datasets in this package
#' @export
#'
#' @examples
#' \donttest{
#'
#' get_info_covid19swiss()
#' }
#'
get_info_covid19swiss <- function(){
  data.frame(
    data_set_name = "covid19swiss",
    package_name = "covid19swiss",
    function_to_get_data = "refresh_covid19swiss*",
    data_details = "The 2019 Novel Coronavirus COVID-19 (2019-nCoV) Cases in Switzerland and Principality of Liechtenstein",
    data_url = "http://open.zh.ch/internet/justiz_inneres/ogd/de/daten.html/",
    license_url = "MIT",
    data_types = "total_tested, total_confirmed, new_hosp, current_hosp, current_icu, current_vent, total_recovered, total_death",
    location_types = "Canton of Switzerland, Principality of Liechtenstein",
    spatial_extent = "global",
    TRUE
  )
}


#' Refresh the 2019 Novel Coronavirus COVID-19 (2019-nCoV) Dataset in the Covid19R Project Format
#'
#' aily summary of the Coronavirus (COVID-19) pandemic cases in Switzerland by Canton
#' @export
#' @return A data.frame object
#' @source Specialist Unit for Open Government Data Canton of Zurich \href{https://www.zh.ch/de/politik-staat/opendata.html}{website}
#
#' @examples
#' \donttest{
#' # update the data
#' covid19_swiss <- refresh_covid19swiss()
#' }
#'

refresh_covid19swiss <- function(){
  df <- NULL
  df <- utils::read.csv("https://raw.githubusercontent.com/Covid19R/covid19swiss/master/csv/covid19swiss.csv",
                            stringsAsFactors = FALSE)

  # Unit tests
  if(nrow(df) == 0 || is.null(df)){
    stop("The imported object is empty, please check you connection")
  } else if(ncol(df) != 7){
    stop("The structure of the imported object is not valid")
  } else if(!all(names(df) %in% c("date", "location", "location_type", "location_code", "location_code_type", "data_type", "value"))){
    stop("The names of the imported object are not valid")
  }

  df$date <- as.Date(df$date)
  return(df)
}
