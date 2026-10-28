#' @title
#' Get the Banff data dictionary
#'
#' @description
#' This function gets the data dictionary used to control the consistency of
#' the input dataset.
#'
#' @inheritParams get_banff_version
#' @param which Indicates which variables to get from the Banff data dictionary.
#' If NULL both input and output variables are provided.*
#' @param language Optional input allowing the user to get the diagnoses in a
#' specific language. Options are "label:en" (default), "label:fr", "label:de",
#' "label:sp", "label:nl", "label:jp", "label:in".
#' @param detail Optional argument indicating whether the data dictionary should
#' include temporary variables generated in the process or not.
#' FALSE by default.
#'
#' @return
#' A list of tibbles representing meta data used in the process. The metadata
#' are the list of variables used, and their associated categories, if any.
#'
#' @examples
#' {
#'
#'   get_banff_dictionary()
#'
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export
get_banff_dictionary <- function(
    version = NULL,which = NULL, language = "label:en",detail = FALSE){

  # get the version
  version <- get_banff_version(version)

  # creation of the data dictionary
  banff_dict <- get(paste0("banff_dict_",version),
                    envir = asNamespace("banffIT"))

  # check if the label provided by user is among the provided labels.
  labels <- str_subset(names(banff_dict$`Categories`),'^label')
  if(! tolower(language) %in% labels)
    stop(call. = FALSE,"

The possible `language` are : ",toString(labels))

      if(!toString(which) %in% c("","input","output")){
        stop(call. = FALSE,"
`which` must be NULL or a character string, either 'input' or 'output'")
  }

  # creation of the output data dictionary (extended if detail == TRUE)
  if(detail == FALSE){
    banff_dict <-
      banff_dict %>%
      data_dict_filter(filter_var = "output !='if detail = TRUE'")}

  # rename label:xx into label
  banff_dict$`Variables` <-
    banff_dict$`Variables` %>%
    rename("label" = "label:en") %>%
    select("index","name","label","valueType",everything())

  # rename label:xx into label
  banff_dict$`Categories` <-
    banff_dict$`Categories` %>%
    select("variable","name", "labels" = "label:en", "test" = all_of(language),"description") %>%
    mutate(labels = ifelse(is.na(.data$`test`),.data$`labels`,.data$`test`)) %>%
    select("variable","name","labels","description",everything(),-"test")


  if(is.null(which)){
    return(banff_dict)
  }

  if(toString(which) == "input"){
    # creation of the input data dictionary
    banff_dict_input <-
      banff_dict %>%
      data_dict_filter(filter_var = "input == 'yes'")

    banff_dict_input$`Variables` <-
      banff_dict_input$`Variables` %>%
      select(-c("input","output"))

    return(banff_dict_input)

  }


  if(toString(which) == "input"){
    # creation of the input data dictionary
    banff_dict_input <-
      banff_dict %>%
      data_dict_filter(filter_var = "input != 'no'")

    banff_dict_input$`Variables` <-
      banff_dict_input$`Variables` %>%
      select(-c("input","output"))

    return(banff_dict_input)

  }

  if(toString(which) == "output"){
    # creation of the input data dictionary
    banff_dict_output <-
      banff_dict %>%
      data_dict_filter(filter_var = "output != 'no'")

    banff_dict_output$`Variables` <-
      banff_dict_output$`Variables` %>%
      select(-c("input","output"))

    return(banff_dict_output)

  }

}

#' @title
#' Get a dataset example
#'
#' @description
#' This function gets the dataset used in the vignette as an example.
#'
#' @inheritParams get_banff_version
#'
#' @return
#' A tibble representing the dataset used in the the vignette as an example.
#'
#' @examples
#' {
#'
#'   get_banff_example()
#'
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export
get_banff_example <- function(version = NULL){

  # get the version
  version <- get_banff_version(version)

  # creation of the example
  banff_example <- get(paste0("banff_example_",version),
                       envir = asNamespace("banffIT"))

  return(banff_example)

}

#' @title
#' Get a dataset template
#'
#' @description
#' This function gets the empty dataset with variables that are mandatory in the
#' process.
#'
#' @inheritParams get_banff_version
#'
#' @return
#' A tibble representing the empty dataset.
#'
#' @examples
#' {
#'
#'   get_banff_template()
#'
#' }
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export
get_banff_template <- function(version = NULL){

  # get the version
  version <- get_banff_version(version)

  # creation of the template
  banff_template <- get(paste0("banff_template_",version),
                    envir = asNamespace("banffIT"))

  return(banff_template)

}

#' @title
#' Call the online documentation
#'
#' @description
#' This function sends the user to the online documentation for the package,
#' which includes a description of the latest version of the package, vignettes,
#' user guides, and a reference list of functions and help pages.
#'
#' @returns
#' Nothing to be returned. The function opens a web page.
#'
#' @examples
#' {
#'
#' banffIT_website()
#'
#' }
#'
#' @importFrom utils browseURL
#'
#' @export
banffIT_website <- function(){

  browseURL("https://PersonalizedTransplantCare.github.io/banffIT-documentation/")

}

#' @title
#' Extract version of the Banff classification
#'
#' @description
#' This internal function extracts for user input the version of the Banff
#' classification.
#'
#' @param version A character string referring the version of Banff classification.
#' The most recent classification is the default.
#' Options are "2022" (default), "2017".
#'
#' @returns
#' A character string which is the version of Banff classification.
#'
#' @examples
#' {
#'
#' get_banff_version()
#'
#' }
#'
#' @import dplyr
#'
#' @export
get_banff_version <- function(version = NULL){

  # check the version.
  versions_list <- c('2017','2022')
  if(is.null(version)) version <- last(versions_list)
  version <- toString(version)
  if(! version %in% versions_list)
    stop(call. = FALSE,"

The possible `version` values are : ",toString(versions_list))

  if(! version %in% last(versions_list))
    message("
A more recent version can be used (",last(versions_list),")")

  return(version)
}

