#' @title pickerVar function
#' @description Function to generate a picker input element based on given id and element.
#' @param id A string representing the id of the input element.
#' @param element A string representing the element.
#' @param df_categories A dataframe with metadata about the available categories per picker element
#' @param label A string representing the label of the input. Default is NULL.
#' @return A pickerInput object.
pickerVar <- function(id, element, df_categories, label = NULL) {
  Categorie <- NULL

  id_element <- shiny::NS(id, element)

  ## Select desired variables for the selection element based on metadata and presence in df
  lVariables <- df_categories %>%
    dplyr::filter(!is.na(!!rlang::sym(id_element))) %>%
    dplyr::arrange(!!rlang::sym(id_element)) %>%
    ## set Categorie based upon the order in the id_element column
    ## Zie: https://stackoverflow.com/a/61503816/6375668
    dplyr::mutate(Categorie = stats::reorder(factor(Categorie), !!rlang::sym(id_element))) %>%
    ## Get variables and put them in named list per category
    dplyr::group_split(Categorie) %>%
    purrr::set_names(purrr::map_chr(., ~ .x$Categorie[1] %>% as.character())) %>%
    purrr::map(~ .x %>%
      dplyr::pull(Veldnaam) %>%
      as.list()) %>%
    ## Iterate over elements
    purrr::map(
      ~ purrr::map(
        ## check if element is present and correctly formed
        .x, ~ purrr::keep(.x, present_and_correct(.x, element, df = df)) %>%
          ## set the display name per element
          purrr::set_names(display_name(.x, id))
        ## Remove all empty elements
      ) %>%
        purrr::compact() %>%
        unlist()
    )


  ## Set the selected value based on the element
  if (element == "y_rechts") {
    selected <- lVariables[[1]][[2]]
  } else {
    selected <- lVariables[[1]][[1]]
  }

  if (is.null(label)) {
    label <- stringr::str_to_title(element)
  }

  shinyWidgets::pickerInput(
    inputId = id_element,
    label = label,
    choices = lVariables,
    selected = selected,
    options = list(`live-search` = TRUE)
  )
}


#' @title pickerSplitVar function
#' @description Function to create a picker input for splitting variables.
#' @param id A string representing the id of the input element.
#' @param variable A string representing the variable to split. Default is "INS_Splits_variabele".
#' @param name A string representing the name. Default is "color".
#' @param label A string representing the label of the input. Default is "Kleur".
#' @param df A data frame containing the data. Default is dfCombi_geaggregeerd.
#' @return A pickerInput object.
pickerSplitVar <- function(id, variable = "INS_Splits_variabele", name = "color", label = "Kleur", df) {
  ## Create a named list with unique values as names and the combination of unique value and column name as value
  choices <- sort(unique(df[[variable]])) %>%
    purrr::discard(~ .x == "Alle") %>%
    purrr::map(~ paste(.x, variable, sep = ";")) %>%
    purrr::set_names(~ map_chr(.x, ~ display_name(.x, id)))

  inputId <- paste(id, name, sep = "-")

  shinyWidgets::pickerInput(
    inputId = inputId,
    label = label,
    choices = choices,
    options = list(
      `live-search` = TRUE
    )
  )
}


#' @title pickerValues function
#' @description Function to create a picker input for filtering value.
#' @param id A string representing the id of the input element.
#' @param df A data frame containing the data.
#' @param variable A string representing the variable to filter. Default is "faculty".
#' @param role A string representing the role. Default is "left".
#' @param selected The selected value. Default is "All".
#' @param multiple A boolean indicating whether multiple selections are allowed. Default is TRUE.
#' @return A pickerInput object.
pickerValues <- function(id, df, variable = "faculty", role = "left", selected = "All", multiple = TRUE) {
  ns <- shiny::NS(id)

  inputId <- ns(paste(variable, role, sep = "_"))

  ## Convert user-friendly variable names to appropriate column names
  if (variable == "faculty") {
    variable <- "INS_Faculteit"
    variable_name <- "Faculteit"
  } else if (variable == "phase") {
    variable <- "INS_Opleidingsfase_BPM"
    variable_name <- "Fase"
  } else if (variable == "opleiding") {
    variable <- "INS_Opleidingsnaam_2002"
    variable_name <- "Opleiding"
  } else if (variable == "cohort") {
    variable <- "INS_Inschrijvingsjaar_EOI"
    variable_name <- "Cohort"
  } else {
    variable_name <- variable
  }

  label <- variable_name

  ## Create a named list with unique values as names
  choices <- sort(unique(df[[variable]])) %>%
    purrr::set_names(.) %>%
    purrr::map(~ paste(.x, variable, sep = ";"))


  if (length(selected) == 1 && selected == "All") {
    selected <- choices
  }

  ## Transform the string all to all choices
  shinyWidgets::pickerInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    options = list(
      `actions-box` = TRUE,
      `deselect-all-text` = "Geen",
      `select-all-text` = "Alles",
      `none-selected-text` = "-- GEEN FILTER --",
      `live-search` = TRUE
    )
  )
}


#' @title present_and_correct function
#' @description Function to check if the column is present and correctly formed based on the element type.
#' @param column_name A string representing the column name.
#' @param element A string representing the element. Default is NA.
#' @param df A data frame for which to check the column. Default is dfCombi_geaggregeerd.
#' @return A boolean indicating whether the column is present and correctly formed.
present_and_correct <- function(column_name, element = NA, df) {
  present <- column_name %in% names(df)

  ## Controleer per type grafiek-element of de kolom voldoet
  correct_form <- switch(element,
    ##
    # "x" = length(unique(df[[column_name]])) < 15,
    "x" = TRUE,
    "y" = typeof(df[[column_name]]) %in% c("logical", "double", "integer") & class(df[[column_name]]) != "Date",
    "y_links" = typeof(df[[column_name]]) %in% c("logical", "double", "integer") & class(df[[column_name]]) != "Date",
    "y_rechts" = typeof(df[[column_name]]) %in% c("logical", "double", "integer") & class(df[[column_name]]) != "Date",
    # "color" = TRUE
    "color" = is.logical(df[[column_name]]) |
      is.integer(df[[column_name]]) & length(unique(df[[column_name]])) < 15 |
      is.character(df[[column_name]]) & length(unique(df[[column_name]])) < 15,
    "sankey" = is.logical(df[[column_name]]) |
      is.integer(df[[column_name]]) & length(unique(df[[column_name]])) < 15 |
      is.character(df[[column_name]]) & length(unique(df[[column_name]])) < 15
  )

  ## Set correct form to TRUE when the element is not defined
  if (is.null(correct_form)) {
    correct_form <- TRUE
  }

  ## Combine the two checks
  result <- present & correct_form

  return(result)
}
