#' @title Getting parameter values from xml files
#'
#' @description Extracting parameter values for a list of xml files and
#' parameters
#'
#' @param file Vector of the xml file paths from which parameters values
#' must be extracted
#' @param xml_file `r lifecycle::badge("deprecated")` `xml_file` is no
#'   longer supported, use `file` instead.
#' @param param Vector of parameter names. Optional, if not provided, the
#' function returns information for all parameters.
#' @param param_name `r lifecycle::badge("deprecated")` `param_name` is no
#'   longer supported, use `param` instead.
#' @param select node name or attribute name to use for selection
#' (optional, default to no selection)
#' @param select_value Vector of values used for select (see examples).
#' Optional, should be provided only if select is provided.
#' @param value_id Vector of ids of the parameters values to be retrieved
#' from the parameter values vector
#' @param value `r lifecycle::badge("deprecated")` `value` is no
#'   longer supported, use `select_value` instead.
#' @param ... Pass further arguments to `get_param_value()`
#'
#' @return A list of parameter values for each xml_file (a list of list)
#'
#' @examples
#'
#' # Soil file
#' file <- file.path(get_examples_path(file_type = "xml"), "sols.xml")
#'
#' # For all soils
#' get_param_xml(file)
#' get_param_xml(file, c("argi", "norg"))
#'
#' # With soil selection
#' # scalar parameters per soil
#' get_param_xml(file, c("argi", "norg"),
#'   select = "sol", select_value = c("solcanne", "solbanane")
#' )
#'
#' # Crop management file
#' file <- file.path(get_examples_path(file_type = "xml"), "file_tec.xml")
#'
#' # Getting parameters for irrigation (date and quantity)
#' get_param_xml(file, c("julapI_or_sum_upvt", "amount"))
#'
#'
#' @export
get_param_xml <- function(file,
                          param = NULL,
                          select = NULL,
                          select_value = NULL,
                          value_id = NULL,
                          xml_file = lifecycle::deprecated(),
                          param_name = lifecycle::deprecated(),
                          value = lifecycle::deprecated(),
                          ...) {
  # ... argument for passing : ids, show_xpath to get_param_value

  # Managing parameter names changes between versions:
  if (lifecycle::is_present(xml_file)) {
    lifecycle::deprecate_warn("1.0.0",
                              "get_param_xml(xml_file)",
                              "get_param_xml(file)")
  } else {
    xml_file <- file # to remove when we update inside the function
  }

  if (lifecycle::is_present(param_name)) {
    lifecycle::deprecate_warn("1.0.0",
                              "get_param_xml(param_name)",
                              "get_param_xml(param)")
  } else {
    param_name <- param # to remove when we update inside the function
  }

  if (lifecycle::is_present(value)) {
    lifecycle::deprecate_warn("1.0.0",
                              "get_param_xml(value)",
                              "get_param_xml(select_value)")
  } else {
    value <- select_value # to remove when we update inside the function
  }

  xml_docs <- lapply(xml_file, xmldocument)

  # Checking if any param duplicates in tec files, for 'cut crop' choices
  lapply(
    xml_docs,
    function(x) {
      check_choice_param(
        xml_doc = x,
        param_name = param_name
      )
    }
  )

  values <- get_param_value(
    xml_doc = xml_docs,
    param_name = param_name,
    parent_name = select,
    parent_sel_attr = value,
    ids = value_id,
    ...
  )
  xml_names <- lapply(xml_file, basename) %>% unlist()

  # If there are duplicated names in xml_file:
  is_duplicated_name <- xml_names %>% duplicated()
  xml_names[is_duplicated_name] <- paste0("xml_",
                                          which(is_duplicated_name == TRUE),
                                          "_",
                                          xml_names[is_duplicated_name])

  # Fixing parameters with no values with NA
  values[[1]] <- lapply(values[[1]],
                        function(x) {
                          if (length(x) == 0) return(NA)
                          x
                          })


  names(values) <- xml_names

  lapply(xml_docs, delete)

  return(values)
}
