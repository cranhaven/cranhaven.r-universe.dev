#' @title Setting parameter values into xml files
#'
#' @description Setting parameter values for a parameter or a vector of and
#' with a parameters values vector
#'
#' @param file Path (including name) of the xml file to modify
#' @param param Vector of parameter names.
#' @param values A vector or a list of parameter(s) values (see details).
#' @param save_as Path (including name) of the xml file to generate.
#' Optional, if NULL `file` is overwritten.
#' @param select node name or attribute name to use for selection
#' (optional, default to no selection)
#' @param select_value Vector of values used for select (see examples).
#' Optional, should be provided only if select is provided.
#' @param value_id Vector of ids of the parameters values to be retrieved
#' from the parameter values vector
#' @param overwrite Logical TRUE for overwriting the output file,
#' FALSE otherwise (default)
#' @param xml_file `r lifecycle::badge("deprecated")` `xml_file` is no
#'   longer supported, use file instead.
#' @param out_path `r lifecycle::badge("deprecated")` `out_path` is no
#'   longer supported, use save_as instead.
#' @param param_name `r lifecycle::badge("deprecated")` `param_name` is no
#'   longer supported, use param instead.
#' @param param_value `r lifecycle::badge("deprecated")` `param_value` is no
#'   longer supported, use values instead.
#' @param value `r lifecycle::badge("deprecated")` `value` is no
#'   longer supported, use select_value instead.
#' @param... Pass further arguments to `set_param_value()`.
#'
#' @return A logical value TRUE for operation success, FALSE otherwise
#'
#' @details It is possible to give several values for a parameter by passing
#' a vector of values. For example, for two parameters with two values each:
#' value= list(c(1,2), c(2.3,4.5))
#'
#' @examples
#'
#' ex_path <- get_examples_path(file_type = "xml")
#'
#' # Soil file
#'
#' sol_path <- file.path(ex_path, "sols.xml")
#'
#' # For scalar parameters per soil
#' # Setting all soils "argi" values to 50
#' set_param_xml(sol_path, "argi", 50, overwrite = TRUE)
#' # Getting changed values
#' # get_param_xml(sol_path, "argi")
#'
#' # Setting a specific value to "argi" for "solcanne" soil
#' set_param_xml(file = sol_path, param = "argi", values = 56,
#'    select = "sol", select_value = "solcanne", overwrite = TRUE
#' )
#' # Getting changed values
#' # get_param_xml(sol_path, "argi",
#' #   select = "sol", select_value = "solcanne"
#' #)
#'
#'
#' # Setting a specific values to 2 parameters "argi" and
#' # "norg" for "solcanne" soil
#' set_param_xml(sol_path, c("argi", "norg"), list(100, 150),
#'   select = "sol", select_value = "solcanne", overwrite = TRUE
#' )
#' # Getting changed values
#' # get_param_xml(sol_path, c("argi", "norg"),
#' #   select = "sol", select_value = "solcanne"
#' #)
#'
#'
#' # For vector parameters per soil (5 values, one per soil layer)
#' set_param_xml(sol_path, c("epc", "HCCF"),
#'   select = "sol",
#'   select_value = c("solcanne", "solbanane"),
#'   values = list(c(20:24, 10:14), c(50:54, 40:44)),
#'   overwrite = TRUE
#' )
#'
#' # Getting changed values
#' # get_param_xml(sol_path, c("epc", "HCCF"),
#' # select = "sol",
#' # select_value = c("solcanne", "solbanane")
#' # )
#'
#' # For specific values of vector parameters
#' set_param_xml(sol_path, "HCCF",
#'   select = "sol",
#'   select_value = "solcanne",
#'   values = c(46.8, 48.5, 50.1),
#'   value_id = c(1,3,5),
#'   overwrite = TRUE
#'  )
#'
#' # Getting changed values
#' # get_param_xml(sol_path, "HCCF",
#' # select = "sol",
#' # select_value = "solcanne",
#' # value_id = c(1,3,5)
#' # )
#'
#' # Crop management file
#'
#' tec_path <- file.path(ex_path, "file_tec.xml")
#'
#' # Modifying irrigations parameters
#' set_param_xml(tec_path, c("julapI_or_sum_upvt", "amount"),
#'   values = list(200:215, 20:35), overwrite = TRUE
#' )
#' # Getting changed values
#' # get_param_xml(tec_path, c("julapI_or_sum_upvt", "amount"))
#'
#'
#' @export
set_param_xml <- function(file,
                          param,
                          values,
                          save_as = NULL,
                          select = NULL,
                          select_value = NULL,
                          value_id = NULL,
                          overwrite = FALSE,
                          xml_file = lifecycle::deprecated(),
                          out_path = lifecycle::deprecated(),
                          param_name = lifecycle::deprecated(),
                          param_value = lifecycle::deprecated(),
                          value = lifecycle::deprecated(),
                          ...) {

  # ... argument for passing : ids, show_xpath to get_param_value
  if (lifecycle::is_present(xml_file)) {
    lifecycle::deprecate_warn("1.0.0",
                              "set_param_xml(xml_file)",
                              "set_param_xml(file)")
  } else {
    xml_file <- file # to remove when we update inside the function
  }
  if (lifecycle::is_present(out_path)) {
    lifecycle::deprecate_warn("1.0.0",
                              "set_param_xml(out_path)",
                              "set_param_xml(save_as)")
  } else {
    out_path <- save_as # to remove when we update inside the function
  }
  if (lifecycle::is_present(param_name)) {
    lifecycle::deprecate_warn("1.0.0",
                              "set_param_xml(param_name)",
                              "set_param_xml(param)")
  } else {
    param_name <- param # to remove when we update inside the function
  }
  if (lifecycle::is_present(param_value)) {
    lifecycle::deprecate_warn("1.0.0",
                              "set_param_xml(param_value)",
                              "set_param_xml(values)")
  } else {
    param_value <- values # to remove when we update inside the function
  }
  if (lifecycle::is_present(value)) {
    lifecycle::deprecate_warn("1.0.0",
                              "set_param_xml(value)",
                              "set_param_xml(select_value)")
  } else {
    value <- select_value # to remove when we update inside the function
  }


  # Setting output file path
  if (base::is.null(out_path)) {
    out_path <- xml_file
  }


  # Checking output directory
  if (!dir.exists(dirname(path = out_path))) {
    stop("The output directory does not exist: ", dirname(path = out_path))
  }

  # Ckecking if file exists and overwriting right
  if (base::file.exists(out_path) && !overwrite) {
    warning(paste(
      "The file already exists, ",
      "set overwrite argument to TRUE or delete the file: ",
      out_path
    ))
    return(invisible(FALSE))
  }


  # For future version
  # TODO: multiple files and multiple params list and values ...ids ...?
  xml_doc <- xmldocument(xml_file)



  # Checking if any of param_name can be in intervention
  # nodes of 2 option choices (specific of "cut crop" in tec files)
  check_choice_param(
    xml_doc = xml_doc,
    param_name = param_name,
    stop = TRUE
  )


  # Setting parameters values in the xmlDoxument object
  set_param_value(xml_doc,
                  param_name = param_name,
                  param_value = param_value,
                  parent_name = select,
                  parent_sel_attr = value,
                  ids = value_id,
                  ...
  )


  # Saving
  save_xml_doc(xml_doc, out_path)

  delete(xml_doc)

  # Output status
  return(invisible(TRUE))
}
