#' @title Get a set of a (or a list of) STICS xml parameter(s)
#' values from a request
#' @title in an xml_document or a list of
#' @title Getting parameter value for different kinds of parameters
#'
#' @description Extracting parameter value from an xml document object
#'
#' @param xml_doc an xml_document object
#' @param param_name parameter name or a vector of names (optional)
#' @param parent_name parent node name or attribute name (optional)
#' @param parent_sel_attr parent attribute value (optional)
#' @param ... To pass some other arguments
#'
#' @return a numeric vector values of parameter or a list of
#'
#' @examples
#' \dontrun{
#' xml_path <- file.path(get_examples_path(file_type = "xml"), "sols.xml")
#' sols_doc <- xmldocument(xml_path)
#' get_param_value(sols_doc, "argi")
#' get_param_value(sols_doc, c("argi", "norg"))
#'
#' get_param_value(sols_doc, "argi",
#'   parent_name = "sol", parent_sel_attr = "solcanne"
#' )
#'
#' get_param_value(sols_doc, c("argi", "norg"),
#'   parent_name = "sol", parent_sel_attr = c("solcanne", "solbanane")
#' )
#'
#' get_param_value(list(sols_doc, sols_doc), c("argi", "norg"),
#'   parent_name = "sol", parent_sel_attr = c("solcanne", "solbanane")
#' )
#' }
#'
#' @keywords internal
#'
#' @noRd
#'

get_param_value <- function(xml_doc,
                            param_name = NULL,
                            parent_name = NULL,
                            parent_sel_attr = NULL,
                            ...) {



  # ... for getting : ids, show_xpath and mult_par arguments
  dot_args <- list(...)
  dot_names <- names(dot_args)

  # Getting ids and show_xpath
  if ("ids" %in% dot_names) ids <- dot_args$ids else ids <- NULL
  if ("show_xpath" %in% dot_names)
    show_xpath <- dot_args$show_xpath else show_xpath <- FALSE
  if ("to_num" %in% dot_names)
    to_num <- dot_args$to_num else to_num <- TRUE


  # Getting param values for the same parameters for the xml documents list
  if (base::is.list(xml_doc)) {
    values <- lapply(xml_doc, function(x) {
      get_param_value(
        xml_doc = x,
        param_name = param_name,
        parent_name = parent_name,
        parent_sel_attr = parent_sel_attr,
        ...
      )
    })
    return(values)
  }

  # If no given parameters names
  if (base::is.null(param_name)) {
    param_name <- get_param_names(xml_doc,
                                  parent_name = parent_name,
                                  parent_sel_attr = parent_sel_attr)

    # testing if param_name is empty
    if (is.null(param_name))
      stop(paste0("Not any parameter names detected for: \n",
                  parent_name, " = ", parent_sel_attr))

  }

  # If not any names given
  if (is.null(param_name)) stop("Parameter(s) name(s) must be given !")

  # Setting multiple parameters names flag
  if ("mult_par" %in% dot_names)
    mult_par <- dot_args$mult_par else mult_par <- FALSE


  # recursive call for a list of parameter names
  if (length(param_name) > 1) {
    param_value <- lapply(param_name, function(x) {
      get_param_value(
        xml_doc = xml_doc,
        param_name = x,
        parent_name = parent_name,
        parent_sel_attr = parent_sel_attr,
        mult_par = TRUE,
        ...
      )
    })

    sel_values <- !unlist(lapply(param_value, base::is.null))
    param_value <- param_value[sel_values]
    names(param_value) <- param_name[sel_values]
    return(param_value)
  }

  # Getting param type and path
  param_type <- get_param_type(
    xml_doc = xml_doc,
    param_name = param_name,
    parent_name = parent_name,
    parent_sel_attr = parent_sel_attr,
    id = ids
  )
  type <- param_type$type
  xpath <- param_type$xpath

  value <- NULL

  if (show_xpath) {
    message(xpath)
  }

  if (base::is.null(xpath)) {
    return(value)
  }



  # TODO: see if it could be simplified with a default case !
  switch(type,
         nodename = {
           value <- get_values(xml_doc, xpath, ids)
         },
         attr = {
           value <- get_attrs_values(xml_doc, xpath, param_type$attr, ids)
         },
         attrname = {
           value <- get_attrs_values(xml_doc, xpath, param_type$attr, ids)
         },
         param = {
           value <- get_values(xml_doc, xpath, ids)
         },
         option = {
           value <- get_attrs_values(xml_doc, xpath, "choix", ids)
         },
         table = {
           value <- get_values(xml_doc, xpath, ids)
         },
         table2 = {
           value <- get_values(xml_doc, xpath, ids)
         },
         node_param = {
           value <- get_values(xml_doc, xpath, ids)
         },
         choix_param = {
           value <- get_values(xml_doc, xpath, ids)
         },
         node_node = {
           value <- get_values(xml_doc, xpath, ids)
         },
         node_option = {
           value <- get_attrs_values(xml_doc, xpath, "choix", ids)
         },
         form_option = {
           value <- get_attrs_values(xml_doc, xpath, "choix", ids)
         },
         node_table = {
           value <- get_values(xml_doc, xpath, ids)
         },
         node_table2 = {
           value <- get_values(xml_doc, xpath, ids)
         },
         node_attr = {
           value <- get_attrs_values(xml_doc, xpath, "nom", ids)
         },
         attr_attr = {
           value <- get_values(xml_doc, xpath, ids)
         },
         attr_attr2 = {
           value <- get_attrs_values(xml_doc, xpath, param_name, ids)
         },
         choix_attr = {
           value <- get_attrs_values(xml_doc, xpath, param_name, ids)
         },
         nodename_childs = {
           value <- get_values(xml_doc, xpath, ids)
         }


         # TODO : add other cases for tables in ini, soil,
         # and other specific parameters
  )


  # Converting value to numeric if not any character in it
  # numbers may contain scientific notation e+ e-, decimal, and space
  if (to_num) {
    num_value <- suppressWarnings(as.numeric(value))
    is_number <- suppressWarnings(!is.na(num_value))

    # conversion to numeric
    if (all(is_number)) {
      value <- num_value
    }
  }

  value <- suppressWarnings(as.vector(value))

  # Converting value to a names list for only one parameter request
  if (!mult_par) {
    value <- list(value)
    names(value) <- param_name
  }


  return(value)
}



get_param_parent <- function(args_list) {
  parent_name <- NULL
  parent_sel_attr <- NULL

  if (length(args_list) > 1) stop("Too much args !")


  # Argument names to be detected
  parent_names <- c("sol", "plante", "variete", "usm")

  parent_list <- list(
    parent_name = parent_name,
    parent_sel_attr = parent_sel_attr
  )

  parent_idx <- parent_names %in% names(args_list)

  if (!any(parent_idx)) {
    return(parent_list)
  }

  parent_list$parent_name <- parent_names[parent_idx]
  parent_list$parent_sel_attr <- as.character(args_list[[1]])

  return(parent_list)
}
