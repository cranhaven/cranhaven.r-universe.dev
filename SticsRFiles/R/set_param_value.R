#' @title Setting parameter value for different kinds of parameters
#'
#' @description Setting parameter value in a xml_document object
#'
#' @param xml_doc an xml_document object
#' @param param_name parameter name
#' @param param_value vector of parameter values, or a list of
#' @param parent_name parent node name or attribute name (optional)
#' @param parent_sel_attr parent attribute value (optional)
#' @param ids elements indices (optional)
#' @param show_xpath Print the xpath
#'
#' @return A logical vector with successes status return
#'
#' @examples
#' \dontrun{
#' xml_path <- file.path(get_examples_path(file_type = "xml"), "sols.xml")
#' sols_doc <- xmldocument(xml_path)
#' get_param_value(sols_doc, "argi")
#'
#  [1] 30.2 21.0 27.0 39.0  1.0 12.2 70.0 22.0  9.9 10.2 10.2 17.0 23.1 22.0
# [15] 27.0 30.7 0.1 27.3 25.0 10.2 25.0 28.6 36.0 29.0 10.2 21.2 22.2 13.0
# [29] 17.0 15.0 26.0 28.2 20.0
#'
#' # setting all argi parameters with the same value
#' set_param_value(sols_doc, "argi", 15)
#' get_param_value(sols_doc, "argi")
#'
#  [1] 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15
# [25] 15 15 15 15 15 15 15 15 15
#'
#' # setting specific values for some soils
#' set_param_value(sols_doc, "argi", c(30, 35),
#'   parent_name = "sol", parent_sel_attr = c("solcanne", "solbanane")
#' )
#' get_param_value(sols_doc, "argi")
#'
#  [1] 30 15 15 15 15 15 35 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15
# [25] 15 15 15 15 15 15 15 15 15
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
set_param_value <- function(xml_doc,
                            param_name,
                            param_value,
                            parent_name = NULL,
                            parent_sel_attr = NULL,
                            ...) {


  # Calling the for several parameters
  param_nb <- length(param_name)

  # For managing a vector of values
  if (!base::is.list(param_value)) param_value <- list(param_value)

  # Checking consistency between names and valuess dimensions
  if (length(param_value) != param_nb) {
    warning("Parameters names and list of values are no of the same length !")
    return(invisible(FALSE))
  }


  # Manage ids and show_xpath and remove them from the list
  ids <- NULL
  show_xpath <- FALSE

  # ... for getting : ids, show_xpath arguments
  dot_args <- list(...)
  dot_names <- names(dot_args)


  # Getting ids and show_xpath
  if ("ids" %in% dot_names) ids <- dot_args$ids
  if ("show_xpath" %in% dot_names) show_xpath <- dot_args$show_xpath

  # Loop over param number
  if (param_nb > 1) {
    ret <- vector("logical", param_nb)
    for (p in 1:param_nb) {
      ret[p] <- set_param_value(xml_doc,
        param_name = param_name[p],
        param_value = param_value[[p]],
        parent_name = parent_name,
        parent_sel_attr = parent_sel_attr,
        ...
      )
    }
    return(invisible(ret))
  }

  # back to scalar or vector
  param_value <- unlist(param_value)

  param_types <- get_param_type()

  if (is.numeric(parent_name)) {
    ids <- parent_name
    parent_name <- NULL
  }

  if (is.numeric(parent_sel_attr)) {
    ids <- parent_sel_attr
    parent_sel_attr <- NULL
  }

  param_type <- get_param_type(
    xml_doc, param_name,
    parent_name, parent_sel_attr
  ) # , ids)
  type <- param_type$type
  xpath <- param_type$xpath

  if (show_xpath) {
    message(xpath)
  }


  if (!is.element(type, param_types)) {
    warning(
      "Setting values failed for parameter : ",
      param_name,
      ", \nunknown parameter, mispelled, or not taken into account yet.\n"
    )
    # returning success status
    return(FALSE)
  }

  # TODO
  # Add treatment if type is  (tableau_entete, ou ta_entete)
  # How to add nodes tableau or intervention to set param_value
  # in following switch
  # add functions to return a tableau node or an intervention node
  # according to STICS version : get_xml_node(file, node_name) from
  # xml examples files


  values_nb <- length(param_value)
  nodes_nb <- length(get_nodes(xml_doc, xpath))
  # checking dimensions between nodes ids and nodes number for xpath
  if (base::is.null(ids) && values_nb > 1 && !values_nb == nodes_nb) {
    warning(
      param_name,
      "replacement values are not consistent with parameters number !"
    )
    # returning success status
    return(FALSE)
  }

  # TODO: see if could be simplified with a default case !
  switch(type,
    nodename = {
      value <- set_values(xml_doc, xpath, param_value, ids)
    },
    attr = {
      value <- set_attrs_values(xml_doc, xpath, "nom", param_value, ids)
    },
    attrname = {
      value <- set_attrs_values(xml_doc, xpath, param_name, param_value, ids)
    },
    param = {
      value <- set_values(xml_doc, xpath, param_value, ids)
    },
    option = {
      value <- set_attrs_values(xml_doc, xpath, "choix", param_value, ids)
    },
    table = {
      # check number if values
      if (length(param_value) > param_type$length) {
        stop("Too many values to set !")
      }
      value <- set_values(xml_doc, xpath, param_value, ids)
    },
    table2 = {
      if (length(param_value) > param_type$length) {
        stop("Too many values to set !")
      }
      value <- set_values(xml_doc, xpath, param_value, ids)
    },
    node_param = {
      value <- set_values(xml_doc, xpath, param_value, ids)
    },
    choix_param = {
      value <- set_attrs_values(xml_doc, xpath, "choix", param_value, ids)
    },
    node_node = {
      value <- set_values(xml_doc, xpath, param_value, ids)
    },
    node_option = {
      value <- set_attrs_values(xml_doc, xpath, "choix", param_value, ids)
    },
    form_option = {
      value <- set_attrs_values(xml_doc, xpath, "choix", param_value, ids)
    },
    node_table = {
      value <- set_values(xml_doc, xpath, param_value, ids)
    },
    node_attr = {
      value <- set_attrs_values(xml_doc, xpath, "nom", param_value, ids)
    },
    attr_attr = {
      value <- set_values(xml_doc, xpath, param_value, ids)
    },
    attr_attr2 = {
      value <- set_attrs_values(xml_doc, xpath, param_name, param_value, ids)
    },
    choix_attr = {
      value <- set_attrs_values(xml_doc, xpath, param_name, param_value, ids)
    },
    nodename_childs = {
      value <- set_values(xml_doc, xpath, param_value, ids)
    }
  )

  ret <- TRUE
  # unsuccessful replacement
  if (base::is.null(value)) ret <- FALSE

  return(invisible(ret))
}
