#' @title Get a STICS xml parameter type and xpath
#' from a request in an xml_document
#' @param xml_doc an xml_document object (created from an xml file)
#' @param param_name parameter name or a vector of names
#' @param parent_name xml parent node name
#' @param parent_sel_attr parent attribute value for selecting
#' @param id node identifier(s) (if multiple nodes) for selecting
#'
#' @return a list with fields $type, $xpath and $length, or a named list of
#'
#' @examples
#' \dontrun{
#' xml_path <- file.path(get_examples_path(file_type = "xml"), "sols.xml")
#' sols_doc <- xmldocument(xml_path)
#' get_param_type(sols_doc, "argi")
#' get_param_type(sols_doc, c("argi", "norg"))
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
# TODO : may be merged with get_params_types !
get_param_type <- function(xml_doc,
                           param_name,
                           parent_name = NULL,
                           parent_sel_attr = NULL,
                           id = NULL) {
  param_types <- c(
    "param", "option", "table", "table2",
    "choix_param", "node_node", "node_option", "node_table",
    "node_param", "form_option", "node_attr", "attr", "attrname",
    "attr_attr", "choix_attr", "nodename", "attr_attr2",
    "nodename_childs", "node_table_ent", "node_table_ent2"
  )


  char_to_filter <- c("-", ".", ":", "/", "%", "#", " ", "(", ")")

  # returning param types list if no arguments given
  if (!nargs()) {
    return(param_types)
  }

  # for multiple param names
  if (length(param_name) > 1) {
    param_types <- lapply(param_name, function(x) get_param_type(xml_doc, x))
    names(param_types) <- param_name
    return(param_types)
  }


  param_name <- param_name[1]
  parent_name <- parent_name[1]

  if (base::is.null(id)) {
    sel_id <- ""
  } else {
    sel_id <- paste0("[", id, "]")
  }


  # filtering requests on special character presence in param_name
  # implying they cannot be used as : node name or attribute name
  filter_name <- FALSE

  find_bad_char <- grep(paste(c("[", char_to_filter, "]"), collapse = ""),
                        param_name)

  if (length(find_bad_char)) {
    filter_name <- TRUE
  }


  # treatment of parameters with no dependencies
  if (base::is.null(parent_name) && base::is.null(parent_sel_attr)) {
    xpath_nodename_childs <- paste0("//", param_name, "//*")
    xpath_nodename <- paste0("//", param_name)
    xpath_attrname <- paste0("//*", "[@", param_name, "]")
    xpath_param <- paste0("//param[@nom=\"", param_name, "\"]")
    xpath_option <- paste0("//option[@nomParam=\"", param_name, "\"]")
    # for varietal options
    xpath_optionv <- paste0("//optionv[@nomParam=\"", param_name, "\"]")
    xpath_table <- paste0("//tableau/colonne[@nom=\"", param_name, "\"]")
    xpath_table2 <- paste0("//intervention/colonne[@nom=\"", param_name, "\"]")
    # ajout des en-tete si pas noeud tableau (sols.xml)
    xpath_ent <- paste0("//tableau_entete/colonne[@nom=\"", param_name, "\"]")
    # Ajout des en-tete si pas de noeuds intervention (fichier _tec.xml)
    xpath_ent2 <- paste0("//ta_entete/colonne[@nom=\"", param_name, "\"]")


    # if no bad charaters in param_name
    if (!filter_name) {
      attr_values <- get_attrs_values(xml_doc, xpath_nodename, "nom")
      if (!base::is.null(attr_values)) {
        return(list(
          type = "attr",
          xpath = xpath_nodename,
          length = 1,
          attr = "nom"))
      }

      attr_values <- get_attrs_values(xml_doc, xpath_nodename, "dominance")
      if (!base::is.null(attr_values)) {
        return(list(
          type = "attr",
          xpath = xpath_nodename,
          length = 1,
          attr = "dominance"))
      }

      # new case for getting values of the node childs
      values <- get_values(xml_doc, xpath_nodename_childs)
      if (!base::is.null(values)) {
        return(list(
          type = "nodename_childs", xpath = xpath_nodename_childs,
          length = length(values)
        ))
      }

      # or get_nodes ?
      values <- get_values(xml_doc, xpath_nodename)
      if (!base::is.null(values)) {
        return(list(
          type = "nodename",
          xpath = xpath_nodename,
          length = length(values)))
      }

      # for example for : nb_interventions
      attr_values <- get_attrs_values(xml_doc, xpath_attrname, param_name)
      if (!base::is.null(attr_values)) {
        return(list(
          type = "attrname",
          xpath = xpath_attrname,
          length = 1,
          attr = param_name))
      }
    }

    attr_values <- get_attrs_values(xml_doc, xpath_param, "nom")
    if (!base::is.null(attr_values)) {
      return(list(type = "param", xpath = xpath_param, length = 1))
    }

    attr_values <- get_attrs_values(xml_doc, xpath_option, "nomParam")
    if (!base::is.null(attr_values)) {
      return(list(type = "option", xpath = xpath_option, length = 1))
    }

    attr_values <- get_attrs_values(xml_doc, xpath_optionv, "nomParam")
    if (!base::is.null(attr_values)) {
      return(list(type = "optionv", xpath = xpath_option, length = 1))
    }

    attr_values <- get_attrs_values(xml_doc, xpath_table, "nom")
    if (!base::is.null(attr_values)) {
      return(list(
        type = "table",
        xpath = xpath_table,
        length = length(attr_values)))
    }

    attr_values <- get_attrs_values(xml_doc, xpath_table2, "nom")
    if (!base::is.null(attr_values)) {
      return(list(
        type = "table2",
        xpath = xpath_table2,
        length = length(attr_values)))
    }

    attr_values <- get_attrs_values(xml_doc, xpath_ent, "nom")
    if (!base::is.null(attr_values)) {
      return(list(
        type = "node_table_ent",
        xpath = xpath_ent,
        length = length(attr_values)))
    }

    attr_values <- get_attrs_values(xml_doc, xpath_ent2, "nom")
    if (!base::is.null(attr_values)) {
      return(list(
        type = "node_table_ent2",
        xpath = xpath_ent2,
        length = length(attr_values)))
    }

    return(list(type = "unknown", xpath = NULL, length = 0))
  }


  # node with attr value for @nom as param_name
  # and a parent name as @nom attr
  if (!base::is.null(parent_name)) {
    xpath_attr_attr <- paste0(
      "//*[@nom=\"", parent_name, "\"]",
      "//*[@nom=\"", param_name, "\"]", sel_id
    )
  }

  values <- get_values(xml_doc, xpath_attr_attr)
  if (!base::is.null(values)) {
    return(list(
      type = "attr_attr",
      xpath = xpath_attr_attr,
      length = length(values)))
  }

  # No special character in param_name
  if (!filter_name) {
    # path to nb_interventions ta node
    if (!base::is.null(parent_name)) {
      xpath_attr_attr <- paste0(
        "//*[@nom=\"", parent_name,
        "\"]", "//*[@", param_name, "]",
        sel_id
      )
    }

    values <- get_attrs_values(xml_doc, xpath_attr_attr, param_name)
    if (!base::is.null(values)) {
      return(list(
        type = "attr_attr2",
        xpath = xpath_attr_attr,
        length = length(values)))
    }
  }

  # node with attr value for any attr (@nom, @code) as param_name
  # with a link to a parent node selected with a @nomParam name
  if (!base::is.null(parent_name)) {
    xpath_choix_attr <- paste0(
      "//*", "[@nomParam=\"",
      parent_name, "\"]", "/choix",
      sel_id
    )
  }
  attr_values <- get_attrs_values(xml_doc, xpath_choix_attr, param_name)
  if (!base::is.null(attr_values)) {
    return(list(
      type = "choix_attr",
      xpath = xpath_choix_attr,
      length = length(attr_values)))
  }

  # No special character in param_name
  if (!filter_name) {
    # node with attr value for @nom as param_name
    if (!base::is.null(parent_name)) {
      xpath_node_attr <- paste0(
        "//", param_name,
        "[@nom=\"", parent_name, "\"]",
        sel_id
      )
    }

    attr_values <- get_attrs_values(xml_doc, xpath_node_attr, "nom")
    if (!base::is.null(attr_values)) {
      return(list(
        type = "node_attr",
        xpath = xpath_node_attr,
        ength = length(attr_values)))
    }
  }

  # param node, with parent node with possible selection on @nom
  new_parent_name <- parent_name

  if (!base::is.null(parent_sel_attr)) {
    new_parent_name <- paste0(parent_name, "[@nom=\"", parent_sel_attr, "\"]")
  }

  xpath_node_param <- paste0("//",
                             new_parent_name,
                             "//param[@nom=\"",
                             param_name, "\"]")

  values <- get_values(xml_doc, xpath_node_param)
  if (!base::is.null(values)) {
    return(list(
      type = "node_param",
      xpath = xpath_node_param,
      length = length(values)))
  }


  # treatment of param with parent dependency
  # parent node is a choix node
  xpath_choix_param <- paste0(
    "//choix[@nom=\"", parent_name, "\"]",
    "/param[@nom=\"", param_name, "\"]"
  )

  attr_values <- get_attrs_values(xml_doc, xpath_choix_param, "nom")
  if (!base::is.null(attr_values)) {
    return(list(type = "choix_param", xpath = xpath_choix_param, length = 1))
  }


  # parent is an option node -> pas utile dans ce cas nom param unique !
  # xpath_option_param



  # option node, with a parent formalisme node with fixed @nom

  new_parent_name <- parent_name

  if (!base::is.null(parent_name)) {
    # voir remplacer * par formalisme
    new_parent_name <- paste0("*", "[@nom=\"", parent_name, "\"]")
  }

  xpath_form_option <- paste0(
    "//", new_parent_name,
    "//option[@nomParam=\"", param_name, "\"]"
  )

  attr_values <- get_attrs_values(xml_doc, xpath_form_option, "choix")
  if (!base::is.null(attr_values)) {
    return(list(
      type = "form_option",
      xpath = xpath_form_option,
      length = 1))
  }


  # option node with a parent node with possible select with
  # @nom attribute value
  new_parent_name <- parent_name

  if (!base::is.null(parent_sel_attr)) {
    new_parent_name <- paste0(parent_name, "[@nom=\"", parent_sel_attr, "\"]")
  }

  xpath_node_option <- paste0("//",
                              new_parent_name,
                              "//option[@nomParam=\"",
                              param_name,
                              "\"]")

  attr_values <- get_attrs_values(xml_doc, xpath_node_option, "choix")
  if (!base::is.null(attr_values)) {
    return(list(
      type = "node_option",
      xpath = xpath_node_option,
      length = length(attr_values)))
  }


  # option node with a parent node with possible select with
  # @dominance attribute value (ini files, ...)
  new_parent_name <- parent_name

  if (!base::is.null(parent_sel_attr)) {
    new_parent_name <- paste0(
      parent_name,
      "[@dominance=\"",
      parent_sel_attr,
      "\"]")
  }

  xpath_node_option <- paste0(
    "//",
    new_parent_name,
    "//option[@nomParam=\"",
    param_name,
    "\"]")

  attr_values <- get_attrs_values(xml_doc, xpath_node_option, "choix")
  if (!base::is.null(attr_values)) {
    return(list(
      type = "node_option",
      xpath = xpath_node_option,
      length = length(attr_values)))
  }



  # simple node with possible simple node parent (ini files, ...)
  # with possible select with @nom attribute value
  # No special character in param_name
  if (!filter_name) {
    new_parent_name <- parent_name

    if (!base::is.null(parent_sel_attr)) {
      new_parent_name <- paste0(parent_name, "[@nom=\"", parent_sel_attr, "\"]")
    }

    xpath_node_node <- paste0("//", new_parent_name, "//", param_name)

    values <- get_values(xml_doc, xpath_node_node)
    if (!base::is.null(values)) {
      return(list(
        type = "node_node",
        xpath = xpath_node_node,
        length = length(values)))
    }


    new_parent_name <- parent_name
    # with possible select with @dominance attribute value
    if (!base::is.null(parent_sel_attr)) {
      new_parent_name <- paste0(
        parent_name, "[@dominance=\"",
        as.character(parent_sel_attr), "\"]"
      )
    }

    # param node name is parent of target nodes
    xpath_node_node <- paste0(
      "//",
      new_parent_name,
      "//",
      param_name,
      "/horizon")

    values <- get_values(xml_doc, xpath_node_node)
    if (!base::is.null(values)) {
      return(list(
        type = "node_node",
        xpath = xpath_node_node,
        length = length(values)))
    }


    new_parent_name <- parent_name
    # with possible select with @dominance attribute value
    if (!base::is.null(parent_sel_attr)) {
      new_parent_name <- paste0(
        parent_name, "[@dominance=\"",
        as.character(parent_sel_attr), "\"]"
      )
    }

    xpath_node_node <- paste0("//", new_parent_name, "//", param_name)

    values <- get_values(xml_doc, xpath_node_node)
    if (!base::is.null(values)) {
      return(list(
        type = "node_node",
        xpath = xpath_node_node,
        length = length(values)))
    }
  }





  # tableau/colonne node with a possible parent simple node,
  #possible selection with @nom
  new_parent_name <- parent_name

  if (!base::is.null(parent_sel_attr)) {
    new_parent_name <- paste0(parent_name, "[@nom=\"", parent_sel_attr, "\"]")
  }

  xpath_node_table <- paste0(
    "//",
    new_parent_name,
    "//tableau/colonne[@nom=\"",
    param_name,
    "\"]")

  values <- get_values(xml_doc, xpath_node_table)
  if (!base::is.null(values)) {
    return(list(
      type = "node_table",
      xpath = xpath_node_table,
      length = length(values)))
  }

  # intervention/colonne node with a possible parent simple node,
  # possible selection with @nom
  new_parent_name <- parent_name

  if (!base::is.null(parent_sel_attr)) {
    new_parent_name <- paste0(parent_name, "[@nom=\"", parent_sel_attr, "\"]")
  }

  xpath_node_table2 <- paste0(
    "//",
    new_parent_name,
    "//intervention/colonne[@nom=\"", param_name, "\"]")

  values <- get_values(xml_doc, xpath_node_table2)
  if (!base::is.null(values)) {
    return(list(
      type = "node_table2",
      xpath = xpath_node_table2,
      length = length(values)))
  }


  # pour en tete
  new_parent_name <- parent_name
  xpath_node_table <- paste0(
    "//",
    new_parent_name,
    "//ta_entete/colonne[@nom=\"",
    param_name,
    "\"]")

  values <- get_values(xml_doc, xpath_node_table)
  if (!base::is.null(values)) {
    return(list(
      type = "node_table_ent",
      xpath = xpath_node_table,
      length = length(values)))
  }

  return(NULL)
}
