#' @title Get the "formalisme" node "@nom" attribute value of a
#' (or a list of) parameter(s) in an xml_document, of a vector of
#' @param xml_doc an xml_document object (created from any xml file),
#' or a vector of
#' @param name Optional. A parameter name or a vector of. If not given,
#' all parameters names are extracted.
#'
#'
#' @return a list parameters formalism name
#'
#' @examples
#' \dontrun{
#'
#' xml_sta <- file.path(get_examples_path(file_type = "xml"), "file_sta.xml")
#'
#' sta_doc <- xmldocument(xml_sta)
#'
#' par_form <- get_param_formalisms(sta_doc, "zr")
#'
#' par_form_list <- get_param_formalisms(sta_doc,
#'                                                    c("zr", "altistation"))
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
#'
get_param_formalisms <- function(xml_doc, name = NULL, form_only = FALSE) {

  # For multiple documents
  if (is.list(xml_doc) && length(xml_doc) > 1) {
    names <- lapply(
      xml_doc,
      function(x) {
        get_param_formalisms(
          xml_doc = x,
          name = name,
          form_only = form_only
        )
      }
    )


    return(names)
  }

  # if one doc
  if (base::is.list(xml_doc)) xml_doc <- unlist(xml_doc)

  # If no parameter name is given
  if (base::is.null(name)) name <- get_param_names(xml_doc)


  # recursive call for a parameter name list
  if (length(name) > 1) {
    form_list <- lapply(name, function(x) {
      get_param_formalisms(xml_doc,
        x,
        form_only = form_only
      )
    })
    # if no formalism
    if (all(unlist(lapply(form_list, base::is.null)))) {
      form_list <- list(NA)
      return(form_list)
    }

    if (form_only) form_list <- unique(unlist(form_list, use.names = FALSE))

    return(form_list)
  }

  # case : param name as value of @nom attribute
  x_path <- paste0("//*[@nom=\"", name, "\"]//ancestor::formalisme")
  values <- param_formalism_elt(xml_doc, x_path, name)
  if (!all(base::is.null(values)) && !all(values == "none")) {
    return(values)
  }


  # case : param name as value of @nomParam attribute
  x_path <- paste0("//*[@nomParam=\"", name, "\"]//ancestor::formalisme")
  values <- param_formalism_elt(xml_doc, x_path, name)
  if (!all(base::is.null(values)) && !all(values == "none")) {
    return(values)
  }


  return(values)
}


param_formalism_elt <- function(xml_doc, xpath, name) {

  # Formatting a parameter formalism list unit
  values <- get_attrs_values(xml_doc, xpath, "nom")

  param_values <- get_param_value(xml_doc = xml_doc, param_name = name)

  if (base::is.null(values)) {
    # Fix: parameter exists but no formalism
    if (!base::is.null(param_values)) {
      values <- matrix("none")
      names(values) <- name
    }
    return(values)
  }


  # Just for an unnamed vector
  names(values[[1]]) <- NULL
  names(values) <- name

  return(values)
}
