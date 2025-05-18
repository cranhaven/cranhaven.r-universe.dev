#' Checking if any parameter appear in 2 option choices
#'
#' @param xml_doc an xml_document object
#' @param param_name parameter names vector, i.e.: parameter name or option code
#' @param stop TRUE for rising an error, FALSE for just warning
#'
#' @return invisible NULL
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' check_choice_param(xml_doc = xml_doc, param_name = param_name)
#' }
#'
check_choice_param <- function(xml_doc, param_name, stop = FALSE) {

  #--------------------------------------------------------------------#
  # This is for the moment a specific case attached to tec files:
  # parameter names attached to intervention nodes
  # might appear in 2 option choices for special techniques in "cut crop"
  #--------------------------------------------------------------------#

  # Early exiting for other docs than tec ones
  if (!XML::xmlName(XML::xmlRoot(xml_doc@content)) == "fichiertec") {
    return(invisible())
  }

  # Parameters related to cut crop
  choice_specif_par <- c("julfauche", "tempfauche")
  choice_common_par <- c("hautcoupe", "lairesiduel", "msresiduel", "anitcoupe")

  # Early exiting: not any parameters found in param_name
  if (!any(c(choice_specif_par, choice_common_par) %in% param_name)) {
    return(invisible())
  }

  # Detecting incompatible choices parameters
  par_idx <- choice_specif_par %in% param_name
  if (all(par_idx)) {
    message <- sprintf(
      "%s%s%s", "Parameters ", paste(choice_specif_par,
        collapse = ", "
      ),
      "\ncannot be used for different choices of the same option 'cut crop'"
    )
    if (stop) {
      stop(message)
    } else {
      warning(message)
    }
  }

  # Checking common parameter names
  par_idx <- choice_common_par %in% param_name
  if (!any(par_idx)) {
    return(invisible())
  }

  # Detecting duplicates in xml_doc for intervention nodes
  common_par_name <- choice_common_par[par_idx][1]

  # Getting all nodes intervention containing a common parameter
  common_par_path <- get_param_type(xml_doc, param_name = common_par_name)$xpath
  interv_nodes <- lapply(get_nodes(xml_doc, common_par_path), XML::xmlParent)
  interv_par_names <-
    unique(
      unlist(
        lapply(interv_nodes, function(x) XML::xmlSApply(x, FUN = XML::xmlAttrs))
      )
    )
  if (all(choice_specif_par %in% interv_par_names)) {
    par_list <- intersect(
      interv_par_names,
      setdiff(param_name, choice_specif_par)
    )
    message <- sprintf(
      "%s%s%s%s", "Impossible to get/set values for parameters: ",
      paste(par_list, collapse = ", "),
      "\nexisting in intervention nodes belonging",
      " to 2 choices of the same option 'cut crop'"
    )
    if (stop) {
      stop(message)
    } else {
      warning(message)
    }
  }

  invisible()
}
