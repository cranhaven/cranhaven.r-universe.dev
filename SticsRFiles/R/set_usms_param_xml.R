#' @title Setting a usm param value(s) in an usms
#' @param xml_doc an  object (created from an usms file)
#'
#' @param usms_param usms parameters (data.frame)
#' @param overwrite replace existing usms (TRUE) or not,
#' updating existing ones (FALSE)
#'
#' @examples
#' \dontrun{
#' xml_path <- file.path(get_examples_path(file_type = "xml"), "usms.xml")
#' usms_doc <- xmldocument(xml_path)
#'
#' xl_path <- file.path(get_examples_path(file_type = "xl"),
#'                      "inputs_stics_example.xlsx")
#' usms_df <- read_excel(xl_path, sheet = "USMs")
#'
#' # For updating an existing xml doc (using existing usms names)
#' # Creating a fake existing_doc
#' existing_doc <- gen_usms_sols_doc("usms", nodes_nb = 3)
#' set_param_value(existing_doc,
#'   param_name = "usm",
#'   param_value = usms_df$usm_nom[c(3, 1, 5)]
#' )
#'
#' set_usms_param_xml(existing_doc, usms_df)
#'
#'
#' # For a new xml doc
#' # In that case: usms_df must contain all the usms parameters !
#' usms_nb <- dim(usms_df)[1]
#' new_doc <- gen_usms_sols_doc("usms", nodes_nb = usms_nb)
#'
#' set_usms_param_xml(new_doc, usms_df, overwrite = TRUE)
#' }
#'
#' @keywords internal
#'
#' @noRd
#'


set_usms_param_xml <- function(xml_doc,
                               usms_param = NULL,
                               overwrite = FALSE) {
  if (!base::is.null(usms_param)) {
    if (!("data.frame" %in% class(usms_param))) {
      stop("usms_param does not belong to data.frame class/type")
    }
    # if any factor field type in data.frame
    if (any(unlist(lapply(usms_param, is.factor)))) {
      stop("usms_param contains fields of factor type !")
    }
  }

  if (!is.xml_document(xml_doc)) {
    stop("xml_doc is not an xml_document object")
  }

  # detecting usm names column
  in_params <- names(usms_param)
  col_id <- grep("^usm", tolower(in_params))
  if (!length(col_id)) {
    stop("The column for identifying usm names has not been found !")
  }
  usm_col <- in_params[col_id]

  # default idx
  usms_xml_idx <- seq_len(dim(usms_param)[1])

  # Checking parameter names from param_table against xml ones
  check_param_names(
    param_names = in_params[-col_id],
    ref_names = get_param_names(xml_object = xml_doc)
  )

  # checking usms based on names if overwrite == FALSE
  if (!overwrite) {
    # getting usm names
    xml_usms <- as.vector(get_param_value(xml_doc, "usm"))

    ###############################################
    # TODO : see adding usms not in xml file ?
    ###############################################


    # checking xl names against xml names
    xl_in_xml <- usms_param[[usm_col]] %in% xml_usms

    if (!any(xl_in_xml)) {
      stop("Not any usm name in usms_param table is in xml doc !")
    }

    # xl usms idx in xml doc to be updated
    usms_xml_idx <- which(xml_usms %in% usms_param[[usm_col]])


    # Selecting data & ordering upon xml
    # order
    usms_param <- usms_param[xl_in_xml, ]
    usms_param <-
      usms_param[match(xml_usms[usms_xml_idx], usms_param[[usm_col]]), ]
  } else {
    # setting usms names
    set_param_value(xml_doc, "usm", usms_param[[usm_col]])
    usms_xml_idx <- seq_along(usms_param[[usm_col]])
  }

  # Managing parameter values replacement from usms_param
  # data.frame

  # Getting param names linked to plante node
  plante_params <- grep("_[0-9]*$", in_params, value = TRUE)
  plante_params_pref <- unique(gsub("_[0-9]*$", "", plante_params))

  fix_plant_params <- length(plante_params) != 0

  if (fix_plant_params) {
    # Setting parameter values
    for (i in 1:2) {
      for (p in plante_params_pref) {
        par <- paste0(p, "_", i)
        if (is.element(par, plante_params)) {
          set_param_value(
            xml_doc, p,
            usms_param[[par]],
            "plante", as.character(i),
            usms_xml_idx
          )
        }
      }
    }
  }
  # Getting independant params from plante
  other_params <- setdiff(in_params, c(plante_params, usm_col))

  if (!length(other_params)) {
    return()
  }

  # Setting param values
  for (p in other_params) {
    set_param_value(
      xml_doc, p, usms_param[[p]],
      usms_xml_idx
    )
  }
}
