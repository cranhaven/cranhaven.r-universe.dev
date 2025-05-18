#' @title Generates from a template a STICS ini xml_document
#'
#' @param xml_doc an optional xml_document object (created from an ini file)
#' @param param_table a table (df, tibble) containing parameters to use
#' (optional)
#' @param crop_tag a crop identifier for crop parameters
#' (for example like "crop" used for in a parameter suffix :
#' param_crop1, param_crop2)
#' @param params_desc a list describing crop parameters and others
#' @param stics_version the STICS files version to use (optional,
#' default to latest). Only used if xml_doc = NULL.
#' @param check_names logical for checking names of param_table columns or not
#'
#' @return an invisible xml_document object or a list of
#'
#' @examples
#' \dontrun{
#' library(readxl)
#'
#' xl_path <- "inputs_stics_example.xlsx"
#' download_usm_xl(file = xl_path)
#' ini_param_df <- read_excel(xl_path, sheet = "Ini")
#' ini_doc <- gen_ini_doc(param_table = ini_param_df)
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
gen_ini_doc <- function(xml_doc = NULL,
                        param_table = NULL,
                        crop_tag = "Crop",
                        params_desc = NULL,
                        stics_version = "latest",
                        check_names = TRUE) {


  # check/get version
  stics_version <- get_xml_stics_version(
    stics_version = stics_version,
    xml_doc = xml_doc
  )

  # getting a default xml template
  if (base::is.null(xml_doc)) {
    xml_doc <- get_xml_base_doc("ini", stics_version = stics_version)
  }

  # Nothing to do
  if (base::is.null(param_table)) {
    return(xml_doc)
  }


  # TODO : replace with get_values_from_table
  # TODO : get_params_from_table to be completed with plant specificities crop
  # tag see treatment inside next block
  if (base::is.null(params_desc)) {
    # detecting ini names column
    crop_regex <- paste0("_", crop_tag, "[0-9]*$")
    layer_regex <- "_[0-9]*$"

    param_names <- names(param_table)

    base_params <- param_names[!grepl("[0-9]$", param_names)]

    plante_params <- param_names[grep(crop_regex, param_names)]


    other_params <- setdiff(param_names, c(plante_params, base_params))
    other_params_pref <- unique(gsub(layer_regex, "", other_params))

    plante_params <- gsub("_[a-zA-Z0-9]*$", "", plante_params)
    plante_params <- gsub("_[0-9]*$", "", plante_params)
    plante_params_pref <- unique(gsub(layer_regex, "", plante_params))

    params_desc <- list(
      plante_params_pref = plante_params_pref,
      other_params_pref = other_params_pref,
      base_params = base_params,
      param_names = param_names
    )
  } else {
    plante_params_pref <- params_desc$plante_params_pref
    other_params_pref <- params_desc$other_params_pref
    base_params <- params_desc$base_params
  }

  # Checking parameter names from param_table against xml ones
  if (check_names) {
    check_param_names(
      param_names = param_names,
      ref_names = get_param_names(xml_object = xml_doc),
      pattern_tag = crop_tag
    )
  }

  # managing several doc generation based upon the lines number in param_table
  lines_nb <- dim(param_table)[1]
  if (lines_nb > 1) {
    xml_docs <- apply(
      param_table,
      1,
      function(x) {
        gen_ini_doc(
          xml_doc = clone_xml_doc(xml_doc),
          param_table = as.data.frame(t(x),
            stringsAsFactors = FALSE
          ),
          params_desc = params_desc,
          stics_version = stics_version,
          check_names = FALSE
        )
      }
    )
    return(xml_docs)
  }


  # Filtering parameters not to be set
  # i.e. : 999 or NA
  par_idx <- as.vector(!grepl(pattern = "^999", param_table) &
    !is.na(param_table))

  # exiting not any parameters values to set
  if (!any(par_idx)) {
    return(invisible(xml_doc))
  }

  # Filtering columns
  param_table <- param_table[, par_idx]

  # Getting resulting param names
  param_names <- names(param_table)

  plant_nb <- 2

  # Setting base parameters
  for (p in base_params) {
    if (is.element(p, param_names)) {
      set_param_value(
        xml_doc = xml_doc,
        param_name = p,
        param_value = param_table[[p]]
      )
      if (p == "nbplantes") plant_nb <- param_table[[p]]
    }
  }

  # Setting plante params
  for (i in 1:plant_nb) {
    for (p in plante_params_pref) {
      par <- paste0(p, "_", crop_tag, i)
      if (is.element(par, param_names)) {
        set_param_value(
          xml_doc = xml_doc,
          param_name = p,
          param_value = param_table[[par]],
          parent_name = "plante",
          parent_sel_attr = as.character(i)
        )
        next
      }

      for (j in 1:5) {
        # densinitial
        par2 <- paste0(p, "_", j, "_", crop_tag, i)

        if (is.element(par2, param_names) && !is.na(param_table[[par2]])) {
          set_param_value(
            xml_doc = xml_doc,
            param_name = p,
            param_value = param_table[[par2]],
            parent_name = "plante",
            parent_sel_attr = as.character(i),
            ids = j
          )
        }
      }
    }
  }

  # Setting other params with suffix (layer index)
  for (j in 1:5) {
    for (p in other_params_pref) {
      par <- paste0(p, "_", j)
      if (is.element(par, param_names) && !is.na(param_table[[par]])) {
        set_param_value(
          xml_doc = xml_doc,
          param_name = "horizon",
          param_value = param_table[[par]],
          parent_name = p,
          parent_sel_attr = j
        )
      }
    }
  }


  return(invisible(xml_doc))
}
