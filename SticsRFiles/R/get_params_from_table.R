#' @title Get a list of STICS xml parameters names and values from a table
#' (data.frame, tibble)
#' @param params_table a table (df, tibble) containing parameters to use
#'
#' @param param_names parameters names to select
#' @param xml_doc an xml_document object (of any xml type)
#' @param lines_id table lines identifiers to select
#' @param stopping logical value for stopping if any unknown parameters
#' (if TRUE)
#' @param dict List of names correspondence between short names (tags)
#' and real parameters names
#' @param na_values value to use as missing value in param_table
#' (optional, default : NA)
#'
#' @return a named list (with parameters names as list names)
#' of data.frame/tibble
#'
#' @examples
#' \dontrun{
#'
#' download_usm_xl(file = "inputs_stics_example.xlsx",
#'                 dest_dir = "/path/to/dest/dir")
#' xl_path <- file.path("/path/to/dest/dir", "inputs_stics_example.xlsx")
#' ini_param_df <- read_excel(xl_path, sheet = "Ini")
#' xml_path <- "path/to/ini/xml"
#' ini_doc <- xmldocument(xml_path)
#' get_params_from_table(ini_param_df, ini_doc)
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
get_params_from_table <- function(params_table,
                                  param_names = NULL,
                                  xml_doc,
                                  lines_id = NULL,
                                  stopping = FALSE,
                                  dict = NULL,
                                  na_values = NA) {

  # TODO: doing a merge with get_values_from_table

  if (base::is.null(dict)) {
    dict <- list(
      julapI = "julapI_or_sum_upvt", doseI = "amount",
      julapN = "julapN_or_sum_upvt", doseN = "absolute_value/%"
    )
  }

  # TODO : perhaps add checking of dict list content !!!

  if (!nargs()) {
    return(dict)
  }


  # replacing column names starting with names of the dico list
  # with values of corresponding field in dico
  for (key in names(dict)) {
    params_table <- params_table %>%
      dplyr::rename_at(
        dplyr::vars(dplyr::matches(paste0(key, "\\_[0-9*]"))),
        list(~ gsub(x = ., pattern = paste0("(", key, ")(\\_[0-9*])"),
             replacement = paste0(dict[[key]], "\\2")))
      )
  }



  # getting values from table
  param_values <- get_values_by_param(params_table, param_name = param_names)


  # checking if all params from xl table exist in xml doc file
  tbl_par_names <- names(param_values)
  doc_par_names <- unlist(get_params_from_doc(xml_doc), use.names = FALSE)

  unknown_params <- setdiff(tbl_par_names, doc_par_names)

  if (length(unknown_params)) {
    message_str <- sprintf(
      "\n%s\n%s\n\n%s\n\n%s\n\n", "Unknown parameters found in table: ",
      paste(unknown_params, collapse = ", "),
      "Verify parameters names in xml files or",
      "check substitution names in list returned by get_params_from_table()"
    )
    if (stopping) {
      stop(message_str)
    } else {
      message(message_str)
    }

    # removing unknown param columns
    param_values[!names(param_values) %in% unknown_params]
  }

  return(param_values)
}
