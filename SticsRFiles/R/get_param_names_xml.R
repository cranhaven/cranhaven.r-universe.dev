#' @title Getting parameters names from xml files
#'
#' @description Extracting parameters names for an xml files or a vector
#' of files.
#'
#' @param xml_file an xml file path or a vector of paths
#'
#' @param name Parameter name or name part, or a vector of
#'
#' @param bounds Boolean. Are the parameter bounds to be returned ?
#'
#' @param output Output data format either "list" or "data.frame" (default)
#'
#' @param combine Logical, useful only for data.frame.
#' TRUE, to transform a data.frame list to a unique data.frame,
#' FALSE otherwise.
#'
#' @param exact Logical, if TRUE, the exact name is searched, FALSE otherwise
#' for partial search
#'
#' @return A list of parameters names data.frames or list, or a unique
#' data.frame for multiple files.
#'
#' @examples
#' \dontrun{
#' library(SticsRFiles)
#' xml_file <- "path/to/xmlfile"
#' xml_files_list <- c("path/to/xmlfile1", "path/to/xmlfile2")
#'
#' param_names <- get_param_names_xml(xml_file)
#'
#' param_names <- get_param_names_xml(xml_files_list)
#'
#' param_names <- get_param_names_xml(xml_files_list,
#'                                    param_name = c("al", "albedo"))
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
get_param_names_xml <- function(xml_file,
                                name = NULL,
                                bounds = TRUE,
                                output = "data.frame",
                                combine = TRUE,
                                exact = FALSE) {

  # Switch for transformations to data.frame format
  df_out <- output == "data.frame"
  df_comb <- df_out & combine

  # Recusring when multiple files in xml_file
  if (length(xml_file) > 1) {
    param_names <- lapply(
      xml_file,
      function(x) {
        get_param_names_xml(
          xml_file = x,
          name = name,
          bounds = bounds,
          output = output,
          combine = combine,
          exact = exact
        )
      }
    )

    # Empty param_names list
    if (length(param_names) == 0) {
      return(NULL)
    }

    # Filtering NULL elements
    param_names <- param_names[!unlist(lapply(param_names, base::is.null))]

    # To a named list
    if (!df_out) param_names <- unlist(param_names, recursive = FALSE)

    # Only for data.frames list
    if (df_comb) {
      param_names <- dplyr::bind_rows(param_names)
    }

    return(param_names)
  }

  # Getting param names for one xml document
  xml_doc <- xmldocument(xml_file)
  param_names <- get_param_names(xml_object = xml_doc)


  # Search based on names or a substring of parameters names
  # exact match with exact == TRUE
  if (!base::is.null(name)) {
    param_names <- find_names(names = param_names, exact = exact, name = name)
  }

  # No parameters names found
  if (!length(param_names)) {
    delete(xml_doc)
    return(NULL)
  }


  # Getting optional bounds
  if (bounds) {
    param_bounds <- get_param_bounds(
      xml_doc = xml_doc,
      param_name = param_names,
      output = output
    )
  }

  # Transforming list to data.frame (default behaviour)
  if (df_out) {
    param_names <- data.frame(
      name = unlist(param_names),
      file = base::basename(xml_file),
      stringsAsFactors = FALSE
    )

    if (bounds) {
      param_names <- merge(param_names, param_bounds, by = "name", all.x = TRUE)
    }
  } else {
    if (bounds) {
      param_names <- list(list(file = base::basename(xml_file),
                               name = param_bounds))
    } else {

      # To a named list
      names(param_names) <- base::basename(xml_file)
    }
  }

  delete(xml_doc)

  return(dplyr::as_tibble(param_names))
}
