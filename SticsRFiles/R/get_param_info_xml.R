#' Getting parameters information using partial search words
#'
#' @param param Optional name or partial name or a vector of
#'
#' @param file Optional, xml file path or a vector of
#'
#' @param stics_version An optional version name as listed in
#' get_stics_versions_compat() return
#' @param kind Kind of information to be retrieved for parameters
#' among "parameter", "formalism" or "all" for both of them
#'
#' @param exact Logical, if TRUE, the exact name is searched
#'
#' @details If not any name vector of names, information are extracted for
#' all the existing parameters
#'
#' @return A data.frame containing parameters names and
#' their origin (file name), and their bounds or the formalism they belong to,
#' or both of them.
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#'
#' get_param_data_df(param = "albedo")
#'
#' get_param_data_df(param = "albedo", file = "/path/to/file.xml")
#'
#' get_param_data_df(param = "albedo", kind = "formalism")
#'
#' get_param_data_df(param = "albedo", stics_version = "V9.0")
#'
#' get_param_data_df(param = c("albedo", "latitude", "humcapil"))
#'
#' get_param_data_df(
#'   param = c("albedo", "latitude", "humcapil"),
#'   kind = "formalism"
#' )
#'
#' get_param_data_df(file = "/path/to/JavaSTICS/folder/config/inputs.csv")
#'
#' get_param_data_df(
#'   param = "albedo",
#'   file = "/path/to/JavaSTICS/folder/config/inputs.csv"
#' )
#' }
#'
get_param_data_df <- function(param = NULL,
                              file = NULL,
                              stics_version = "latest",
                              kind = "all",
                              exact = FALSE) {
  kinds <- c("parameter", "formalism", "all")

  # Checking kind
  if (!kind %in% kinds) {
    stop(paste("Unknown kind of parameter(s) information to retrieve:", kind))
  }

  # Just in case
  param <- unique(param)

  if (base::is.null(file)) {
    # Check STICS version
    stics_version <- get_xml_stics_version(stics_version)

    # Getting XML examples files dir from the package
    xml_dir <- get_examples_path(file_type = "xml",
                                 stics_version = stics_version)

    # Getting the XML files list
    files_list <- list.files(
      path = xml_dir,
      pattern = "\\.xml$",
      full.names = TRUE
    )

    # Not any files found !
    if (length(files_list) == 0) {
      stop("Examples XML files not found in the package !")
    }
  } else {
    files_list <- file
  }

  # getting parameters from an inputs.csv file
  if (length(files_list) == 1 &&
      grepl(pattern = "inputs.csv", x = files_list)) {
    param_names <- utils::read.csv2(
      file,
      header = FALSE,
      stringsAsFactors = FALSE
    )[c(1, 4, 5, 7:8)]
    names(param_names) <- c("name", "file", "dim", "min", "max")

    if (is.null(param)) {
      return(param_names)
    }

    par_idx <- param_names$name %in% param

    return(param_names[par_idx, ])
  }

  # Getting parameters names bounds and file
  param_names <- suppressWarnings(get_param_names_xml(
    xml_file = files_list,
    name = param,
    exact = exact
  ))

  # Not any parameters found
  if (dim(param_names)[1] < 1) {
    warning(paste("Not any parameter found for STICS version: ", stics_version))
    return(invisible())
  }

  # Returning parameters information
  if (kind == "parameter") {
    return(param_names)
  }

  # Some parameters names may be found in several files (i.e.: nbplantes)
  uniq_param_names <- unique(param_names$name)

  param_formalism <- get_formalisms_xml(
    xml_file = files_list,
    par_name = uniq_param_names
  )

  param_formalism <- form_list2df(param_formalism)

  # if formalism request
  if (kind == "formalism") {
    return(param_formalism)
  }

  # merging all information from names and formalisms
  param_df <- suppressMessages(dplyr::left_join(param_names, param_formalism))

  # Adding a version  attribute
  attr(x = param_df, which = "version") <- stics_version
  return(param_df)
}



form_list2df <- function(formalism_list) {

  # filtering NA list values (files whithout formalisms)
  formalism_list <- formalism_list[unlist(lapply(formalism_list, is.list))]

  # files names
  files <- names(formalism_list)

  out <- vector("list", length(files))

  for (i in seq_along(files)) {
    file <- files[i]
    forms_names <- names(formalism_list[[file]])
    out_file <- NULL
    out_form <- NULL
    out_param <- NULL
    for (j in seq_along(forms_names)) {
      form <- forms_names[[j]]
      par_names <- formalism_list[[file]][[j]]
      out_file <- c(out_file, rep(file, length(par_names)))
      out_form <- c(out_form, rep(form, length(par_names)))
      out_param <- c(out_param, par_names)
    }
    out[[i]] <- data.frame(file = out_file,
                           formalism = out_form,
                           name = out_param,
                           stringsAsFactors = FALSE)
  }

  # returning the tibble
  dplyr::as_tibble(dplyr::bind_rows(out))
}
