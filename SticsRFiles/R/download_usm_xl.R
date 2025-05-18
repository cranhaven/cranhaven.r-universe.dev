#' @title Downloading an Excel usms data file example into a directory
#'
#' @description The file is an example that can be used for generating
#' JavaSTICS input files from parameters values stored in Excel spreadsheet
#' format (USMs, Ini, Soils, Tec, Station, ...). Each sheet contains parameters
#' values to insert into XML files, with the help of these functions:
#' \code{\link{gen_usms_xml}}, \code{\link{gen_sols_xml}},
#' \code{\link{gen_tec_xml}}, \code{\link{gen_sta_xml}},
#' \code{\link{gen_usms_xml}}, \code{\link{gen_ini_xml}}
#'
#' @param file Name of an Excel file (optional, not used for the moment)
#' @param out_dir Directory path where to copy the Excel file
#'  (optional, default: `tempdir()`)
#' @param stics_version Name of the STICS version. Optional, by default
#' the latest version returned by get_stics_versions_compat() is used.
#' @param overwrite Optional logical, TRUE for overwriting files,
#' FALSE otherwise (default)
#' @param verbose Logical value for displaying information while running
#' @param xl_name `r lifecycle::badge("deprecated")` `xl_name` is no
#'   longer supported, use file instead.
#' @param dest_dir `r lifecycle::badge("deprecated")` `dest_dir` is no
#'   longer supported, use out_dir instead.
#' @param version_name `r lifecycle::badge("deprecated")` `version_name` is no
#'   longer supported, use stics_version instead.
#' @param ... Additional arguments to be passed
#'
#' @return A vector of copied files path.
#'
#' @examples
#'
#' download_usm_xl()
#'
#'
#' @export
#'

download_usm_xl <- function(file = NULL,
                            out_dir = tempdir(),
                            stics_version = "latest",
                            overwrite = FALSE,
                            verbose = FALSE,
                            xl_name = lifecycle::deprecated(),
                            version_name = lifecycle::deprecated(),
                            dest_dir = lifecycle::deprecated(),
                            ...) {
  if (lifecycle::is_present(xl_name)) {
    lifecycle::deprecate_warn("1.0.0",
                              "download_usm_xl(xl_name)",
                              "download_usm_xl(file)")
  } else {
    xl_name <- file # to remove when we update inside the function
  }
  if (lifecycle::is_present(version_name)) {
    lifecycle::deprecate_warn("1.0.0",
                              "download_usm_xl(version_name)",
                              "download_usm_xl(stics_version)")
  } else {
    version_name <- stics_version # to remove when we update inside the function
  }
  if (lifecycle::is_present(dest_dir)) {
    lifecycle::deprecate_warn("1.0.0",
                              "download_usm_xl(dest_dir)",
                              "download_usm_xl(out_dir)")
  } else {
    dest_dir <- out_dir # to remove when we update inside the function
  }


  args <- list(...)


  xl_patt <- "^inputs\\_.*(example|USMs)\\.(xls|xlsx)$"
  file_type <- "xl"

  if ("type" %in% names(args) && args$type == "csv") {
    xl_patt <- "^inputs\\_.*(example|USMs)\\.csv$"
    file_type <- "csv"
  }

  if (base::is.null(xl_name)) {
    xl_name <- xl_patt
  }

  xl_dir <- get_examples_path(file_type = "xl",
                              stics_version = version_name)

  files_list <- list.files(xl_dir, pattern = xl_patt)

  if (length(files_list) > 1) {
    warning("You must give a file name, see in returned list !")
    return(files_list)
  }

  if (!length(files_list)) {
    warning("No matching files found, see in returned list !")
    return(list.files(xl_dir, pattern = xl_patt))
  }

  dest_list <- file.path(dest_dir, files_list)
  exist_files <- file.exists(dest_list)
  if (!overwrite && any(exist_files)) {
    warning(paste(
      files_list[exist_files], "already exists in ", dest_dir,
      "\nConsider to set overwrite = TRUE to overwrite (it | them )"
    ))
    # filtering existing files, not copied if overwrite == FALSE
    files_list <- files_list[!exist_files]
  }

  src_list <- file.path(xl_dir, files_list)
  success <- file.copy(from = src_list, to = dest_dir, overwrite = overwrite)

  if (any(success)) {
    if (verbose)
      message(paste(files_list[success],
                    " has been copied in directory ",
                    dest_dir))
    dest_list <- dest_list[success]
  }

  if (!all(success))
    warning("Error copying files:\n", paste(src_list[!success],
                                            collapse = "\n"))


  return(invisible(dest_list))
}



#' @title Downloading a CSV usms data file example into a directory
#'
#' @description The file is an example that can be used for generating JavaSTICS
#' usms.xml input file from parameters values stored in a CSV file using
#' the function \code{\link{gen_usms_xml}}
#'
#' @param file Name of a csv file (optional, not used for the moment)
#' @param out_dir Directory path where to copy the csv file
#' (default: `tempdir()`)
#' @param stics_version Name of the STICS version. Optional, by default
#' the latest version returned by get_stics_versions_compat() is used.
#' @param overwrite Optional logical, TRUE for overwriting files,
#' FALSE otherwise (default)
#' @param verbose Logical value for displaying information while running
#' @param csv_name `r lifecycle::badge("deprecated")` `csv_name` is no
#'   longer supported, use file instead.
#' @param version_name `r lifecycle::badge("deprecated")` `version_name` is no
#'   longer supported, use stics_version instead.
#' @param dest_dir `r lifecycle::badge("deprecated")` `dest_dir` is no
#'   longer supported, use out_dir instead.
#' @return A vector of copied files path.
#'
#' @examples
#' download_usm_csv()
#'
#' @export
#'

download_usm_csv <- function(file = NULL,
                             out_dir = tempdir(),
                             stics_version = "latest",
                             overwrite = FALSE,
                             verbose = FALSE,
                             csv_name = lifecycle::deprecated(),
                             version_name = lifecycle::deprecated(),
                             dest_dir = lifecycle::deprecated()) {
  if (lifecycle::is_present(csv_name)) {
    lifecycle::deprecate_warn("1.0.0",
                              "download_usm_csv(csv_name)",
                              "download_usm_csv(file)")
  } else {
    csv_name <- file # to remove when we update inside the function
  }
  if (lifecycle::is_present(version_name)) {
    lifecycle::deprecate_warn("1.0.0",
                              "download_usm_csv(version_name)",
                              "download_usm_csv(stics_version)")
  } else {
    version_name <- stics_version # to remove when we update inside the function
  }
  if (lifecycle::is_present(dest_dir)) {
    lifecycle::deprecate_warn("1.0.0",
                              "download_usm_csv(dest_dir)",
                              "download_usm_csv(out_dir)")
  } else {
    dest_dir <- out_dir # to remove when we update inside the function
  }


  download_usm_xl(
    file = csv_name,
    out_dir = dest_dir,
    stics_version = version_name,
    overwrite = overwrite,
    verbose = verbose,
    type = "csv"
  )
}
