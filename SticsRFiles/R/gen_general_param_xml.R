#' @title Generate STICS general parameters xml file(s) from a template
#' file according to a STICS version
#'
# @param param_df A table (df, tibble) containing the values of the parameters
# to use (see details)
# @param file Path of a sta xml file to be used as a template. Optional,
# if not provided, the function will use a standard template depending
# on the STICS version.
#' @param out_dir Path of the directory where to generate the file(s).
#' @param stics_version Name of the STICS version. Optional, the latest one
#' is used as default
#' @param overwrite Optional logical, TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @details Please see `get_stics_versions_compat()` for the full list of
#' STICS versions that can be used for the argument `stics_version`.
#'
#' @return None
#'
#' @examples
#' gen_general_param_xml(out_dir = tempdir())
#'
#' gen_general_param_xml(out_dir = tempdir(),
#'                       stics_version = "V10.0",
#'                       overwrite = TRUE)
#'
#' @export
#'
gen_general_param_xml <- function(out_dir,
                                  stics_version = "latest",
                                  overwrite = FALSE) {


  # check/get version
  stics_version <- get_xml_stics_version(
    stics_version = stics_version
  )

  # getting dir path of templates
  files_dir <- get_examples_path(file_type = "xml",
                                 stics_version = stics_version)

  # Copying files to out_dir
  files_name <- c("param_gen.xml", "param_newform.xml")
  xml_files <- file.path(files_dir, files_name)

  copy_status <- file.copy(xml_files, out_dir, overwrite = overwrite)

  if (!all(copy_status))
    stop("Some error occured while copying files: \n",
          paste(files_name[!copy_status], collapse = ", \n"),
          "\nConsider setting overwrite to TRUE as function additional input")



}
