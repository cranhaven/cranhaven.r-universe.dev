#' Download example USMs
#'
#' @description Download locally the example data from the
#' [data repository](https://github.com/SticsRPacks/data) in the SticsRPacks
#' organisation.
#'
#' @param out_dir Path of the directory where to download the data
#' @param dir `r lifecycle::badge("deprecated")` `dir` is no
#'   longer supported, use `out_dir` instead.
#' @param example_dirs List of use case directories names (optional)
#' @param stics_version Name of the STICS version. Optional, by default the
#' latest version returned by get_stics_versions_compat() is used.
#' @param version_name `r lifecycle::badge("deprecated")` `file_path` is no
#'   longer supported, use `file` instead.
#'
#' @return The path to the folder where data have been downloaded
#'
#' @export
#'
#' @examples
#'
#' # Getting data for a given example : study_case_1 and a given STICS version
#' download_data(example_dirs = "study_case_1", stics_version = "V9.0")
#'
download_data <- function(out_dir = tempdir(), example_dirs = NULL,
                          stics_version = "latest",
                          dir = lifecycle::deprecated(),
                          version_name = lifecycle::deprecated()) {

  # Managing the parameter name changes from 0.5.0 and onward:
  if (lifecycle::is_present(dir)) {
    lifecycle::deprecate_warn("1.0.0",
                              "download_data(dir)",
                              "download_data(out_dir)")
  } else {
    dir <- out_dir # to remove when we update inside the function
  }

  # Managing the parameter name changes from 0.5.0 and onward:
  if (lifecycle::is_present(version_name)) {
    lifecycle::deprecate_warn("1.0.0",
                              "download_data(version_name)",
                              "download_data(stics_version)")
  } else {
    version_name <- stics_version # to remove when we update inside the function
  }

  # setting version value from input for version == "latest"
  if (is.null(version_name) || version_name == "latest") {
    version_name <- get_stics_versions_compat()$latest_version
  }


  # Getting path string(s) from examples data file
  dirs_str <- get_referenced_dirs(dirs = example_dirs,
                                  stics_version = version_name)

  # Not any examples_dirs not found in example data file
  if (base::is.null(dirs_str))
    stop("Error: no available data for ", example_dirs)

  data_dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  data_dir_zip <- normalizePath(file.path(data_dir, "master.zip"),
                                winslash = "/",
                                mustWork = FALSE)
  utils::download.file("https://github.com/SticsRPacks/data/archive/master.zip",
                       data_dir_zip)

  df_name <- utils::unzip(data_dir_zip, exdir = data_dir, list = TRUE)

  # Creating files list to extract from dirs strings
  arch_files <- unlist(lapply(
    dirs_str,
    function(x) grep(pattern = x, x = df_name$Name, value = TRUE)
  ))

  # No data corresponding to example_dirs request in the archive !
  if (!length(arch_files))
    stop("No downloadable data for example(s), version: ",
         example_dirs,
         ",",
         version_name)

  # Finally extracting data
  utils::unzip(data_dir_zip, exdir = data_dir, files = arch_files)
  unlink(data_dir_zip)

  normalizePath(file.path(data_dir, arch_files[1]), winslash = "/")
}


#' Getting valid directories string for download from SticsRPacks data
#' repository
#'
#' @param dirs Directories names of the referenced use cases (optional),
#' starting with "study_case_"
#' @param stics_version An optional version string
#' within those given by get_stics_versions_compat()$versions_list
#'
#' @return Vector of referenced directories string (as "study_case_1/V9.0")
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' # Getting all available dirs from the data repos
#' get_referenced_dirs()
#'
#' # Getting dirs for a use case
#' get_referenced_dirs("study_case_1")
#'
#' # Getting dirs for a use case and a version
#' get_referenced_dirs("study_case_1", "V9.0")
#'
#' get_referenced_dirs(c("study_case_1", "study_case_2"), "V9.0")
#' }
#'
get_referenced_dirs <- function(dirs = NULL, stics_version = NULL) {

  # Loading csv file with data information
  ver_data <- get_versions_info(stics_version = stics_version)
  if (base::is.null(ver_data))
    stop("No examples data referenced for version: ", stics_version)

  dirs_names <- grep(pattern = "^study_case", x = names(ver_data), value = TRUE)
  if (base::is.null(dirs)) dirs <- dirs_names
  dirs_idx <- dirs_names %in% dirs

  # Not any existing use case dir found
  if (!any(dirs_idx)) {
    return()
  }

  # Filtering existing dirs in examples data
  if (!all(dirs_idx)) {
    dirs <- dirs_names[dirs_idx]
  }

  # Only dirs, returned if no specified version
  if (base::is.null(stics_version)) {
    return(dirs)
  }

  # Getting data according to version and dirs
  version_data <- ver_data %>% dplyr::select(dplyr::any_of(dirs))

  # Compiling referenced dirs/version strings, for existing version
  is_na <- base::is.na(version_data)
  dirs_str <-
    sprintf("%s/%s", names(version_data)[!is_na], version_data[!is_na])

  return(dirs_str)
}
