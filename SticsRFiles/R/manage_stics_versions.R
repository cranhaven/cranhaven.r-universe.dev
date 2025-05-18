#' Getting read only account identifiers for the STICS subversion repository
#'
#' @return A list with fields `username` and `password`
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_svn_identifiers()
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
get_svn_identifiers <- function() {

  # logged user identifiers !
  # linux : ~/.subversion/auth/svn.simple/cf86c1bb672ad0bf1613d66194e04e91

  # windows : encrypted !!

  return(list(username = "sticsread", password = "sticsread2020"))
}



#' Downloading inputs and/or outputs csv files from a repository branch/tag
#'
#' @param branch_url Address of the branch or tag
#' @param dest_dir Directory path where to store files
#' @param file_name File name(s)
#' or keyword "all" for both files (default)
#' @param ids Connection identifiers to the subversion server
#' @param overwrite Logical, TRUE for overwriting files, FALSE otherwise
#' @param verbose TRUE to display warnings (default), FALSE otherwise
#'
# @return
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' download_csv_files(
#'  branch_url = "https://w3.avignon.inra.fr/svn/modulostics/branches/branch10",
#'  dest_dir = system.file("extdata", package = "SticsRFiles")
#' )
#' }
download_csv_files <- function(branch_url,
                               dest_dir,
                               file_name = "all",
                               ids = get_svn_identifiers(),
                               overwrite = FALSE,
                               verbose = TRUE) {
  files_list <- c("inputs.csv", "outputs.csv")

  if (length(file_name) == 1 && file_name == "all") {
    file_name <- files_list
  }

  # Initialization of authentication on the subversion server
  # with an authorized read only account
  h <- curl::new_handle()

  curl::handle_setopt(h, username = ids$username)

  curl::handle_setopt(h, password = ids$password)

  curl::handle_setopt(h, ssl_verifypeer = 0)

  # Setting the files url and download
  file_url <- paste0(branch_url, "/doc/", file_name)

  # local files_path
  file_path <- vector(mode = "list", length = length(file_url))

  for (f in seq_along(file_url)) {
    dest_file <- file.path(dest_dir, file_name[f])

    if (file.exists(dest_file) && !overwrite) {
      warning(
        "File ", dest_file,
        "already exists",
        "(consider set overwrite to TRUE for passing through)!"
      )
    }

    file_path[[f]] <- try(
      curl::curl_download(file_url[f],
                          handle = h,
                          destfile = dest_file
      ),
      TRUE
    )
  }

  err_idx <- unlist(lapply(file_path, function(x) class(x) == "try-error"))

  if (any(err_idx) && verbose)
    warning("A least one file does not exist on the server !")

  file_path[err_idx] <- NA

  # Returning local file(s) path(s) vector
  invisible(unlist(file_path))
}


#' Adding a new version in the SticsRFiles library or package
#'
#' @param version_name name (i.e. "VX.Y") of the version to set
#' in the csv file containing informations about versions
#' @param url Subversion repository address of the branch, tag to get
#' information from
#' @param file_name File name(s)
#' or keyword "all" for both files (default)
#' @param location The destination where to write information
#' "install" for writing
#' things in the installed SticsRFiles library (default), "package" for writing
#' them in the package project (in RStudio)
#' @param overwrite A logical, TRUE to overwrite csv files,
#' FALSE otherwise (default)
#' @param verbose Logical, TRUE for displaying warnings (default),
#' FALSE otherwise
#'
#' @return An invisible data.frame containing versions data
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' add_stics_version(
#'   version_name = "V10.0",
#'   url = "https://w3.avignon.inra.fr/svn/modulostics/branches/branch10"
#' )
#'
#' add_stics_version(
#'   version_name = "V10.0",
#'   url = "https://w3.avignon.inra.fr/svn/modulostics/branches/branch10",
#'   location = "package"
#' )
#' }
add_stics_version <- function(version_name,
                              url,
                              file_name = "all",
                              location = "install",
                              overwrite = FALSE,
                              verbose = TRUE) {

  # Taking only into account adding or overwriting csv files :
  # inputs.csv, outouts.csv
  # and updating csv file stics_versions_info.csv gathering by version
  # existing files examples by type.
  # Only the csv col is automatically filled and files copied.
  # Other files examples must be added manuelly and corresponding
  # information updated in stics_versions_info.csv

  # url: modulostics branch or tag url, starting with
  # https://w3.avignon.inra.fr/svn/modulostics/branches/ or
  # https://w3.avignon.inra.fr/svn/modulostics/tags/ or
  # completed with the branch or tag name stord in the subversion reporitory.

  # location : install or package ?
  dest_dir <- get_data_dir(location = location)


  # Creating csv dir
  dir_path <- file.path(dest_dir, "csv")
  if (!dir.exists(dir_path)) dir.create(dir_path)

  # Creating version dir
  dir_path <- file.path(dir_path, version_name)
  if (!dir.exists(dir_path)) dir.create(path = dir_path)


  # Getting csv files from repos (branch or tag url) or overwriting them
  download_csv_files(url,
                     dir_path,
                     file_name = file_name,
                     overwrite = overwrite,
                     verbose = verbose)


  # Writing data updated with new version information (about csv files location)
  set_versions_info(
    version_name = version_name,
    location = location,
    overwrite = overwrite,
    verbose = verbose
  )

  if (verbose)
    message(paste0(version_name,
                   " successfully set in SticsRFiles ",
                   location,
                   ".\n"))
}




#' Removing a version and data from the SticsRFiles library or package
#'
#' @param version_name name (i.e. "VX.Y") of the version to set
#' in the csv file containing informations about versions
#' @param delete_files Logical, TRUE for removing files (default),
#' FALSE otherwise
#' @param location The destination where to remove information and data
#' "install" for removing things from the installed SticsRFiles library
#' (default), "package" for removing them from the package project (in RStudio)
#' @param verbose Logical, TRUE for displaying warnings (default),
#' FALSE otherwise
#'
#' @return An invisible logical value, TRUE if successful removing,
#'  FALSE otherwise
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' remove_stics_version(version_name = "V10.0")
#'
#' remove_stics_version(
#'   version_name = "V10.0",
#'   location = "package"
#' )
#' }
remove_stics_version <- function(version_name,
                                 delete_files = TRUE,
                                 location = "install",
                                 verbose = TRUE) {

  # Getting existing data about versions
  versions_info <- get_versions_info(location = location)

  version_idx <- versions_info$versions %in% version_name

  ret_write <- TRUE
  ret_rm <- TRUE

  # if version  exists in csv files
  if (any(version_idx)) {
    versions_info <- versions_info[!version_idx, ]

    versions_info_file <- get_versions_file_path(location = location)

    utils::write.csv2(
      x = versions_info,
      quote = FALSE,
      file = versions_info_file,
      row.names = FALSE
    )
    ret_write <- TRUE
  }

  if (!delete_files) {
    return(invisible(ret_write & TRUE))
  }

  # Trying to delete files
  files_dir <- file.path(get_data_dir(location = location), "csv", version_name)

  if (!dir.exists(files_dir)) {
    return(invisible(ret_write & TRUE))
  }

  ret_rm <- unlink(x = files_dir, recursive = TRUE)

  invisible(ret_write & ret_rm == 0)
}


#' Getting package or library data path
#'
#' @param location The destination where to remove information and data
#' "install" for removing things from the installed SticsRFiles library
#' (default), "package" for removing them from the package project (in RStudio)
#'
#' @return A directory path
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_data_dir()
#'
#' get_data_dir(location = "package")
#' }
get_data_dir <- function(location = "install") {
  if (location == "install")
    dest_dir <- system.file("extdata", package = "SticsRFiles")

  if (location == "package") {
    proj_dir <- rstudioapi::getActiveProject()
    pkg <- gsub(pattern = "\\.Rproj$",
                x = list.files(pattern = "\\.Rproj$", proj_dir),
                replacement = "")
    if (base::is.null(proj_dir) || pkg != "SticsRFiles")
      stop("Load the project SticsRFiles before proceeding !")

    dest_dir <- file.path(proj_dir, "inst", "extdata")
  }

  return(dest_dir)
}



#' Getting versions information csv file path according to its location
#'
#' @param location The destination where to remove information and data
#' "install" for removing things from the installed SticsRFiles library
#' (default), "package" for removing them from the package project (in RStudio)
#'
#' @return A file path
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_versions_file_path()
#'
#' get_versions_file_path(location = "package")
#' }
get_versions_file_path <- function(location = "install") {
  file.path(get_data_dir(location = location),
            "versions",
            get_versions_file_name())
}


#' Updating csv files for a given version
#'
#' @param version_name name (i.e. "VX.Y") of the version
#' @param url Subversion repository address of the branch, tag to get
#' information from
#' @param file_name File name(s)
#' @param location The destination where to write information "install"
#' for writing things in the installed SticsRFiles library (default),
#' "package" for writing them in the package project (in RStudio)
#' @param verbose Logical, TRUE for displaying warnings, FALSE otherwise
#'
#' @return An invisible data.frame containing versions data in the
#' SticsRFiles library (or package)
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' update_stics_version(
#'   version_name = "V10.0",
#'   url = "https://w3.avignon.inra.fr/svn/modulostics/branches/branch10"
#' )
#'
#' update_stics_version(
#'   version_name = "V10.0",
#'   url = "https://w3.avignon.inra.fr/svn/modulostics/branches/branch10",
#'   location = "package"
#' )
#' }
update_stics_version <- function(version_name,
                                 url,
                                 file_name = "all",
                                 location = "install",
                                 verbose = FALSE) {

  # Forcing csv files overwriting
  add_stics_version(version_name,
                    url,
                    file_name = file_name,
                    location = location,
                    overwrite = TRUE,
                    verbose = verbose
  )
}


#' Writing information about STICS versions and related example files
#' directories in the SticsRFiles library or package
#'
#' @param version_name name (i.e. "VX.Y") of the version to add in versions
#' information
#' @param location The destination where to write information "install"
#' for writing things in the installed SticsRFiles library (default),
#' "package" for writing them in the package project (in RStudio)
#' @param overwrite A logical, TRUE to overwrite versions info file,
#' FALSE otherwise (default)
#' @param verbose Logical, TRUE for displaying warnings (default),
#' FALSE otherwise
#'
# @return
#' @keywords internal
#'
#' @noRd
#'
# @examples
set_versions_info <- function(version_name,
                              location = "install",
                              overwrite = FALSE,
                              verbose = TRUE) {

  # Setting file output flag
  write_file <- TRUE

  # Getting the versions info file and its dir path
  versions_info_file <- get_versions_file_path(location = location)
  dir_path <- dirname(versions_info_file)
  if (!dir.exists(dir_path)) dir.create(dir_path)

  # Getting data.frame for the new version: with only the csv column filled
  version_info <- get_version_info_tmpl(version_name = version_name)

  # Getting existing data about versions
  versions_info <- get_versions_info(location = location)

  # Setting data for a new file, only with csv column filled
  if (base::is.null(versions_info)) versions_info <- version_info

  # Adding data in the final data.frame to write
  # if version_name does not exist
  version_idx <- versions_info$versions %in% version_name

  if (!any(version_idx)) {
    versions_info <- dplyr::bind_rows(versions_info, version_info)
    versions_info[is.na(versions_info)] <- ""

    write_file <- write_file & overwrite
    if (!write_file) {
      if (verbose) {
        warning(
          version_name, " already exists in ", versions_info_file,
          ", it is safer updating it by hand of set overwrite to TRUE !"
        )
      }
      return(invisible(versions_info))
    }
  } else {
    return(invisible(versions_info))
  }

  # Sorting rows against the version number
  # extracted from the versions column
  # Just in case if an older version than existing ones
  # is added

  ord_idx <- order(get_version_num(versions_info$versions))

  versions_info <- versions_info[ord_idx, ]

  # Writing the csv file in the appropriate folder
  if (write_file) {
    utils::write.csv2(
      x = versions_info,
      quote = FALSE,
      file = versions_info_file,
      row.names = FALSE
    )
  }

  return(invisible(versions_info))
}


#' Get a basic data.frame for a version
#'
#' @param version_name name (i.e. "VX.Y") of the version to set
#' in the output data.frame
#'
#' @return a data.frame, with version set in `versions` and `csv` columns
#' (for existing inputs.csv and outputs.csv files in a versions VX.Y
#' sub-directory)
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_versions_info(version_name = "V10.0")
#' }
get_version_info_tmpl <- function(version_name) {
  data.frame(
    versions = version_name,
    csv = version_name,
    obs = "",
    sti = "",
    txt = "",
    xml = "",
    xml_tmpl = "",
    xl = "",
    study_case_1 = "",
    study_case_intercrop = "",
    stringsAsFactors = FALSE
  )
}
