#' Getting examples files path attached to a STICS version for a given file type
#'
#' @param file_type A file type string among files types or a vector of
#' ("csv", "obs", "sti", "txt", "xml")
#' @param stics_version Name of the STICS version. Optional, by default
#' the latest version returned by `get_stics_versions_compat()` is used.
#' @param overwrite TRUE for overwriting directory; FALSE otherwise
#' @param version_name `r lifecycle::badge("deprecated")` `version_name` is no
#'   longer supported, use `stics_version` instead.
#'
#' @return A directory path for examples files for given file type and STICS
#' version or a vector of (for unknown file types "" is returned as path)
#'
#' @export
#'
#' @examples
#' get_examples_path(file_type = "csv")
#'
#' get_examples_path(file_type = c("csv", "sti"))
#'
#' get_examples_path(file_type = "csv", stics_version = "V8.5")
#'
get_examples_path <- function(file_type, stics_version = "latest",
                              overwrite = FALSE,
                              version_name = lifecycle::deprecated()) {
  if (lifecycle::is_present(version_name)) {
    lifecycle::deprecate_warn(
      "1.0.0", "get_examples_path(version_name)",
      "get_examples_path(stics_version)"
    )
  } else {
    # to remove when we update inside the function
    version_name <- stics_version
  }

  # Getting files types list
  example_types <- get_examples_types()

  # If not any arguments : displaying files types list
  if (missing(file_type)) {
    message("Available files types: ",
            paste(get_examples_types(), collapse = ","))
    return(invisible())
  }

  # Checking if all types in file_type exist
  files_type_idx <- file_type %in% example_types
  if (!all(files_type_idx))
    stop("Unknown file_type: ", file_type[!files_type_idx])

  # Validating the version string
  version_name <- check_version_compat(version_name)

  # Checking if files available for the given version
  ver_data <- get_versions_info(stics_version = version_name)
  if (base::is.null(ver_data))
    stop("No examples available for version: ", version_name)

  # Getting files dir path for the given type
  version_dirs <- unlist(dplyr::select(ver_data, dplyr::all_of(file_type)))
  is_na_dirs <- is.na(version_dirs)

  if (any(is_na_dirs))
    stop("Not any data in examples for ",
         paste(file_type[is_na_dirs], collapse = ", "),
         " and version ", version_name)

  files_str <- unlist(
    lapply(file_type,
           function(x) gsub(pattern = "(.*)_.*", x = x, replacement = "\\1")
    )
  )

  # Getting and storing path for each kind of file
  examples_path <- vector(mode = "character", length = length(files_str))
  for (i in seq_along(files_str)) {
    base_path <- unzip_examples(files_str[i], overwrite = overwrite)
    if (base_path == "") {
      examples_path[i] <- ""
    } else {
      examples_path[i] <- normalizePath(file.path(base_path, version_dirs[i]),
                                        winslash = "/",
                                        mustWork = FALSE)
    }
  }

  # Treating not existing directories for file_type
  exist_ex_path <- !(examples_path == "")
  if (!all(exist_ex_path))
    warning("Not any available ",
            paste(file_type[!exist_ex_path], collapse = ", "),
            " examples for version: ",
            version_name)

  # Returning the examples files dir path for the given type
  return(invisible(examples_path))
}

# TODO: evaluate if useful ?
list_examples_files <- function(file_type,
                                version_name = "latest",
                                full_names = TRUE) {
  examples_path <- get_examples_path(file_type = file_type,
                                     stics_version = version_name)


  files_list <- list.files(pattern = "\\.[a-zA-Z]+$",
                           path = examples_path,
                           full.names = full_names)

  return(files_list)
}


get_examples_types <- function() {
  file_types <- c("csv", "obs", "sti", "txt", "xml", "xl", "xml_tmpl",
                  "xml_param", "xsl")
  return(file_types)
}


#' Unzip files archive if needed and return examples files path
#' in extdata directory
#'
#' @param files_types type of file of examples files set
#' @param version_dir version directory names of the example files
#' @param overwrite TRUE for overwriting directory; FALSE otherwise
#'
#' @return library examples files path
#'
#' @keywords internal
#'
#' @noRd
#'
# @examples
unzip_examples <- function(files_type, version_dir, overwrite = FALSE) {

  ex_path <- system.file("extdata",
                         package = "SticsRFiles")

  dir_path <- normalizePath(file.path(tempdir(), files_type),
                            winslash = "/",
                            mustWork = FALSE)

  if (dir.exists(dir_path) && !overwrite) return(dir_path)

  if (overwrite)
    unlink(x = dir_path, recursive = TRUE)

  zip_path <- file.path(ex_path, paste0(files_type, ".zip"))

  if (file.exists(zip_path)) {
    utils::unzip(zipfile = zip_path, exdir = tempdir())
  } else {
    dir_path <- ""
  }

  return(dir_path)
}


#' Copy mod, obs, lai, and weather data files
#' @param workspace JavaStics xml workspace path
#' @param file_type file type to copy among "mod", "obs", "clim"
#' @param javastics JavaStics folder path (Optional)
#' @param out_dir   Output directry path
#' @param verbose   logical, TRUE for displaying a copy message
#' FALSE otherwise (default)
#' @param overwrite Logical TRUE for overwriting files,
#' FALSE otherwise (default)
#'
#' @return invisible copy statuses
#'
#' @keywords internal
#' @noRd
#'
workspace_files_copy <- function(workspace,
                                 file_type = NULL,
                                 javastics = NULL,
                                 out_dir,
                                 overwrite = FALSE,
                                 verbose = FALSE) {

  # files types vector and associated regex
  file_types <- c("mod", "obs", "lai", "meteo")
  file_patt <- c("*.mod", "*.obs", "*.lai", "\\.[0-9]{4}$")
  file_desc <- c("output definition (*.mod)",
                 "observation (*.obs)",
                 "LAI dynamics (*.lai)",
                 "weather data (*.YYYY)")

  # if file_type is not given, all files type are processed
  if (is.null(file_type)) file_type <- file_types

  # recurive call for a vector
  if (length(file_type) > 1) {
    stat_list <- vector(mode = "list", length(file_type))
    for (i in seq_along(file_type)) {
      stat_list[[i]] <- workspace_files_copy(workspace = workspace,
                                             file_type = file_type[i],
                                             javastics = javastics,
                                             out_dir = out_dir,
                                             overwrite = overwrite,
                                             verbose = verbose
      )
    }
    return(invisible(stat_list))
  }

  # Just in case if the func is used outside of the workspace upgrade
  type_idx <- file_types %in% file_type

  if (! any(type_idx)) {
    warning("The given file type does not exist: ", file_type, " nothing done!")
    return()
  }

  # getting the file path list to copy
  patt <- file_patt[type_idx]
  files_list <- list.files(path = workspace,
                           full.names = TRUE,
                           pattern = patt)

  # Just for the *.mod files, looking in javastics if not found in the workspace
  # TODO: combine both if partial match
  if (length(files_list) == 0) {
    if (file_type == "mod") {

      if (is.null(javastics)) {
        warning(paste("No", "mod",
                      "files in the source workspace",
                      "the Javastics path must be given",
                      "as input for copying files from it"))
      }

      files_list <- list.files(
        path = file.path(javastics, "example",
                         full.names = TRUE,
                         pattern = patt))

    }
  }

  # nothing to do
  if (length(files_list) == 0) {
    warning(paste0("Not any '", file_desc[type_idx], "' file to copy!"))
    return()
  }

  # copy and treat of the copy return
  stat <- file.copy(
    from = files_list,
    to = out_dir,
    overwrite = overwrite
  )

  if (verbose) {
    message(paste("Copying", file_desc[type_idx], "files.\n"))
  }

  if (!all(stat)) {
    warning("Error when copying file(s): ",
            paste(basename(files_list[!stat]), collapse = ", "),
            "\nin\n",
            out_dir,
            "\n",
            "Consider to set as input: overwrite = TRUE")
  }
  return(invisible(stat))
}
