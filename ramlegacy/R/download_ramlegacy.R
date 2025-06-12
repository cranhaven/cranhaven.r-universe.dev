#' @name download_ramlegacy
#' @family ramlegacy functions
#' @title Download RAM Legacy Excel Database
#' @description Downloads a specified version of RAM Legacy Stock Assessment
#' Excel Database and as an RDS object to a local directory specified by
#' \code{\link{ram_dir}}. The function will check if the version requested
#' already exists on the user's computer and if it does then it prompts the user
#' to download it again unless `overwrite = TRUE` in which case the function will
#' download the version without displaying the prompt. The function also supports
#' downloading all the older versions (1.0, 2.0, 2.5, 3.0, 4.3) from
#' [a github repo](www.github.com/kshtzgupta1/ramlegacy-assets)
#' @param version A character vector of length 1 specifying the version number
#' of the database that should be downloaded. As of writing this package, the
#' available versions are "1.0","2.0", "2.5", "3.0", "4.3", "4.40", "4.41",
#' and "4.44". If the version argument is not specified then it defaults
#' to latest version (currently latest version is "4.44").
#' @param ram_path A string specifying the path of the local directory where
#' database will be downloaded. By default this path is set to the location
#' provided by \pkg{rappdirs} package and can be viewed by calling
#' \code{\link{ram_dir}}. Although this is not the \strong{recommended}
#' approach \code{download_ramlegacy} supports downloading to a user-specified path.
#' @param ram_url A string. By default it is set to the Zenodo url of the database.
#' Please \strong{do not pass} in any other url to \code{ram_url}.
#' @param overwrite This argument is only relevant if you are trying to
#' re-download a version that is already present locally in the rappdirs
#' directory. If overwrite = TRUE then user will not encounter the interactive prompt
#' that confirms whether to overwrite the version present locally.
#' @param quiet If TRUE, suppress status messages
#' @export
#' @examples
#' \donttest{
#' \dontshow{
#' Sys.setenv(RAM_HOME = tempfile())
#' }
#' # download version 4.44
#' download_ramlegacy(version = "4.44")
#'
#' # download version 1.0
#' download_ramlegacy(version = "1.0")
#' \dontshow{
#' Sys.setenv(RAM_HOME = tempfile())
#' }
#' # If version not specified then default
#' # to latest version (currently 4.44)
#' download_ramlegacy()
#' }
download_ramlegacy <- function(version = NULL, ram_path = NULL,
                               ram_url = "https://doi.org/10.5281/zenodo.2542918",
                               overwrite = FALSE, quiet = FALSE) {

  # According to the Zenodo this
  # DOI will always resolve to the latest url.

  # check internet connection and throw error
  # if there is a connection issue
  #net_check(ram_url, show_error = TRUE)

  # user_path, a boolean flag set to FALSE by default
  user_path <- FALSE

  # Get the latest version from the ram_url
  latest_vers <- find_latest(ram_url)

  # the user has specified a version
  if (!is.null(version)) {
    # make sure the version argument is formatted correctly
    version <- fmt_version(version)
    # check that it is a valid version
    check_version_arg(version)
  } else {
    version <- latest_vers
  }

  # ram_path argument
  if (is.null(ram_path)) {
    ram_path <- ram_dir()
  } else {
    check_path(ram_path)
    user_path <- TRUE
  }
  vers_path <- file.path(ram_path, version)
  ## Provided that the path is not set by user if
  # there is an existing ramlegacy version
  # ask the user what to do in interactive mode otherwise exit
  if (!overwrite) {
    if (!user_path) {
      if (version %in% find_local(ram_path)) {
        if (interactive()) {
          ans <- ask_yn(
            "Version ", version, " has already been downloaded.",
            " Overwrite?"
          )
          if (!ans) return("Not overwriting. Exiting the function.")
        } else {
          return(notify(paste(
            paste("Version", version, "has already been downloaded."),
            "Exiting the function."
          )))
        }
      }
    }
  }

  # create vers_path if it doesn't exist
  if (!dir.exists(vers_path)) {
    dir.create(vers_path, recursive = TRUE)
  }


  # notify the user
  if (!quiet) {
    notify("Downloading...this may take a while")
  }

  # constructing different urls and downloading depending on whether old
  # or recent version

  if (version < "4.4") {

    # construct download url for older versions
    bckup_url <- paste0(
      "https://github.com/kshtzgupta1/",
      "ramlegacy-assets/raw/master/RLSADB%20v"
    )
    ram_url <- paste0(bckup_url, version, "%20(assessment%20data%20only).xlsx")

    # create path for excel file
    excel_path <- file.path(
      vers_path,
      paste("RLSADB v", version, " (assessment data only).xlsx")
    )

    # download the older version and convert the excel database to rds file in
    # user's rappdirs directory using read_ramlegacy
    httr::GET(ram_url, httr::write_disk(excel_path, overwrite = TRUE))

    if (file.exists(excel_path)) {
      if (!quiet) {
        notify(paste0(
          "Finished downloading v", version,
          ". Saving the database as RDS object..."
        ))
      }
      completed(paste("Version", version, "successfully downloaded."))
      suppressWarnings(read_ramlegacy(vers_path, version))
    }
  } else {
    vers_doi_vec <- c("2542935", "2542927", "2542919")
    names(vers_doi_vec) <- c("4.40", "4.41", "4.44")
    # construct the download url
    doi <- unname(vers_doi_vec[version])
    ram_url <- paste0(
      "https://zenodo.org/record/", doi,
      "/files/RLSADB%20v", version, ".zip?download=1"
    )

    ## Download the zip file from zenodo website to temp
    tmp <- tempfile("ramlegacy_")
    httr::GET(ram_url, httr::write_disk(tmp))

    # unzip it and read it in
    if (file.exists(tmp)) {
      if (!quiet) {
        notify(paste(
          "Finished Downloading.",
          "Now unzipping it..."
        ))
      }
      utils::unzip(tmp, exdir = vers_path, overwrite = TRUE)
      if (!quiet) {
        notify("Saving the unzipped database as RDS object...")
      }
      vers_path <- file.path(vers_path, paste0("RLSADB v", version))
      vers_path <- file.path(vers_path, "DB Files With Assessment Data")
      suppressWarnings(read_ramlegacy(vers_path, version))
    }

    # check if file downloaded or not
    rds_path <- file.path(vers_path, paste0("v", version, ".rds"))
    if (file.exists(rds_path)) {
      completed(paste("Version", version, "successfully downloaded."))
    } else {
      not_completed(paste("Failed to download Version", version))
    }
  }

  invisible(TRUE)
}
