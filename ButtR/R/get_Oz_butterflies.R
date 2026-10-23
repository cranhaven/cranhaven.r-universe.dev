# Download one or more files from the repository
downloadFiles <- function(files, subset, destDir, quiet) {
  # Download them one at a time
  for (idx in subset) {
    # Define the path where the file will be saved locally
    destfile <- file.path(destDir, files$file[idx])

    # Download the spreadsheet from the URL and save it in the specified path
    utils::download.file(url = files$url[idx], destfile = destfile, mode = "wb", quiet = quiet)
  }
}

# Checks whether requested values for some parameter exist in the database. Throws an error if not
checkValuesInSet <- function(what1, whatn, requested, available) {
  badVals <- !tolower(requested) %in% unique(tolower(available))
  if (any(badVals)) {
    if (sum(badVals) == 1) {
      stop(sprintf(
        "The following requested %s does not exist in the OzButterflies database: %s",
        what1, requested[badVals]
      ))
    } else {
      stop(sprintf(
        "The following requested %s do not exist in the OzButterflies database: %s",
        whatn, paste(requested[badVals], collapse = ", ")
      ))
    }
  }
}

#' Download and install the OzButterflies Database
#'
#' Simplifies downloading the *OzButterflies database* (Ref) to a local folder.
#' The function allows users to download specific subsets of the database by
#' applying multiple filters, such as species name, genus, site, family,
#' specific IDs, and more.
#'
#' Be aware that downloading very large files can take many hours, depending on
#' the speed of your Internet connection.
#'
#' \code{get_Oz_butterflies} will not remove local existing files, so subsequent
#' calls can be used to add to the installed database.
#'
#' If you receive an intermittent error such as "`status was 'SSL peer
#' certificate or SSH remote key was not OK'`", try using a different download
#' method. ButtR downloads files by calling \code{\link[utils]{download.file}},
#' so the download method can be specified by setting the `download.file.method`
#' option; for example, `options(download.file.method = "curl")`. See the
#' \code{\link[utils]{download.file}} help for further details.
#'
#' @param species Optional vector of binomial names of species of interest. If
#'   specified, only species from this list will be included in the local
#'   database.
#' @param genus If specified, only specimens from this genus will be installed.
#' @param family If specified, only specimens from this family will be
#'   installed.
#' @param sex If specified, only specimens of this sex (\code{"male"},
#'   \code{"female"} or \code{"unknown"}) will be installed.
#' @param year If specified, only specimens collected during these years will be
#'   installed (options are 2022 or 2023).
#' @param site If specified, only specimens collected at these sites will be
#'   installed.
#' @param spectra If specified, only specimens with the specified spectra value
#'   will be installed (\code{"y"} or \code{"n"}).
#' @param sampleIDs If specified, only specimens with the specified IDs will be
#'   installed.
#' @param download_images Specifies whether \code{"raw"} and/or \code{"jpeg"}
#'   images should be downloaded. Only images with the specified type(s) will be
#'   downloaded. In versions 1 to 3 of the Oz butterflies database, raw files
#'   are in Sony raw format (`.ARW`) database. From version 4, raw files are in
#'   the Adobe Digital negative format (`.DNG`) (see the `db_version` parameter).
#' @param download_dna If \code{TRUE} (the default), DNA files (.ab1) will be
#'   downloaded and installed. If \code{FALSE}, DNA files will not be installed.
#' @param save_folder Folder where the downloaded database will be saved. This
#'   argument must be provided by the user.
#' @param timeout Maximum time allowed (in seconds) to download _each_ file;
#'   default is 10 hours. The time required will depend on the speed of your
#'   Internet connection and the parts of the database that you choose to
#'   download. If you experience an error message such as "\code{Timeout of 36000
#'   seconds was reached}", try increasing the timeout.
#' @param quiet If \code{FALSE}, a progress bar is displayed showing the
#'   download progress for _each_ file as it is downloaded from the repository,
#'   and informational messages are printed to the console. Specify \code{quiet
#'   = TRUE} to prevent progress bar display.
#' @param db_version Version of the database to download. \code{NA} (the default)
#'   means download the latest version. An integer version number will download
#'   that version of the database. Note that in version \code{3} and earlier,
#'   raw image files were in \code{.ARW} format. Starting from version 4, raw
#'   images are in \code{.DNG} format.
#'
#' @returns The installation folder (\code{save_folder}) in canonical form in
#'   invisible form (which means it is not automatically printed).
#'
#' @examples
#' \dontrun{
#' # Download the full OzButterflies Database
#' get_Oz_butterflies(save_folder = "OzButterflies")
#'
#' # Get data only for Delias aganippe
#' get_Oz_butterflies(species = "Delias aganippe", save_folder = "Delias_aganippe")
#'
#' # Get data for all species of the genus Delias
#' get_Oz_butterflies(genus = "Delias", save_folder = "Delias_database")
#'
#' # Get all species within the Nymphalidae family
#' get_Oz_butterflies(family = "Nymphalidae", save_folder = "Nymphalidae_data")
#'
#' # Get raw files in .ARW format (from version 3 of the database)
#' get_Oz_butterflies(
#'   download_images = "raw",
#'   db_version = 3,
#'   species = "Delias aganippe",
#'   save_folder = "Delias_raw_ARW"
#' )
#'
#' # Get raw files in .DNG format (from version 4 of the database)
#' get_Oz_butterflies(
#'   download_images = "raw",
#'   db_version = 4,
#'   species = "Delias aganippe",
#'   save_folder = "Delias_raw_DNG"
#' )
#' }
#'
#' @export
get_Oz_butterflies <- function(species = NULL,
                               genus = NULL,
                               family = NULL,
                               sex = NULL,
                               year = NULL,
                               site = NULL,
                               spectra = NULL,
                               sampleIDs = NULL,
                               download_images = c("raw", "jpeg"),
                               download_dna = TRUE,
                               save_folder = NULL,
                               timeout = 10 * 60 * 60,
                               quiet = FALSE,
                               db_version = NA) {

  download_images <- match.arg(download_images, several.ok = TRUE)

  # Require the user to provide the destination folder
  if (is.null(save_folder)) {
    stop("Please provide 'save_folder' to specify where the OzButterflies database should be downloaded.")
  }

  # Increase default timeout for download.file
  oldTimeout <- getOption("timeout")
  on.exit(options(timeout = oldTimeout), add = TRUE)
  options(timeout = max(timeout, oldTimeout))

  # Check if the destination directory exists; if not, create it
  if (!dir.exists(save_folder)) {
    dir.create(save_folder, recursive = TRUE)
  }

  # List all files in the database
  files <- ListDbsFiles(db_version)

  # Download the metadata - all files that aren't .zip, except for filter_holders.zip which is metadata
  metadata <- which(!grepl("\\.zip$", files$file) | files$file == "filter_holders.zip")
  if (length(metadata) == 0) {
    stop("Internal error: Unable to locate OzButterflies metadata file in repository")
  }
  downloadFiles(files, metadata, save_folder, quiet)

  # Identify the local spreadsheet file path
  spread_sheet <- file.path(save_folder, "Oz_butterflies.csv")

  # Read metadata
  meta_data <- utils::read.csv(spread_sheet)

  # Create a new column "full_species" that combines the "genus" and "species" columns into a full species name
  meta_data$full_species <- paste(meta_data$Genus, meta_data$Species)

  # Start by selecting all rows (TRUE will be repeated nrow times)
  rows <- rep(TRUE, nrow(meta_data))

  # If the user has specified a family (or more), this will filter the 'meta_data'
  # to include only the rows where the 'Family' column matches any specified family names.
  # The '&' operator is used to combine this condition with any previously applied,
  # ensuring that the final set of rows meets all specified criteria.
  if (length(family) > 0) {
    checkValuesInSet("family", "families", tolower(family), tolower(meta_data$Family))
    rows <- rows & tolower(meta_data$Family) %in% tolower(family)
  }

  # If genus is specified, update the filtering
  if (length(genus) > 0) {
    checkValuesInSet("genus", "genus", tolower(genus), tolower(meta_data$Genus))
    rows <- rows & tolower(meta_data$Genus) %in% tolower(genus)
  }

  # If species is specified, update the filtering
  if (length(species) > 0) {
    checkValuesInSet("species", "species", tolower(species), tolower(meta_data$full_species))
    rows <- rows & tolower(meta_data$full_species) %in% tolower(species)
  }

  # If site is specified, update the filtering
  if (length(site) > 0) {
    checkValuesInSet("site", "sites", tolower(site), tolower(meta_data$Site))
    rows <- rows & meta_data$Site %in% site
  }

  # If spectra is specified, update the filtering
  if (length(spectra) > 0) {
    checkValuesInSet("spectra", "spectra", tolower(spectra), tolower(meta_data$Spectra))
    rows <- rows & meta_data$Spectra %in% spectra
  }

  # If sex is specified, update the filtering
  if (length(sex) > 0) {
    checkValuesInSet("sex", "sexes", tolower(sex), tolower(meta_data$Sex))
    rows <- rows & tolower(meta_data$Sex) %in% tolower(sex)
  }

  # Filter only the specified years
  if (length(year) > 0) {
    ddate <- as.Date(meta_data$Date)
    dyear <- format(ddate, "%Y")
    rows <- rows & dyear %in% as.character(year)
  }

  # If sample ID is specified, update the filtering
  if (length(sampleIDs) > 0) {
    checkValuesInSet("sampleIDs", "sampleID", tolower(sampleIDs), tolower(meta_data$ID))
    rows <- rows & meta_data$ID %in% sampleIDs
  }

  # Get the unique zip file names that match the filtered criteria
  zips <- unique(meta_data$Repo.zipname[rows])

  # Create a temporary directory to store zip files
  tmp_dir <- tempdir()

  # Iterate over each zip file
  for (zip in zips) {
    # Define the path to be saved temporarily
    zip_path <- file.path(tmp_dir, zip)

    # Get the URL of the corresponding zip file
    if (!zip %in% files$file) {
      stop(sprintf(
        "Internal error: zip file %s not found in repository: files in repo are %s",
        zip, paste(files$file, collapse = ", ")
      ))
    }
    url <- files$url[files$file == zip]

    # Download the zip file from the URL
    utils::download.file(url = url, destfile = zip_path, mode = "wb", quiet = quiet)

    # List files inside the zip archive without extracting them
    zip_content <- utils::unzip(zip_path, list = TRUE)

    # Select files in this zip that match the filtered sample IDs.
    # Assume sample folder is sample ID
    samplesInZip <- basename(dirname(zip_content$Name))
    selected_files <- zip_content$Name[samplesInZip %in% meta_data$ID[rows]]

    # If not all image files were requested, filter them out
    if (!"raw" %in% download_images) {
      selected_files <- selected_files[grep("\\.dng$|\\.arw$", selected_files, invert = TRUE, ignore.case = TRUE)]
    }
    if (!"jpeg" %in% download_images) {
      selected_files <- selected_files[grep("\\.jpg$", selected_files, invert = TRUE, ignore.case = TRUE)]
    }

    # Don't install DNA if not required
    if (!download_dna) {
      selected_files <- selected_files[grep("\\.ab1$", selected_files, invert = TRUE, ignore.case = TRUE)]
    }

    # If there are matching files, extract only those files
    if (length(selected_files) > 0) {
      utils::unzip(zip_path, files = selected_files, exdir = save_folder)
    }

    # Delete the zip file so that disk space use is minimised
    unlink(zip_path)
  }

  invisible(normalizePath(save_folder))
}
