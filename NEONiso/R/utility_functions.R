# general utility functions in this file.
# this file is intended to keep short utility functions
# that do *not* need to be exported,  to help keep down on
# number of files present in the repo/package. -rpf 200214.
# 200219 - added a water isotope function.
#-----------------------------------------------

#' List terrestrial core sites
#'
#' Returns a vector of four-letter NEON site codes for the
#' core terrestrial sites that have TIS instrumentation.
#'
#' @return A vector listing NEON core terrestrial sites.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @export
#' @examples
#' terrestrial_core_sites()
terrestrial_core_sites <- function() {

  # core sites as of 190523.

  core_sites <- c("BONA", "CLBJ", "CPER", "GUAN", "HARV", "KONZ",
                  "NIWO", "ONAQ", "ORNL", "OSBS", "PUUM", "SCBI",
                  "SJER", "SRER", "TALL", "TOOL", "UNDE", "WOOD",
                  "WREF", "YELL")

  return(core_sites)
}

#' List terrestrial gradient sites
#'
#' Returns a vector of four-letter NEON site codes for the
#' gradient terrestrial sites that have TIS instrumentation.
#'
#' @return A vector listing NEON gradient terrestrial sites.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @export
#' @examples
#' terrestrial_gradient_sites()

terrestrial_gradient_sites <- function() {

  # relocatable sites as of 190523.
  # renamed to gradient sites. function updated 230830.

  grad_sites <- c("ABBY", "BARR", "BART", "BLAN", "DCFS", "DEJU",
                  "DELA", "DSNY", "GRSM", "HEAL", "JERC", "JORN",
                  "KONA", "LAJA", "LENO", "MLBS", "MOAB", "NOGP",
                  "OAES", "RMNP", "SERC", "SOAP", "STEI", "STER",
                  "TEAK", "TREE", "UKFS")

  return(grad_sites)

}

#' List sites with water vapor isotope ratios.
#'
#' Returns a vector of four-letter NEON site codes for the
#' terrestrial sites that have water vapor isotope ratio instrumentation.
#'
#' @return A vector listing NEON sites measuring water vapor isotope ratios.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @export
#' @examples
#' water_isotope_sites()

water_isotope_sites <- function() {

  # water isotope sites as of 200608.

  wiso_sites <- c("BONA", "CLBJ", "CPER", "GUAN", "HARV", "KONZ",
                  "NIWO", "ONAQ", "ORNL", "OSBS", "PUUM", "SCBI",
                  "SJER", "SRER", "TALL", "TOOL", "UNDE", "WOOD",
                  "WREF", "YELL", "BARR")

  return(wiso_sites)

}

#' Manage a local eddy covariance (EC) data archive.
#'
#' Utility function to help retrieve new EC data and/or prune duplicates,
#' as NEON provisions new data or re-provisions data for an existing site
#' and month.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param file_dir Specify the root directory where the local EC store is kept.
#' @param get Pull down data from NEON API that does not exist locally?
#' @param unzip_files NEON gzips the hdf5 files, should we unzip any gzipped
#'             files within file_dir? (Searches recursively)
#' @param trim Search through local holdings, and remove older file where
#'             there are duplicates?
#' @param dry_run List files identified as duplicates, but do not actually
#'             delete them? Default true to prevent unintended data loss.
#' @param sites Which sites to retrieve data from? Default will be all sites
#'              with available data, but can specify a single site or a vector
#'              here.
#' @param release Download data corresponding to a specific release? Defaults
#'              to "RELEASE-2024." To download all data, including provisional
#'              data, set to NULL.
#' @export
#'
#' @return Returns nothing to the environment, but will download new NEON HDF5
#'         files for selected sites (if `get = TRUE`), unzip them in the local
#'         file directory (if `unzip_files = TRUE`), and identify and remove
#'         suspected duplicate files (if `trim = TRUE` and `dry_run = FALSE`).

manage_local_EC_archive <- function(file_dir,
                                    get = TRUE,
                                    unzip_files = TRUE,
                                    trim = FALSE,
                                    dry_run = TRUE,
                                    sites = "all",
                                    release = "RELEASE-2024") {
  if (!is.null(release)) {
    file_dir2 <- paste(file_dir, release, sep = "/")
  } else {
    file_dir2 <- file_dir
  }

  if (get == TRUE) {

    # script to pull down EC data files.
    data_product <- "DP4.00200.001"
    neon_api_address <- "https://data.neonscience.org/api/v0/products/"

    # make copy of site list available to this function
    csites <- terrestrial_core_sites()
    gsites <- terrestrial_gradient_sites()

    # see what sites have data
    data_request <- httr::GET(paste0(neon_api_address, data_product))
    # parse JSON object into an R list
    available <- httr::content(data_request, as = "parsed")
    # get number of sites.
    nsites <- length(available$data$siteCodes)

    # loop through sites, and download data.
    for (i in 1:nsites) {
      # get site name
      site_name <- available$data$siteCodes[[i]]$siteCode

      # check to see if site [i] is a core/gradient site
      if (!(site_name %in% csites | site_name %in% gsites)) {
        print(paste("Site name", site_name,
                    "is not a core or gradient site...skipping..."))
        next
      } else if (sites == "all" | site_name %in% sites) {
        print(paste("Checking site:", site_name))
      } else {
        print(paste("Site", site_name, "has not been requested...skipping..."))
        next
      }

      # get a vector of site months available for site i
      if (!is.null(release)) {
        release_index <- which(sapply(
                                      available$data$siteCodes[[i]]$availableReleases,
                                      "[[",
                                      1) == release)
        site_months <- unlist(available$data$siteCodes[[i]]$availableReleases[[release_index]]$availableMonths)
      } else {
        site_months <- unlist(available$data$siteCodes[[i]]$availableMonths)
      }

      # okay, check to see if data folder exists for site, otherwise create.

      ifelse(!dir.exists(paste(file_dir2, site_name, sep = "/")),
             dir.create(paste(file_dir2, site_name, sep = "/"),
                        recursive = TRUE),
             FALSE)

      # okay - now loop through months and get the data files.
      if (!is.null(length(site_months))) {

        for (j in seq_along(site_months)) {

          # re-query api w/ given site code and month.
          sitemonth_urls_json <- httr::GET(
            unlist(available$data$siteCodes[[i]]$availableDataUrls[j]))

          # list files returned.
          sitemonth_urls_parsed <- httr::content(sitemonth_urls_json,
                                                 as = "parsed")

          # extract just file names and URLs
          fnames <- sapply(sitemonth_urls_parsed$data$files, "[[", "name")
          furls  <- sapply(sitemonth_urls_parsed$data$files, "[[", "url")
          
          # get basic zipfile for now, but should kick out to a
          # function argument later on.
          fnames_basic <- (grepl("basic", fnames) & (grepl("h5.gz", fnames) |
                                                       grepl("h5", fnames)))

          # check to see if files already exist, and download if missing.
          dl_names <- fnames[fnames_basic]
          dl_urls  <- furls[fnames_basic]

          for (k in seq_along(dl_names)) {
            if (!length(dl_names[k]) == 0) {
              if (!is.na(dl_names[k])) {
                # check to see if file exists in folder
                if (file.exists(paste0(file_dir2, "/",
                                       site_name,
                                       "/",
                                       dl_names[k])) |
                    file.exists(paste0(file_dir2,
                                       "/",
                                       site_name,
                                       "/",
                                       substr(dl_names[k],
                                              1,
                                              nchar(dl_names[k]) - 3)
                               )
                    )
                ) {
                  print(paste(dl_names[k], "exists...skipping..."))
                  next
                } else { #doesn't exist, so download it.
                  print(paste("Downloading", dl_names[k]))
                  httr::GET(url = dl_urls[k],
                            httr::write_disk(paste0(file_dir2, "/",
                                                    site_name,
                                                    "/",
                                                    dl_names[k]),
                                             overwrite = TRUE))

                } # if
              }
            }
          } # k loop
        } # j loop
      } # if is.null

      Sys.sleep(100) # need to wait due to API throttling.

    } # i loop
  } # get branch.

  # unzip files if requested.
  if (get == TRUE & unzip_files == TRUE) {
    # list the files in file_dir
    files <- list.files(path = paste0(file_dir2, "/"),
                        pattern = "*.gz",
                        recursive = TRUE,
                        full.names = TRUE)

    if (length(files) > 0) {
      lapply(files, function(x) {
        R.utils::gunzip(x)
      })
    }
  }

  if (trim == TRUE) {
    # list the files in file_dir
    files <- list.files(path = file_dir2,
                        pattern = "*.h5",
                        recursive = TRUE,
                        full.names = TRUE)

    #need to extract sites, months from file names.
    file_pieces <- strsplit(files, split = ".", fixed = TRUE)

    #Unless there's a very unusual file path, the
    #site name should be element 3 of resulting list,
    #and site year/month should be element 8. We'll
    #also want element 10, which is either 'h5' in the old
    #file naming convention, or is the publication date/time.

    sites <- sapply(file_pieces, "[[", 3)
    yrmn  <- sapply(file_pieces, "[[", 8)
    fdiff <- sapply(file_pieces, "[[", 10)

    site_list <- unique(sites)

    for (i in seq_along(site_list)) {
      # get list of files where site == site[i]
      isite <- sites == site_list[i]

      # check to see if there are duplicates of yrmn
      yrmn_isite <- yrmn[isite]
      fdiff_isite <- fdiff[isite]
      files_isite <- files[isite]

      # test to see if there are any duplicates
      if (sum(duplicated(yrmn_isite) > 0)) {

        # print list of files that are duplicated?
        dups <- duplicated(yrmn_isite) | duplicated(yrmn_isite, fromLast = TRUE)

        # narrow list to just list of candidate duplicate files.
        dup_candidates <- files_isite[dups]
        dup_yrmn       <- yrmn_isite[dups]
        dup_fdiff      <- fdiff_isite[dups]

        # check to see if one site has an 'h5' fdiff and the duplicates
        # have a date.
        if (!is.null(dup_candidates) & any(dup_fdiff == "h5")) {
          h5files <- fdiff_isite[dups] == "h5"
          print(paste("Removing:", dup_candidates[h5files]))
          if (!dry_run) {
            file.remove(dup_candidates[h5files]) # remove files.
          }
        } else { # none are simply h5, so need to determine most recent file.
          for (i in seq_along(unique(dup_yrmn))) {
            # get times associated w/ particular duplicate.
            h5_times <- as.POSIXct(dup_fdiff[dup_yrmn == unique(dup_yrmn)[i]],
                                   format = "%Y%m%dT%H%M%SZ")
            # determine which files are not the most recent.
            # get file names for only this yrmn.
            dups_yrmn <- dup_candidates[(dup_yrmn == unique(dup_yrmn)[i]) &
                                          (h5_times != max(h5_times))]
            # print which files to remove
            print(paste("Removing:", dups_yrmn))
            if (!dry_run) {
              file.remove(dups_yrmn)
            }
          }
        }
      }
    }
  }

}
