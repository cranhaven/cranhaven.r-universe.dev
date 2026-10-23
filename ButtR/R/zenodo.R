# Interface with Zenodo

# The deposition ID of the ButtR repo in Zenodo. This is the "all versions"
# record ID, and we explicitly look up the latest version
BUTTR_DEPOSITION <- "15881960"

# Deposition IDs for specific versions
BUTTR_DEP_V <- c("15881961", "17178034", "17533293", "19019034")

# Zenodo API constants
ZENODO_DOI_PREFIX <- "https://doi.org/10.5281/zenodo."
ZENODO_REC_URL <- "https://zenodo.org/api/records/"

# Returns the Zenodo deposition ID for a specified version.
#
# @param version Integer version of the OzButterflies to retrieve from Zenodo.
#   `NA` or `NULL` indicate the most recent version.
zenodoVersionToDepo <- function(version) {
  # If a version has been explicitly specified, use it
  if (is.numeric(version) && version >= 1 && version <= length(BUTTR_DEP_V)) {
    BUTTR_DEP_V[version]
  } else {
    # Default to the most recent version
    BUTTR_DEPOSITION
  }
}

# Given the record ID from either a specific verion or the concept DOI (i.e. the
# DOI that resolves to the latest version, whatever that is), returns the record
# URL for the appropriate version
getZenodoVersionURL <- function(deposition = BUTTR_DEPOSITION) {
  # Fetch the DOI record. If the deposition is for the latest version rather
  # than an explicit version, it will automatically redirect us to the newest
  # version
  resp <- httr::HEAD(paste0(ZENODO_DOI_PREFIX, deposition))
  httr::stop_for_status(resp)
  # We have been redirected to the web page for the latest version. Derive the
  # API url from the HTML URL
  sub("/records/", "/api/records/", resp$url)
}

# Given a URL, downloads it and interprets it as JSON.
getJSON <- function(url) {
  resp <- httr::GET(url)
  httr::stop_for_status(resp)
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, flatten = TRUE)
}

# Given a Dryad dataset deposition ID, returns a data frame with the names of all files
# available for download, together with their download URLs
#
# @returns data frame with 2 columns, `file` with the name of the file stored in
#   Dryad, and `url` with the URL that can be used to retrieve the file.
#
listFilesInZenodo <- function(deposition) {
  # List the Zenodo record
  rec <- getJSON(getZenodoVersionURL(deposition))

  # AI suggests that downloads could be made more reliable by using the direct
  # Zenodo link rather than the API link, but results were not more reliable for me
  # # Try to make downloads more reliable - I get an intermittent error:
  # # cannot open URL ...
  # # In addition: Warning message:
  # # In .rs.downloadFile(url = files$url[idx], destfile = destfile, quiet = quiet,  :
  # #      URL '...': status was 'SSL peer certificate or SSH remote key was not OK'
  # # This is an attempt to solve that
  # apiToDirect <- function(url) {
  #   sub("\\/content$", "?download=1", sub("api/records/", "records/", url))
  # }
  #
  # rec$files$links.self <- apiToDirect(rec$files$links.self)

  # Available files
  stats::setNames(rec$files[, c("key", "links.self", "size")],
                  c("file", "url", "size"))
}

# Function for testing without using Dryad. This is a drop-in replacement for
# listFilesInDryad. It may not work correctly for files with spaces in their
# names.
#
# @examples
# # To use in testing, do something like:
# testthat::local_mocked_bindings(ListDbsFiles = function() {
#    listLocalFiles(testthat::test_path("testdata/repo"))
# })
# get_species(...)
#
listLocalFiles <- function(path, encode = FALSE) {
  files <- list.files(path, recursive = TRUE)

  # Remove unwanted files
  files <- grep("^~\\$", files, invert = TRUE, value = TRUE)

  # Minimal fix: return empty data frame if no files were found
  if (length(files) == 0) {
    return(data.frame(
      file = character(0),
      url = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Construct URLs
  urls <- paste0("file://", normalizePath(file.path(path, files), winslash = "/"))

  if (encode) {
    urls <- utils::URLencode(urls)
  }

  data.frame(
    file = files,
    url = urls,
    stringsAsFactors = FALSE
  )
}

#################################################################################

# Private function to get list of available files in the database. This is the
# function to call to obtain formation about the database contents. Done this
# way to facilitate testing without using Dryad, but without exposing something
# publicly
ListDbsFiles <- function(version) listFilesInZenodo(zenodoVersionToDepo(version))


#Uncomment this for debugging/development using local repository. For unit
#tests, use testthat::local_mocked_bindings instead
# ListDbsFiles = function(version) listLocalFiles(testthat::test_path("testdata/repo"))
