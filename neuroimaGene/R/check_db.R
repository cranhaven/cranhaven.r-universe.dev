#' Check neuroimaGene database downloaded
#'
#' Check if the NeuroimaGene database exists in the proper location prior to running the query and prompt user to download if not.
#' @keywords installation
#' @param timeout time to spend downloading the NeuroimaGene database in seconds (default = 900)
#' @export
#' @importFrom utils download.file
#' @returns no return value, called to give information on status of neuroimaGene
#' database and prompt user the user to download if resource file is missing.
#' @examples
#' check_db(timeout = 600)



# Define the function to check and download the database
check_db <- function(timeout=900) {
  pkg_dir <- system.file(package = "neuroimaGene")
  db_path <- file.path(pkg_dir, "extdata", "neuroimaGenefast.db")
  db_url <- "https://zenodo.org/records/10994978/files/NeuroimaGenefast.db"
  if (!file.exists(db_path)) {
    # Prompt the user for permission to download the database
    response <- readline(prompt = "NeuroimaGene database not found. It will require ~1.7Mb of space. Do you want to download it from Zenodo? (yes/no): ")
    if (tolower(response) == "yes") {
      message("Downloading database from Zenodo...\n(This step may take >10-15 minutes depending on download speed)")
      message(paste("user determined timeout:", timeout, "seconds"))
      options(timeout = timeout)  # Set timeout for download
      download_success <- FALSE
      tryCatch({
        if (Sys.info()["sysname"] == "Windows") {
          download.file(db_url, db_path, mode = "wb", method = "wininet", quiet = FALSE)
        } else {
          download.file(db_url, db_path, mode = "wb", method = "libcurl", quiet = FALSE)
        }
        download_success <- TRUE
      }, error = function(e) {
        file.remove(db_path)
        message("Download failed: ", e$message)
      })
      if (!download_success) {
        stop("Database download failed. Please try again later.")
      } else {
        message("Download complete.")
      }
    } else {
      message("Database download was not permitted by the user.")
    }
  } else {
    message("Database already exists locally.")
  }
}
