.giacR_package_path <- function() {
  pkg_path <- system.file(package = "giacR")
  file.path(pkg_path, "Giac", "giacwasm.js")
}

.giac_is_installed <- function() {
  possible_file <- .giacR_package_path()
  file.exists(possible_file)
}

#' @importFrom utils flush.console
#' @importFrom utils download.file
#' @noRd
.giac_download <- function() {
  if(.giac_is_installed()) {
    return(invisible())
  }
  giac_url <- "https://www-fourier.univ-grenoble-alpes.fr/~parisse/giacwasm.js"
  dest_folder <- dirname(.giacR_package_path())
  if(!dir.exists(dest_folder)) {
    dir.create(dest_folder)
  }

  temp_file <- file.path(tempdir(), basename(.giacR_package_path()))

  flush.console()
  dwnld <- download.file(
    url = giac_url, destfile = temp_file, mode = "wb", cacheOK = FALSE,
    quiet = TRUE
  )

  if(dwnld != 0L) {
    warning("Downloading has failed.")
    invisible()
  } else {
    file.copy(temp_file, .giacR_package_path())
  }
}
