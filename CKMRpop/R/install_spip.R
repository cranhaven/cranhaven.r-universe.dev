

#' Download the spip binary and install it where CKMRpop expects it
#'
#' This checks the operating system and installs the correct version
#' (either Darwin or Linux for Mac or Linux, respectively.)  To install
#' the spip binary this function downloads it from its GitHub site.  It also
#' installs a windows implementation of awk.
#' @param Dir the directory to install spip into.  Because of restrictions
#' on functions writing to the user's home filespace, this is set, by default,
#' to a temporary directory.  But to really use this function to install spip,
#' this parameter must be set to `system.file(package = "CKMRpop")`.
#' @export
#' @return No return value.  Called for side effect of installing the 'spip' binary.
#' @examples
#' \dontrun{
#' install_spip(Dir = system.file(package = "CKMRpop"))
#' }
install_spip <- function(
  Dir = tempfile()
) {

  if(Dir != system.file(package = "CKMRpop")) {
    message("\n*** Note: To properly install spip, the function install_spip() must be called like this: ***\n\n    install_spip(Dir = system.file(package = \"CKMRpop\"))
\n*** The current invocation of install_spip() will not properly install it. ***")
  }

  # first check the OS
  Sys <- Sys.info()["sysname"]

  if(!(Sys %in% c("Darwin", "Linux", "Windows"))) {
    stop(paste("spip binary not available for operating system ", Sys, collapse = ""))
  }

  # then get the basename of the file we want
  pname <- paste("spip-", Sys, sep = "", collapse = "")
  if(Sys == "Windows") {
    pname <- paste(pname, ".zip", sep = "")
  }

  # have a variable to hold the base GitHub address:
  Git_base <- "https://github.com/eriqande/spip/raw/master/"

  Git_full <- paste(Git_base, pname, sep = "")

  # get the destination path and create the directory
  Dest_dir <- file.path(Dir, "bin")
  dir.create(Dest_dir, showWarnings = FALSE, recursive = TRUE)

  # record destination file name
  Dest_file <- file.path(Dest_dir, pname)

  # now, download the file and save to the correct destination:
  utils::download.file(url = Git_full, destfile = Dest_file)

  if(Sys == "Windows") {
    # extract the contents of the archive to the destination directory
    unzip(Dest_file, exdir = Dest_dir)

    # reset the name of the Dest file to not have the .zip extension
    Dest_file <- stringr::str_replace(Dest_file, "\\.zip$", ".exe")
  }
  # finally, change the file permissions to be user and group executable and writeable
  # and world readable
  Sys.chmod(Dest_file, mode = "0774", use_umask = FALSE)

  # if this is Windows, then also download gawk.exe and put it in the same spot
  if(Sys == "Windows") {
    message("Also downloading gawk.exe for Windows...")
    Dest_file <- file.path(Dest_dir, "gawk.zip")
    Gawk <- "https://github.com/eriqande/spip/raw/master/gawk.zip"
    utils::download.file(url = Gawk, destfile = Dest_file)
    unzip(Dest_file, exdir = Dest_dir)
    Dest_file <- stringr::str_replace(Dest_file, "\\.zip$", ".exe")
    Sys.chmod(Dest_file, mode = "0774", use_umask = FALSE)
  }


}


