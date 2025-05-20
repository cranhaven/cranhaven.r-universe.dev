


#' file path to be used in a call to spip
#'
#' This version checks to make sure it is there and throws an
#' error with a suggestion of how to get it if it is not there.
#' @keywords internal
spip_binary <- function() {
  if(!spip_exists()) {
    stop("Can't find the spip executable where it was expected
at ", spip_binary_path(), ".

Download and install spip with:

    install_spip(Dir = system.file(package = \"CKMRpop\"))

")
  }

  # then I should check to make sure it is executable
  # though I have not implemented that...

  # if so, return the path
  spip_binary_path()

}

