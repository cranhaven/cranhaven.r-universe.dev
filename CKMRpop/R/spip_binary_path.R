#' return the path where spip should be in the R system paths
#'
#' It expects it to be in the R package directory after external installation.
#' @keywords internal
spip_binary_path <- function() {
  bin_name <- paste("spip", Sys.info()["sysname"], sep = "-")
  if(Sys.info()["sysname"] == "Windows") {
    bin_name <- paste(bin_name, ".exe", sep = "")
  }
  file.path(system.file(package = "CKMRpop"), "bin", bin_name)
}
