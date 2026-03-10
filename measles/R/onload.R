#' @noRd
#' @importFrom utils packageVersion
check_epiworldr_version <- function() {

  ev <- utils::packageVersion("epiworldR")
  required <- package_version("0.11.2.0")

  if (ev < required) {
    packageStartupMessage(
      "===================== {measles} note =====================\n",
      "The installed version of the {epiworldR} R package\n",
      "(", ev, ") is not the latest available. Since {epiworldR}\n",
      "is actively used in public health responses, the package\n",
      "is continually updated. Please install version ", required, ".\n",
      "=========================================================="
    )
  }

}
.onLoad <- function(libname, pkgname) {
  check_epiworldr_version()
}
