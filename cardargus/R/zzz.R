# zzz.R - Package load and attach hooks

#' @importFrom cli cli_alert_success cli_alert_info cli_alert_warning
#' @importFrom cli cli_alert_danger cli_progress_step
#' @importFrom cli cli_h1 cli_h2 cli_h3 cli_bullets cli_warn cli_abort
#' @importFrom grDevices col2rgb as.raster
#' @importFrom stats runif setNames
#' @importFrom utils download.file unzip
#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  # 1) Register knitr helpers only if knitr is installed
  if (requireNamespace("knitr", quietly = TRUE)) {
    tryCatch(
      register_cardargus_knitr(),
      error = function(e) {
        # Silently ignore to avoid breaking package load
      }
    )
  }
  
  # 2) Check for required system fonts
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    tryCatch({
      fonts <- systemfonts::system_fonts()
      jost_available <- "Jost" %in% fonts$family
      
      if (!jost_available) {
        # Try to register Jost from Google Fonts if sysfonts is available
        if (requireNamespace("sysfonts", quietly = TRUE)) {
          tryCatch({
            sysfonts::font_add_google("Jost", "Jost")
          }, error = function(e) {
            # Silently ignore - user will be notified at attach
          })
        }
      }
    }, error = function(e) {
      # Silently ignore font check errors
    })
  }
}
