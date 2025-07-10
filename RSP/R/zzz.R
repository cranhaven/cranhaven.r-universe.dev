.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "img",
    directoryPath = system.file(
      "www/img",
      package = "RSP"
    )
  )
  shiny::addResourcePath("sbs", system.file("www", package="shinyBS"))
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("img")
}
