.pkgglobalenv <- new.env(parent=emptyenv())
.onAttach <- function(libname, pkgname) {
  homeCOF <- base::readLines('https://researcharchive.calacademy.org/research/ichthyology/catalog/fishcatmain.asp',warn = FALSE)
  versionLine<-grep("version of",homeCOF)
  cof_version<-rvest::html_text(xml2::read_html(homeCOF[versionLine]))
  cof_version<-strsplit(cof_version," - |  ")[[1]][2]
  packageStartupMessage("Welcome to rcatfish: Access to the California Academy of Sciences Eschmeyer's Catalog of Fishes") 
  packageStartupMessage(paste0("Catalog of Fishes ", cof_version))
  packageStartupMessage(paste0("If you use this package please cite both:\n\nThis R package: Borstein, S.R., Dominy, B.E. & O'Meara, B.C.(2026) rcatifsh: An R interface to the California Academy of Sciences Eschmeyer's Catalog of Fishes. Available at:https://github.com/sborstein/rcatfish.\n\n", paste0("Catalog of Fishes: Fricke, R., Eschmeyer, W.N. & van der Laan, R. ", lubridate::year(Sys.Date()), ". Catalog of Fishes: Genera, Species, References. Available at: https://researcharchive.calacademy.org/research/ichthyology/catalog/fishcatmain.asp. Electronic version accessed ",format(Sys.Date(), "%d %m %Y"),".")))
  assign("cof_version", cof_version, envir=.pkgglobalenv)
}
