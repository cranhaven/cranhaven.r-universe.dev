#' Retrieve the current version Number of the Eschmeyer's Catalog of Fishes Being Used
#' 
#' @description Returns the current version of Catalog of Fishes being used in the R session
#' @return Character Vector length of one with the Catalog of Fishes version
#' @examples
#' my.version <- rcatfish_version() 
#' @author Samuel R. Borstein
#' @references 
#' Fricke, R., Eschmeyer, W.N. & van der Laan, R. (Year Accessed). Eschmeyer's Catalog of Fishes: Genera, Species, References. https://researcharchive.calacademy.org/research/ichthyology/catalog/fishcatmain.asp.
#' @export

rcatfish_version<-function(){
  homeCOF <- readLines('https://researcharchive.calacademy.org/research/ichthyology/catalog/fishcatmain.asp',warn = FALSE)
  versionLine <- grep("version of",homeCOF)
  cof_version <- rvest::html_text(xml2::read_html(homeCOF[versionLine]))
  cof_version <- strsplit(cof_version," - |  ")[[1]][2]
  return(cof_version)
}
