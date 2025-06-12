#' @name microcontax-package
#' @aliases microcontax-package
#' @aliases microcontax
#' @docType package
#' @title The ConTax data package
#' 
#' @description The consensus taxonomy for prokaryotes is a package of data sets designed to be the best
#' possible for training taxonomic classifiers based on 16S rRNA sequence data.
#' 
#' 
#' @details \tabular{ll}{
#' Package: \tab microcontax\cr
#' Type: \tab Package\cr
#' Version: \tab 1.2\cr
#' Date: \tab 2020-06-06\cr
#' License: \tab GPL-2\cr
#' }
#' 
#' @author Hilde Vinje, Kristian Liland, Lars Snipen.\cr
#' Maintainer: Lars Snipen <lars.snipen@nmbu.no>
#' 
#' 
#' @keywords package
#' @seealso \code{\link[microseq:microseq-package]{microseq}}
#' 
#' @export
#' @import microseq
#' 
microcontax <- function(){
  
}

.onLoad <- function(libname, pkgname) {
  repos = getOption("repos")
  repos["khliland"] = "https://khliland.github.io/drat/"
  options(repos = repos)
  invisible(repos)
}
