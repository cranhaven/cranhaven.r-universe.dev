##' \code{rreg} package
##'
##' Data visualization for Norwegian Health Quality Registries with R. This package
##' will assist and standardize the visualization of data from the Norwegian Health
##' Quality Registries. The standardization is based on the requirement specified by
##' the Nasjonalt servicemiljø for medisinske kvalitetsregistre.
##'
##' @author Yusman Kamaleri <ybkamaleri@gmail.com>
##' @docType package
##' @name rreg

NULL

## quiets concerns of R CMD check re: the .'s
if(getRversion() >= "3.2.2") utils::globalVariables(c(".xname",
                                                      "yvar",
                                                      "txtpos",
                                                      "xvar",
                                                      "yvar",
                                                      "ulvar",
                                                      "llvar",
                                                      "ref",
                                                      "ylocal",
                                                      "ycomp"))
