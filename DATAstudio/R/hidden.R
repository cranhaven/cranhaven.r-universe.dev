.onAttach <- function(libname, pkgname) {
  packageStartupMessage("## ============================================== ##")
  RFver <- read.dcf(file = system.file("DESCRIPTION", package = pkgname),
                    fields = "Version")
  packageStartupMessage(paste
                       ("##", pkgname, RFver, "                               ##"))
  packageStartupMessage("## ---------------------------------------------- ##")
  packageStartupMessage("##  Copyright (C) 2022                            ##")
  packageStartupMessage("##  M. de Carvalho                                ##")
  packageStartupMessage("##  University of Edinburgh                       ##")
  packageStartupMessage("## ============================================== ##")
  packageStartupMessage("")
}
