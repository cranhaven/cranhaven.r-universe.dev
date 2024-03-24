.onAttach = function(libname, pkgname) {
    version = packageDescription(pkgname, fields = "Version")
    
    msg = paste0("========================================
", pkgname, " version ", 
        version, "
Documentation page: https://pacviz.sriley.dev
This message can be suppressed by:
  suppressPackageStartupMessages(library(pacviz))
========================================
")
    packageStartupMessage(msg)
}
