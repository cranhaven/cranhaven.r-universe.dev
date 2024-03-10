
.onAttach <- function(libname, pkgname) {
  
  invisible(suppressPackageStartupMessages(
    sapply(c("brms"),
           requireNamespace, quietly = TRUE)
  ))
  
  
  
  if("rethinking" %in% (.packages())){
    packageStartupMessage("Package 'rethinking' detached and unloaded as it creates conflict",
            " \nwith the current rstan version ", utils::packageVersion('rstan'))
    detach("package:rethinking", unload=TRUE) 
  }
  
}

