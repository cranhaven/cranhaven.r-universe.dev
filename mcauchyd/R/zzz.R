.onAttach <- function(libname, pkgname){
  packageStartupMessage(c("The multvardiv package contains the same tools as mcauchyd, ",
                          "and similar tools for other multivariate distributions.\n\n",
                          "mcauchyd will no longer be maintained. You may wish to install ",
                          "multvardiv instead."))
}
