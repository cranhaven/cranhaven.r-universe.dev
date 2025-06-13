.onAttach <- function(libname, pkgname) {

    packageStartupMessage(paste0("Set default RNG to L'Ecuyer-CMRG for a safe ",
                                 "parallel simulation."))

}

.onLoad <- function(libname, pkgname) {

    RNGkind("L'Ecuyer-CMRG")

}
