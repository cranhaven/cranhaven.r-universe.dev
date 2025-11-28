.onAttach <- function(libname, pkgname) {
 Sh.ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), fields="Version")
 packageStartupMessage(paste("package 'shipunov', version", Sh.ver))
}
