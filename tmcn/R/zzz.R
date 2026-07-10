# TODO: Add comment
# 
# Author: jli
###############################################################################


.onAttach <- function(libname, pkgname ){
	if (!exists(".tmcnEnv", envir = .GlobalEnv)) {
		envir0 = as.environment(1)
		assign(".tmcnEnv", new.env(), envir = envir0)
	}
	options(tmcn.oldlocale = Sys.getlocale("LC_CTYPE"))
	packageStartupMessage( paste("# tmcn Version:", utils::packageDescription("tmcn", fields = "Version")) )
}

.onUnload <- function(libpath) {
	library.dynam.unload("tmcn", libpath)
}
