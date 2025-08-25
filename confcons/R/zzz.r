.onLoad <- function(libname, pkgname) {
	if (!("confcons_information_resolution" %in% names(options())))
		options("confcons_information_resolution" = 1e-4)
	invisible()
}
