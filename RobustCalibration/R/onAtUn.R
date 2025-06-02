##########################################################################
## start-up and clean-up functions
##########################################################################

.onAttach <- function(...) {
	
	date <- date()
	x <- regexpr("[0-9]{4}", date)
	this.year <- substr(date, x[1], x[1] + attr(x, "match.length") - 1)
	
	# # echo output to screen
	# packageStartupMessage("#########")
	# packageStartupMessage("##\n## RobustCalibration Package")
	# packageStartupMessage("## Copyright (C) 2018-", this.year,
	# 		" Mengyang Gu", sep="")
	# packageStartupMessage("#########")
}

.onUnload <- function(libpath) {
	library.dynam.unload("RobustCalibration", libpath)
}