##########################################################################
## start-up and clean-up functions
##
## This software is distributed under the terms of the GNU GENERAL
## PUBLIC LICENSE Version 2 and above, April 2020.
##
## Copyright (C) 2020-present by Pulong Ma
##    
##########################################################################

.onAttach <- function(...) {
	
	date <- date()
	x <- regexpr("[0-9]{4}", date)
	this.year <- substr(date, x[1], x[1] + attr(x, "match.length") - 1)
	
	# echo output to screen
	packageStartupMessage("\n##################################")
	packageStartupMessage("##\n## Multifidelity computer model emulation, ARCokrig Package")
	packageStartupMessage("## Copyright (C) 2020-", this.year,
			" by Pulong Ma", sep="")
	packageStartupMessage("## Please cite this package and related papers")
	packageStartupMessage("####################################")
}

.onUnload <- function(libpath) {
	library.dynam.unload("ARCokrig", libpath)
}