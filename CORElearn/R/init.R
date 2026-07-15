.onLoad <- function(lib, pkg) {
# .First.lib <- function(lib, pkg) {
	#library.dynam("CORElearn", pkg, lib)
	initCore(16384) 
}

.onUnload <- function(libpath) {
#.Last.lib <- function(libpath) {
	destroyCore()
	#library.dynam.unload("CORElearn", libpath)
}

initCore <- function(maxModels=16384)
{
	tmp <- .C(C_initCore, as.integer(maxModels)) ## maximal number of models
			
}

destroyCore <- function()
{
	tmp <- .C(C_destroyCore)
}

