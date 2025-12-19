##########################################################################
## start-up and clean-up functions
#########################################################################

.onAttach <- function(...) {
  # echo output to screen
  packageStartupMessage("##\n## ads R package \n",
                        "## For Spatial Point Patterns Analysis \n",
                        "##\n")
}

.onUnload <- function(libpath) {
  library.dynam.unload("ads", libpath)
}