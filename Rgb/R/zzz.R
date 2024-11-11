# C code binding at package loading and unloading
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

.onLoad <- function(libname, pkgname) {
	library.dynam("Rgb", pkgname, libname)
}
.onUnload <- function(libpath) {
	library.dynam.unload("Rgb", libpath)
}
