# Track class for gene exons
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

# Only defining new drawing parameters defaults (none currently)
setRefClass(
	Class = "track.exons",
	contains = "track.table",
	methods = list(

defaultParams = function(...) {
"Returns class-specific defaults for graphical parameters. Inheriting class should overload it to define their own defaults.
- ...   : may be used by inheriting methods, especially for inter-dependant parameters."
	
	params <- callSuper(...)
	
	params$maxElements <- 200
	params$maxDepth <- 10
	params$labelStrand <- TRUE
	params$groupBy <- "transcript"
	params$groupPosition <- "groupPosition"
	params$groupSize <- "groupSize"
	
	return(params)
},

show = function(include=FALSE, fieldWidth=10) {
"Interactive printing
- include   : single logical value, if TRUE class name will not be printed."
	
	# Class name
	if(!isTRUE(include)) { cat("\n  \"track.exons\" reference class object\n")
	} else               { cat("\n  Extends \"track.exons\"\n")
	}
	
	# Inherited show()
	callSuper(include=TRUE, fieldWidth=fieldWidth)
}

	)
)

# Constructor
track.exons <- function(...) {
	# Inheritance
	object <- new("track.exons")
	object$import(track.table(...))
	
	return(object)
}

