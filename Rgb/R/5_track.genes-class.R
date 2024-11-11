# Track class for genes
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

# Only defining new drawing parameters defaults (none currently)
setRefClass(
	Class = "track.genes",
	contains = "track.table",
	methods = list(
		
show = function(include=FALSE, fieldWidth=10) {
"Interactive printing
- include   : single logical value, if TRUE class name will not be printed."
	
	# Class name
	if(!isTRUE(include)) { cat("\n  \"track.genes\" reference class object\n")
	} else               { cat("\n  Extends \"track.genes\"\n")
	}
	
	# Inherited show()
	callSuper(include=TRUE, fieldWidth=fieldWidth)
}
		
	)
)

# Constructor
track.genes <- function(...) {
	# Inheritance
	object <- new("track.genes")
	object$import(track.table(...))
	
	return(object)
}

