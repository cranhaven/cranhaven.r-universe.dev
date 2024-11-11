# Track class for cytobands
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

# Only defining new drawing parameters defaults
setRefClass(
	Class = "track.bands",
	contains = "track.table",
	methods = list(
		
defaultParams = function(...) {
"Returns class-specific defaults for graphical parameters. Inheriting class should overload it to define their own defaults.
- ...   : may be used by inheriting methods, especially for inter-dependant parameters."
	
	params <- callSuper(...)
	
	params$drawFun <- "draw.boxes"
	params$maxElements <- 1000
	params$label <- TRUE
	params$labelStrand <- FALSE
	params$labelCex <- 0.75
	params$labelSrt <- 90
	params$labelAdj <- "center"
	params$labelOverflow <- FALSE
	params$fillColor <- function() {
		output <- slice$stain
		output[ output == "gneg" ] <- "#EEEEEE"
		output[ output == "gpos25" ] <- "#CCCCCC"
		output[ output == "gpos50" ] <- "#AAAAAA"
		output[ output == "gpos75" ] <- "#888888"
		output[ output == "gpos100" ] <- "#666666"
		output[ output == "acen" ] <- "#884444"
		output[ output == "gvar" ] <- "#448844"
		output[ output == "stalk" ] <- "#444488"
		return(output)
	}
	params$border <- "#000000"
	params$height <- "3 cm"
	params$xaxt <- "n"
	params$yaxt <- "n"
	params$ylim <- 0:1
	params$cex.lab <- 1
	
	return(params)
},

show = function(include=FALSE, fieldWidth=10) {
"Interactive printing
- include   : single logical value, if TRUE class name will not be printed."
	
	# Class name
	if(!isTRUE(include)) { cat("\n  \"track.bands\" reference class object\n")
	} else               { cat("\n  Extends \"track.bands\"\n")
	}
	
	# Inherited show()
	callSuper(include=TRUE, fieldWidth=fieldWidth)
}
		
	)
)

# Constructor
track.bands <- function(...) {
	# Inheritance
	object <- new("track.bands")
	object$import(track.table(...))
	
	return(object)
}

