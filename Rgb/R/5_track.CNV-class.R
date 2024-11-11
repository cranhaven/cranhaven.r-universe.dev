# Track class for Copy Number Variations
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

# Only defining new drawing parameters defaults
setRefClass(
	Class = "track.CNV",
	contains = "track.table",
	methods = list(
		
defaultParams = function(...) {
"Returns class-specific defaults for graphical parameters. Inheriting class should overload it to define their own defaults.
- ...   : may be used by inheriting methods, especially for inter-dependant parameters."
	
	params <- callSuper(...)
	
	params$drawFun <- "draw.boxes"
	params$maxElements <- 2000
	params$label <- FALSE
	params$labelStrand <- FALSE
	params$labelCex <- 0.75
	params$labelSrt <- 0
	params$labelAdj <- "left"
	params$fillColor <- function() {
		colors <- c(
			"loss"                     = "#880000",
			"loss mobile element"      = "#880000",
			"deletion"                 = "#FF8888",
			"duplication"              = "#8888FF",
			"gain"                     = "#8888FF",
			"tandem duplication"       = "#8888FF",
			"amplification"            = "#000088",
			"gain+loss"                = "#BB7744",
			"novel sequence insertion" = "#88FF88",
			"mobile element insertion" = "#88FF88",
			"insertion"                = "#88FF88"		
		)
		output <- colors[ as.character(slice$type) ]
		output[ is.na(output) ] <- "#888888"
		return(output)
	}
	params$border <- "fillColor"
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
	if(!isTRUE(include)) { cat("\n  \"track.CNV\" reference class object\n")
	} else               { cat("\n  Extends \"track.CNV\"\n")
	}
	
	# Inherited show()
	callSuper(include=TRUE, fieldWidth=fieldWidth)
}
		
	)
)

# Constructor
track.CNV <- function(...) {
	# Inheritance
	object <- new("track.CNV")
	object$import(track.table(...))
	
	return(object)
}

