# Virtual reference class for objects that can be genomically sliced (extraction in chrom:start-end coordinates)
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

# A random object with a slice() method, and thus a draw() method based on it (virtual)
setRefClass(
	Class = "sliceable",
	contains = c("drawable", "VIRTUAL"),
	methods = list(

defaultParams = function(drawFun=FALSE, ...) {
"Returns class-specific defaults for graphical parameters. Inheriting class should overload it to define their own defaults.
- drawFun   : single logical value, if TRUE only 'drawFun' default is returned, else it will not be returned (special handling).
- ...       : may be used by inheriting methods, especially for inter-dependant parameters."
	
	params <- callSuper(...)
	params$drawFun <- "draw.boxes"
	
	if(!isTRUE(drawFun)) {
		# Get the list of used drawing functions
		functions <- c("draw.bg", getParam("drawFun", drawFun=TRUE))
		
		# Get default values from 'drawFun' formal arguments
		for(fun in functions) {
			form <- formals(fun)
			form <- form[ setdiff(names(form), c("slice", "chrom", "start", "end", "...")) ]
			for(fname in names(form)) params[[ fname ]] <- eval(form[[ fname ]])
		}
	}
	
	return(params)
},

draw = function(chrom, start=NA, end=NA, ...) {
"Draws the object content corresponding to the defined genomic window, usually in a single plot area with coordinates in x and custom data in y.
- chrom   : single integer, numeric or character value, the chromosomal location. NA is not required to be handled.
- start   : single integer or numeric value, inferior boundary of the window. NA is not required to be handled.
- end     : single integer or numeric value, superior boundary of the window. NA is not required to be handled.
- ...     : additionnal drawing parameters (precede but do not overwrite parameters stored in the object)."
	
	# Aggregate and prioritize drawing parameters from classes, objects and draw call
	argList <- callParams(chrom, start, end, ...)
	
	# Enforce arguments required by all 'drawFun' in sliceable (priority = 4)
	argList$slice <- slice(argList$chrom, argList$start, argList$end)
	
	for(fun in argList$drawFun) {
		# Drawing function
		if(!exists(fun))                             stop("'drawFun' must be an existing object name")
		drawFunction <- get(fun)
		if(!is.function(drawFunction))               stop("'drawFun' must refer to a function")
		if(!"..." %in% names(formals(drawFunction))) stop("'drawFun' must have a '...' argument")
	
		# Call
		do.call(
			what = drawFunction,
			args = argList
		)
	}
},

getChromEnd = function(chrom) {
"Returns as a single integer value the ending position of the object description of the given chromosome. NA (integer) is valid if non relevant, but should be avoided when possible.
- chrom   : single integer, numeric or character value, the chromosomal location. NA is not required to be handled."
	
	# Dirty but fonctionnal, overload if possible
	wholeChrom <- slice(chrom, 0L, .Machine$integer.max)
	if(nrow(wholeChrom) > 0) return(tail(wholeChrom, 1L)$end)
	else                     return(as.integer(NA))
},

show = function(include=FALSE, fieldWidth=10) {
"Interactive printing
- include   : single logical value, if TRUE class name will not be printed."
	
	# Class name
	if(!isTRUE(include)) { stop("'sliceable' is a virtual class")
	} else               { cat("\n  Extends \"sliceable\"\n")
	}
},

slice = function(chrom, start, end) {
"[Virtual method]
Extract elements in the specified window, in a format suitable to draw().
- chrom   : single integer, numeric or character value, the chromosomal location. NA is not handled.
- start   : single integer or numeric value, inferior boundary of the window. NA is not handled.
- end     : single integer or numeric value, superior boundary of the window. NA is not handled."
	
	stop("slice() is a virtual method, it must be overloaded by inheriting classes")
}

	)
)
