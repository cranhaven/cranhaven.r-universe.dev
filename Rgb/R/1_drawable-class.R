# Base level virtual class for genome browsing track
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

# Mainly a random object with a draw() virtual method
setRefClass(
	Class = "drawable",
	contains = "VIRTUAL",
	fields = list(
		name = "character",
		parameters = "list"		
	),
	methods = list(

callParams = function(chrom, start, end, ...) {
"Called with draw() arguments, it returns the final argument list handling default and overloaded parameters.
- chrom, start, end, ...   : arguments passed to draw()."
	
	# Begin with class-specific (overloaded) defaults (medium priority)
	argList <- defaultParams()
	
	# Update using object-specific values (high priority)
	for(p in names(parameters)) argList[[p]] <- parameters[[p]]
	
	# Update using call-specific values (highest priority)
	params <- list(...)
	if(length(params) > 0) {
		if(is.null(names(params)) || any(names(params) == "")) stop("Provide 'name=value' parameter pairs")
		for(paramName in names(params)) {
			argList[[ paramName ]] <- params[[ paramName ]]
		}
	}
	
	# Update coordinates using genome browsing commons
	argList$chrom <- chrom
	if(is.na(start)) { argList$start <- 0L
	} else           { argList$start <- start
	}
	if(is.na(end)) { argList$end <- getChromEnd(chrom)   ### Notice getChromEnd() can also return NA
	} else         { argList$end <- end
	}
	
	# Parameters in none of these steps will use drawing function defaults (lowest priority)
	
	return(argList)
},

check = function(warn=TRUE) {
"Raises an error if the object is not valid, else returns TRUE"
	
	# Fields
	if(length(name) != 1) stop("'name' must be a single character value")
	
	# Warnings
	if(isTRUE(warn)) {
		if(is.na(name))  warning("'name' should not be NA")
	}
	
	return(TRUE)
},

chromosomes = function() {
"[Virtual method]
Returns the chromosome list as a vector. NULL is valid if non relevant, but should be avoided when possible."
	
	return(NULL)
},

defaultParams = function(...) {
"Returns class-specific defaults for graphical parameters. Inheriting class should overload it to define their own defaults.
- ...   : may be used by inheriting methods, especially for inter-dependant parameters."
	
	# No mother class
	params <- list()
	
	params$height <- 1
	params$mar <- c(0.2, 5, 0.2, 1)
	params$new <- FALSE
	params$panel <- FALSE
	
	return(params)
},

draw = function(chrom, start=NA, end=NA, ...) {
"[Virtual method]
Draws the object content corresponding to the defined genomic window, usually in a single plot area with coordinates in x and custom data in y.
Overloading methods should use .self$callParams(chrom, start, end ...) to handle drawing parameters and NA coordinates in a consistent way.
- chrom   : single integer, numeric or character value, the chromosomal location.
- start   : single integer or numeric value, inferior boundary of the window. NA should refer to 0.
- end     : single integer or numeric value, superior boundary of the window. NA should refer to .self$getChromEnd().
- ...     : additionnal drawing parameters (precede but do not overwrite parameters stored in the object)."
	
	stop("draw() is a virtual method, it must be overloaded by inheriting classes")
},

drawPanel = function(chrom=NA, start=NA, end=NA, ...) {
"Draws a genome-level summary of the content, to be optionnally displayed as a panel on the left of draw() output. Default draws a blank plot.
Overloading methods should consider .self$callParams(chrom, start, end ...) to handle drawing parameters and NA coordinates in a consistent way.
- chrom   : single integer, numeric or character value, the chromosomal location.
- start   : single integer or numeric value, inferior boundary of the window. NA should refer to 0.
- end     : single integer or numeric value, superior boundary of the window. NA should refer to .self$getChromEnd().
- ...     : additionnal drawing parameters (precede but do not overwrite parameters stored in the object)."
	
	par(mar=c(0,0,0,0))
	plot(x=NA, y=NA, xlim=0:1, ylim=0:1, xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
},

fix.param = function(parent=NULL) {
"Edit drawing parameters using a Tcl-tk GUI
- parent   : tcltk parent frame for inclusion, or NULL."
	
	# Generalize to drawable.list for tk.edit()
	drawables <- drawable.list(files=as.character(NA), objects=list(.self), warn=FALSE)
	
	# Call function
	tk.edit(
		drawables = drawables,
		parent = parent
	)
},

getChromEnd = function(chrom) {
"[Virtual method]
Returns as a single integer value the ending position of the object description of the given chromosome. NA (integer) is valid if non relevant, but should be avoided when possible.
- chrom   : single integer, numeric or character value, the chromosomal location. NA is not required to be handled."
	
	return(as.integer(NA))
},

getName = function() {
"'name' field accessor."
	
	return(name)
},

getParam = function(name, ...) {
"Returns the parameter stored, or the default value if no custom value is stored for it.
- name   : single character value, the name of the parameter to return.
- ...    : to be passed to defaultParams(), especially for inter-dependant parameters."
	
	if(name %in% names(parameters))  { return(parameters[[name]])
	} else {
		# If not set in the object, search for class-specific default
		params <- defaultParams(...)
		if(name %in% names(params)) { return(params[[name]])
		} else                      { stop("No default value was found for graphical parameter \"", name, "\"")
		}
	}
},

initialize = function(name=NA_character_,parameters=list(), ...) {
	initFields(name=name, parameters=parameters)
},

setName = function(value) {
"'name' field mutator."
	
	name <<- value
},

setParam = function(name, value) {
"Updates a parameter stored in the object.
- name    : single character value, the name of the parameter to set.
- value   : the new value to assign to the parameter (any type). If missing the parameter is discarded, thus returning to dynamic default value."
	
	# New value list
	if(missing(value))        { parameters[[ name ]] <<- NULL
	} else if(is.null(value)) { stop("'NULL' is no longer a valid drawing parameter")
	} else                    { parameters[[ name ]] <<- value
	}
},

show = function(include=FALSE, fieldWidth=10) {
"Interactive printing
- include   : single logical value, if TRUE class name will not be printed."
	
	# Class name
	if(!isTRUE(include)) { stop("'drawable' is a virtual class")
	} else               { cat("\n  Extends \"drawable\"\n")
	}
	
	# Fields
	cat(sprintf("  %-*s : %s\n", fieldWidth, "name", name[1]))
}
		
	)
)

