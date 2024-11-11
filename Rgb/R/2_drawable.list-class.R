# Reference class for lists of 'drawable' objects
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

# R5 class definition
setRefClass(
	Class = "drawable.list",
	fields = list(
		files = "character",
		objects = "list",
		hidden = "logical",
		names = function(newValue) {
			if(missing(newValue)) {
				out <- character(0)
				if(.self$count > 0) for(i in 1:.self$count) out[i] <- .self$get(i)$name
				return(out)
			} else {
				stop("Read-only field")
			}
		},
		classes = function(newValue) {
			if(missing(newValue)) { return(sapply(objects, class))
			} else                { stop("Read-only field")
			}
		},
		count = function(newValue) {
			if(missing(newValue)) { return(length(objects))
			} else                { stop("Read-only field")
			}
		}
	),
	methods = list(

add = function(file, track=NULL, hidden=FALSE, ...) {
"Add a track to the list.
- file     : single character value, the path to the file containing the 'drawable' object to add.
- track    : a 'drawable' object to add. If NULL, will be extracted from 'file'.
- hidden   : single logical value, whether the track is to be shown on plots or hidden. This value can be changed later.
- ...      : further arguments to be passed to drawableFromFile.EXTENSION or drawableFromClass.CLASS, if relevant."
	
	# Checks
	if(length(file) != 1)   stop("'file' must refer to a single existing file")
	if(length(hidden) != 1) stop("'hidden' must be a single non NA logical value")
	
	# File parsing
	if(is.null(track)) {
		# File extension
		fileExtension <- tolower(sub("^.+\\.([^\\.]+)$", "\\1", file))
		
		# Look for a parsing function
		if(fileExtension == "rdt")        { track <- readRDT(file)
		} else if(fileExtension == "rds") { track <- readRDS(file)
		} else {
			# Custom parsing function
			fun <- sprintf("drawableFromFile.%s", fileExtension)
			if(exists(fun, mode="function")) {
				track <- base::get(fun, mode="function")(file=file, track=track, hidden=hidden, ...)
				file <- NA
			} else {
				stop("Unable to extract a 'drawable' object from a \".", fileExtension, "\" file")
			}
		}
	}
	
	# Class check
	if(!is(track, "drawable")) {
		# Custom converting function
		fun <- sprintf("drawableFromClass.%s", class(track)[1])
		if(exists(fun, mode="function")) {
			track <- base::get(fun, mode="function")(file=file, track=track, hidden=hidden, ...)
			if(!is(track, "drawable")) stop("\"", fun, "\" did not return a 'drawable' object")
			file <- NA
		} else {
			stop("Unable to convert a '", class(track)[1], "' object into a 'drawable' object")
		}
	}
	
	# Addition
	i <- .self$count + 1L
	files[i] <<- file
	objects[[i]] <<- track
	hidden[i] <<- hidden
},

check = function(warn=TRUE) {
"Raises an error if the object is not valid, else returns TRUE"
	
	# Fields
	if(length(objects) != length(files))  stop("'files' must contain as many values as 'objects'")
	if(length(objects) != length(hidden)) stop("'hidden' must contain as many values as 'objects'")
	
	# Objects
	for(object in objects) {
		if(!is(object, "drawable"))      stop("'objects' must be a list of 'drawable' objects")
		object$check(warn=warn)
	}
	
	# Warnings
	if(isTRUE(warn)) {
		if(any(is.na(files)))           warning("'files' should not contain NA values")
		if(any(is.na(hidden)))          warning("'hidden' should not contain NA values")
		if(any(!file.exists(files)))    warning("'files' should refer to existing files")
	}
	
	return(TRUE)
},

chromosomes = function(mode=c("union", "intersect")) {
"Returns the chromosome list of the tracks as a vector.
- mode   : single character value defining how to deal with distinct chromosome lists between tracks."
	
	# Check
	mode <- match.arg(mode)
	
	# Collect list
	if(.self$count == 0L) {
		# No drawable
		out <- character(0)
	} else if(mode == "union") {
		# Union
		out <- character(0)
		for(i in 1:.self$count) {
			out <- union(out, .self$get(i)$chromosomes())
		}
	} else {
		# Intersect
		out <- .self$get(1)$chromosomes()
		if(.self$count > 1L) for(i in 2:.self$count) {
			out <- intersect(out, .self$get(i)$chromosomes())
		}
	}
	
	return(out)
},

fix.files = function(parent=NULL) {
"Edit drawable list using a Tcl-tk GUI
- parent   : tcltk parent frame for inclusion, or NULL."
	
	tk.tracks(
		drawables = .self,
		parent = parent
	)
},

fix.param = function(selection=1L, parent=NULL) {
"Edit drawing parameters using a Tcl-tk GUI
- selection   : single integer value, the position of the track selected in the list.
- parent      : tcltk parent frame for inclusion, or NULL."
	
	tk.edit(
		drawables = .self,
		parent = parent,
		selection = selection
	)
},

get = function(index, what="objects") {
"Returns a single 'what' from the series
- index   : single numeric value, the position of the track to get.
- what    : single character value, the field to be exracted."
	
	if(!is.numeric(index) || length(index) != 1 || is.na(index)) stop("'index' must be a single non NA numeric value")
	return(.self[[what]][[index]])
},

getByClasses = function(classes, what="objects") {
"Returns a subset of 'what' from the series, querying by class inheritance
- classes   : character vector, the class names of the objects to get (inheriting classes are picked too).
- what      : single character value, the field to be exracted."
	
	selected <- logical(0)
	if(.self$count > 0) for(i in 1:.self$count) selected[i] <- any(sapply(X=classes, FUN=is, object=.self$get(i)))
	return(.self[[what]][ selected ])
},

getByNames = function(names, what="objects") {
"Returns a subset of 'what' from the series, querying by track name
- names   : character vector, the names of the objects to get.
- what    : single character value, the field to be exracted."
	
	return(.self[[what]][ .self$names %in% names ])
},

getByPositions = function(positions, what="objects") {
"Returns a subset of 'what' from the series, querying by position
- positions   : integer vector, the positions of the objects to get.
- what        : single character value, the field to be exracted."
	
	return(.self[[what]][ positions ])
},

getChromEnd = function(chrom) {
"Returns as a single integer value the maximal ending position of the object descriptions of the given chromosome.
- chrom   : single integer, numeric or character value, the chromosomal location."
	
	# Check
	if(.self$count == 0L) stop("Unable to predict chromosome end, there is no drawable in this drawable list.")
	
	# Collect track-level ends
	ends <- integer(.self$count)
	for(i in 1:.self$count) ends[i] <- .self$get(i)$getChromEnd(chrom)
	
	# Get maximum
	if(all(is.na(ends))) stop("Unable to predict chromosome end. Set 'end' manually or use at least one drawable object whose getChromEnd() method does not return NA.")
	end <- max(ends, na.rm=TRUE)
	
	return(end)
},

getLayout = function(check = FALSE, panel = NA, panelWidth = "5 cm", panelSide = "left") {
"Returns the layout elements
- check        : single logical value, whether to check if the current plot window is high enough to fit the plot or not.
- panel        : single logical value, whether to include a panel or not (NA means to plot one only if at least one track requires it).
- panelWidth   : single value handled by layout(), defining the width of the extra panel column if one is to be plotted.
- panelSide    : either 'left' or 'right', the side on which to plot the panels, if any.
"
	
	# Ignore hidden objects (showing hidden=NA)
	toProcess <- which(!sapply(.self$hidden, isTRUE))
	if(length(toProcess) > 0) {
		# Panel display
		if(is.na(panel)) {
			# Let tracks decide
			panel <- FALSE
			for(i in toProcess) panel <- panel || .self$get(i)$getParam("panel")
		} else {
			# Manually decided
			panel <- as.logical(panel)[1]
		}
		
		# Ignore new=TRUE
		toLay <- integer(0)
		for(i in toProcess) if(!.self$objects[[i]]$getParam("new")) toLay <- c(toLay, i)
		
		# Track heights
		trackHeights <- character(0)
		for(i in toLay) trackHeights <- c(trackHeights, .self$get(i)$getParam("height"))
		
		if(isTRUE(check)) {
			# Absolute tracks (from cm to inches)
			absolute <- grepl("^([0-9\\.]+) cm$", trackHeights)
			heights <- sum(as.double(gsub("^([0-9\\.]+) cm$", "\\1", trackHeights[absolute]))) / 2.54
			
			# Relative tracks (0.2 inches minimum)
			heights <- heights + sum(!absolute) * 0.2
			
			# Check
			if(heights > graphics::par("din")[2]) stop("Plot area too small")
		}
		
		layout <- list()
		if(panel) {
			if(panelSide == "left") {
				# Left panel
				layout$mat <- matrix(data=1:length(toLay), nrow=length(toLay), ncol=2)
				layout$mat[,2] <- layout$mat[,2] + max(layout$mat)
				layout$widths <- c(panelWidth, 1)
			} else if(panelSide == "right") {
				# Right panel
				layout$mat <- matrix(data=1:length(toLay), nrow=length(toLay), ncol=2)
				layout$mat[,1] <- layout$mat[,2] + max(layout$mat)
				layout$widths <- c(1, panelWidth)
			} else stop("Unhandled 'panelSide' value (left or right)")
		} else {
			# No panel
			layout$mat = matrix(data=1:length(toLay), ncol=1)
		}
		layout$heights <- trackHeights
		layout$panel <- panel
	}
	
	return(layout)
},

initialize = function(files=character(0), objects=list(), hidden=logical(0), ...) { # "classes" and "count" are wrappers, do not initialize !
	initFields(files=files, objects=objects, hidden=hidden)
},

moveDown = function(toMove) {
"Increases the position of a track, switching position with the next one
- toMove   : single numeric value, the position of the track to move."
	
	if(is.numeric(toMove) && length(toMove) == 1 && !is.na(toMove) && toMove < .self$count) {
		# New elements order
		newOrder <- append((1:.self$count)[ -toMove ], toMove, after=toMove)
	
		# Apply
		files <<- .self$files[ newOrder ]
		objects <<- .self$objects[ newOrder ]
		hidden <<- .self$hidden[ newOrder ]
		
		return(TRUE)
	} else {
		return(FALSE)
	}
},

moveUp = function(toMove) {
"Decreases the position of a track, switching position with the previous one
- toMove   : single numeric value, the position of the track to move."
	
	if(is.numeric(toMove) && length(toMove) == 1 && !is.na(toMove) && toMove > 1) {
		# New elements order
		newOrder <- append((1:.self$count)[ -toMove ], toMove, after=toMove-2)
	
		# Apply
		files <<- .self$files[ newOrder ]
		objects <<- .self$objects[ newOrder ]
		hidden <<- .self$hidden[ newOrder ]
		
		return(TRUE)
	} else {
		return(FALSE)
	}
},

remove = function(toRemove) {
"Remove one or many tracks from the list
- toRemove   : numeric vector, the positions of the tracks to remove."
	
	if(is.numeric(toRemove) && length(toRemove) > 0 && all(!is.na(toRemove))) {
		# Apply
		files <<- .self$files[ -toRemove ]
		objects <<- .self$objects[ -toRemove ]
		hidden <<- .self$hidden[ -toRemove ]
		
		return(TRUE)
	} else {
		return(FALSE)
	}
},

show = function(include=FALSE, fieldWidth=10) {
"Interactive printing
- include   : single logical value, if TRUE class name will not be printed."
	
	# Class name
	if(!isTRUE(include)) { cat("\n  \"drawable.list\" reference class object\n")
	} else               { cat("\n  Extends \"drawable.list\"\n")
	}
	
	# Content
	cat("\n")
	print(data.frame(name=.self$names, class=.self$classes))
}

	)
)

# Constructor
drawable.list <- function(files=character(0), objects=NULL, hidden=FALSE, warn=TRUE) {
	# Importation
	object <- new("drawable.list")
	
	# Addition
	if(is.null(objects)) {
		if(length(files) > 0) {
			# Parse 'files' (silently recycling 'hidden')
			if(length(hidden) != length(files)) hidden <- rep(hidden, length.out=length(files))
			for(i in 1:length(files)) object$add(file=files[i], hidden=hidden[i])
		}
	} else {
		if(!is.list(objects) || length(objects) != length(files)) stop("'objects' must be a list of the same length as 'files'")
		if(length(objects) > 0) {
			# Use arguments (silently recycling 'hidden')
			if(length(hidden) != length(files)) hidden <- rep(hidden, length.out=length(files))
			for(i in 1:length(files)) object$add(track=objects[[i]], file=files[i], hidden=hidden[i])
		}
	}
	
	# Check
	object$check(warn=warn)
	
	return(object)
}

