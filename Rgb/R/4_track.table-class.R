# Generic reference class for genomic track, containing elements genomically located (chrom:start-end)
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

# A self sufficient table-shaped track element (needs to contains 'chrom', 'start' and 'end')
setRefClass(
	Class = "track.table",
	contains = c("refTable", "crossable"),
	fields = list(
		index = "integer",
		checktrack = "call",
		sizetrack = "call",
		subtrack = "call",
		organism = "character",
		assembly = "character"
	),
	methods = list(

addArms = function(centromeres, temp=FALSE) {
"Adds an arm localization ('p' or 'q') to the 'chrom' column.
- centromeres   : named numeric vector, providing the centromere position of each chromosome. Can also be a band track, as returned by track.UCSC_bands().
- temp          : single logical value, whether to alter the object or return an altered copy."
	
	if(!isArmed()) {
		# Work in a copy
		if(isTRUE(temp)) object <- .self$copy(shallow=FALSE)
		else             object <- .self
	
		# From band track
		if(is(centromeres, "track.table") && "stain" %in% centromeres$colNames) {
			bands <- centromeres$eraseArms(temp=TRUE)$extract(expression(stain == "acen"))
			centromeres <- tapply(bands$start, bands$chrom, max)
		}
		
		if(is.numeric(centromeres) && all(object$chromosomes() %in% names(centromeres))) {
			# Localized elements
			valid <- which(!is.na(object$extract(, "chrom")) & !is.na(object$extract(, "start")) & !is.na(object$extract(, "end")))
			
			# Compute arm location
			arm_valid <- rep("", length(valid))
			arm_valid[ object$extract(valid, "end") <= centromeres[ as.character(object$extract(valid, "chrom")) ] ] <- "p"
			arm_valid[ object$extract(valid, "start") >= centromeres[ as.character(object$extract(valid, "chrom")) ] ] <- "q"
			if(any(arm_valid == "")) stop("Can't split a feature located on both arms of the same chromosome")
			
			# Add arms to chromosome names
			levels <- paste(rep(object$chromosomes(), each=2), rep(c("p","q"), times=length(object$chromosomes())), sep="")
			object$coerce("chrom", "character")
			object$fill(valid, "chrom", paste(object$extract(valid, "chrom"), arm_valid, sep=""))
			object$coerce("chrom", "factor", levels=levels)
		} else {
			stop("'centromeres' must be a named numeric vector with an element for each chromosome")
		}
	}
	
	# Return
	if(isTRUE(temp)) return(object)
	else             invisible(TRUE)
},

addColumn = function(content, name, after=colCount) {
"Overload to keep indexes up-to-date"
	callSuper(content=content, name=name, after=after)
	if(name == "chrom") buildIndex()
	buildCalls()
},

addList = function(...) {
"Overload to keep rows ordered by position"
	callSuper(...)
	rowOrder(c("chrom", "start"), na.last=TRUE, decreasing=FALSE)
},

buildCalls = function() {
"Updates 'checktrack' and 'subtrack' fields. To be performed after each modification of colNames and colReferences (concerned methods are overloaded to enforce this)."
	
	columns <- paste(sprintf("\"%s\"=`%s`", colNames, colReferences), collapse=", ")
	checktrack <<- parse(text=sprintf(".External(\"checktrack\", PACKAGE=\"Rgb\", %s)", columns))[[1]]
	sizetrack <<- parse(text=sprintf(".External(\"track\", PACKAGE=\"Rgb\", mode='size', %s, %s, %s, %s, %s)", NA, NA, NA, NA, columns))[[1]]
	subtrack <<- parse(text=sprintf(".External(\"track\", PACKAGE=\"Rgb\", mode='sub', %s, %s, %s, %s, %s)", NA, NA, NA, NA, columns))[[1]]
},

buildGroupPosition = function(groupBy, colName="groupPosition", reverse=TRUE) {
"Adds a column to be used as 'groupPosition' by draw.boxes()
- groupBy   : single character value, the name of a column to group rows on.
- colName   : single character value, the name of the column to buid.
- reverse   : single logical value, whether to reverse numbering on reverse strand or not."
	
	# Group / row index dictionnary (filter out unused factor levels)
	indexes <- tapply(X=1:.self$rowCount, INDEX=.self$extract(,groupBy), FUN=c)
	indexes <- indexes[ sapply(indexes, length) > 0L ]
	
	# Parallel numbering
	numbers <- lapply(sapply(indexes, length), seq.int, from=1, by=1)
	
	if(isTRUE(reverse)) {
		# Group / strand dictionnary
		firstRows <- sapply(indexes, "[", 1L)
		strands <- .self$extract(firstRows, "strand")
		names(strands) <- names(firstRows)
		
		# Reverse numbering on reverse strand
		toReverse <- !is.na(strands[ names(numbers) ]) & strands[ names(numbers) ] == "-"
		numbers <- c(
			lapply(numbers[ toReverse ], rev),
			numbers[ !toReverse ]
		)
		indexes <- c(
			indexes[ toReverse ],
			indexes[ !toReverse ]
		)
	}
	
	# Fill output vector
	out <- rep(as.integer(NA), .self$rowCount)
	out[ unlist(indexes) ] <- unlist(numbers)
	
	# Add to table
	.self$addColumn(out, name=colName)
},

buildGroupSize = function(groupBy, colName="groupSize") {
"Adds a column to be used as 'groupSize' by draw.boxes()
- groupBy   : single character value, the name of a column to group rows on.
- colName   : single character value, the name of the column to buid."
	
	# Group / size dictionnary
	count <- table(.self$extract(,groupBy))
	
	# Get corresponding values
	out <- as.integer(count[ .self$extract(,groupBy) ])
	
	# Add to table
	.self$addColumn(out, name=colName)
},

buildIndex = function() {
"Updates the 'index' parameter, should be done after any change made on the 'chrom' column (concerned methods are overloaded to enforce this)."
	
	if(rowCount > 0) {
		tmp <- as.matrix(tapply(1:rowCount, extract(,"chrom"), max))
		storage.mode(tmp) <- "integer"
		index <<- tmp[,1]
	} else {
		# Empty table, but chromosomes may be defined
		index <<- rep(as.integer(NA), length(chromosomes()))
	}
},

check = function(warn=TRUE) {
"Raises an error if the object is not valid, else returns TRUE"
	
	# callSuper() FIX
	refTable.check <- getClass("refTable")@refMethods$check
	drawable.check <- getClass("drawable")@refMethods$check
	environment(refTable.check) <- environment(drawable.check) <- as.environment(.self)
	refTable.check(warn=warn)
	drawable.check(warn=warn)
	
	# Columns
	if(!"name" %in% .self$getColNames())                           stop("Must contain a 'name' column")
	if(!"chrom" %in% .self$getColNames())                          stop("Must contain a 'chrom' column")
	if(!"strand" %in% .self$getColNames())                         stop("Must contain a 'strand' column")
	if(!"start" %in% .self$getColNames())                          stop("Must contain a 'start' column")
	if(!"end" %in% .self$getColNames())                            stop("Must contain a 'end' column")
	if(.self$types("name") != "character")                         stop("'name' column must be of class 'character'")
	if(.self$types("chrom") != "factor")                           stop("'chrom' column must be of class 'factor'")
	if(.self$types("strand") != "factor")                          stop("'strand' column must be of class 'factor'")
	if(.self$types("start") != "integer")                          stop("'start' column must be of class 'integer'")
	if(.self$types("end") != "integer")                            stop("'end' column must be of class 'integer'")
	if(!identical(levels(.self$extract(0L,"strand")), c("-","+"))) stop("'strand' column levels must be '-' and '+' (in this order)")
	
	# Fields
	if(length(organism) != 1) stop("'organism' must be a single character value")
	if(length(assembly) != 1) stop("'assembly' must be a single character value")
	
	# checktrack C call
	eval(checktrack, values)
	
	# Index check
	if(length(index) != length(chromosomes()))                                      stop("'index' length does not match 'chrom' column levels count")
	if(any(extract(index[!is.na(index)], "chrom") != chromosomes()[!is.na(index)])) stop("'index' rows 'chrom' do not correspond to expected values")
	
	# Warnings
	if(isTRUE(warn)) {
		if(is.na(organism)) warning("'organism' should not be NA")
		if(is.na(assembly)) warning("'assembly' should not be NA")
	}
	
	return(TRUE)
},

chromosomes = function() {
"Returns the chromosome list as a vector"
	
	return(getLevels("chrom"))
},

coerce = function(j=NULL, class, levels, ...) {
"Overload to keep indexes up-to-date"
	j <- indexes(j, "column")
	callSuper(j=j, class=class, levels=levels, ...)
	if(colNames[j] == "chrom") buildIndex()
},

defaultParams = function(...) {
"Returns class-specific defaults for graphical parameters. Inheriting class should overload it to define their own defaults.
- ...   : may be used by inheriting methods, especially for inter-dependant parameters."
	
	params <- callSuper(...)
	
	params$ylab <- .self$name
	params$ysub <- .self$assembly
	
	return(params)
},

delColumns = function(...) {
"Overload to keep calls up-to-date"
	callSuper(...)
	buildCalls()
},

eraseArms = function(temp=FALSE) {
"Removes 'p' and 'q' added by the addArms() method from the 'chrom' column.
- temp   : single logical value, whether to alter the object or return an altered copy."
	
	# Work in a copy
	if(isTRUE(temp)) object <- .self$copy(shallow=FALSE)
	else             object <- .self
	
	if(object$isArmed()) {
		levels <- sub("p$", "", grep("p$", object$chromosomes(), value=TRUE))
		object$coerce("chrom", "character")
		object$fill(, "chrom", sub("[pq]$", "", object$extract(, "chrom")))
		object$coerce("chrom", "factor", levels=levels)
	}
	
	# Return
	if(isTRUE(temp)) return(object)
	else             invisible(TRUE)
},

fill = function(i=NULL, j=NULL, newValues) {
"Overload to keep indexes up-to-date"
	j <- indexes(j, "column")
	callSuper(i=i, j=j, newValues=newValues)
	if(colNames[j] == "chrom") buildIndex()
},

getChromEnd = function(chrom) {
"Returns as a single integer value the ending position of the object description of the given chromosome. NA (integer) is valid if non relevant, but should be avoided when possible.
- chrom   : single integer, numeric or character value, the chromosomal location. NA is not required to be handled."
	
	if(chrom %in% names(index) && !is.na(index[chrom])) return(extract(index[chrom], "end"))
	else                                                return(as.integer(NA))
},

initialize = function(index=integer(0), checktrack=new("call"), subtrack=new("call"), organism=NA_character_, assembly=NA_character_, ...) {
	# callSuper() FIX
	refTable.initialize <- getClass("refTable")@refMethods$initialize
	drawable.initialize <- getClass("drawable")@refMethods$initialize
	environment(refTable.initialize) <- environment(drawable.initialize) <- as.environment(.self)
	refTable.initialize(...)
	drawable.initialize(...)
	
	initFields(index=index, checktrack=checktrack, subtrack=subtrack, organism=organism, assembly=assembly)
},

isArmed = function() {
"Detects whether the 'chrom' column refers to whole chromosomes or chromosome arms."
	
	# 'p' or 'q' in all chromosome names
	if(any(!grepl("[pq]$", .self$chromosomes()))) return(FALSE)
	
	# All chromosomes are duplicated
	x <- sub("[pq]$", "", .self$chromosomes())
	return(all(x[c(TRUE, FALSE)] == x[c(FALSE, TRUE)]))
},

metaFields = function() {
"Returns a character vector of fields that do not directly depend on the tabular content, for clonage."
	
	out <- callSuper()
	out <- setdiff(out, c("checktrack", "subtrack"))
	return(out)
},

rowOrder = function(...) {
"Overload to keep indexes up-to-date"
	callSuper(...)
	buildIndex()
},

segMerge = function(...) {
"Apply the segMerge() function to the track content.
- ...   : arguments to be passed to segMerge()."
	
	# Processing
	newRegions <- Rgb::segMerge(
		segTable = .self$extract(),
		...
	)
	
	# Region field replacing
	.self$erase()
	.self$addDataFrame(newRegions)
	
	# Check
	.self$check()
},

segOverlap = function(...) {
"Apply the segOverlap() function to the track content.
- ...   : arguments to be passed to segOverlap()."
	
	# Processing
	newRegions <- segOverlap(
		segTable = .self$extract(),
		...
	)
	
	# Region field replacing
	.self$erase()
	.self$addDataFrame(newRegions)
	
	# Check
	.self$check()
},

setColNames = function(j, value) {
"Replaces one or many column names (overload to keep calls and ordering up-to-date).
- j       : subset of columns to rename.
- value   : new column names to use, as a character vector."

	callSuper(j=j, value=value)
	if("chrom" %in% c(j, value) || "start" %in% c(j, value)) rowOrder(c("chrom", "start"), na.last=TRUE, decreasing=FALSE)
	buildCalls()
},

setLevels = function(j=NULL, newLevels) {
"Overload to keep indexes up-to-date"

	j <- indexes(j, "column")
	callSuper(j=j, newLevels=newLevels)
	if(colNames[j] == "chrom") buildIndex()
},

show = function(include=FALSE, fieldWidth=10) {
"Interactive printing
- include   : single logical value, if TRUE class name will not be printed."
	
	# Class name
	if(!isTRUE(include)) {
		cat("\n  \"track.table\" reference class object\n")
	} else {
		cat("\n  Extends \"track.table\"\n")
	}
	
	# Fields
	cat(sprintf("  %-*s : %s\n", fieldWidth, "organism", organism[1]))
	cat(sprintf("  %-*s : %s\n", fieldWidth, "assembly", assembly[1]))
	
	# callSuper() FIX
	drawable.show <- getClass("drawable")@refMethods$show
	refTable.show <- getClass("refTable")@refMethods$show
	environment(drawable.show) <- environment(refTable.show) <- as.environment(.self)
	drawable.show(include=TRUE, fieldWidth=fieldWidth)
	refTable.show(include=TRUE, fieldWidth=fieldWidth)
},

size = function(chrom, start, end) {
"Count elements in the specified window.
- chrom   : single integer, numeric or character value, the chromosomal location.
- start   : single integer or numeric value, inferior boundary of the window.
- end     : single integer or numeric value, superior boundary of the window."
	
	if(is.numeric(chrom)) chrom <- as.integer(chrom)
	if(is.numeric(start)) start <- as.integer(start)
	if(is.numeric(end))   end <- as.integer(end)
	
	expr <- sizetrack
	expr[[5]] <- chrom
	expr[[6]] <- start
	expr[[7]] <- end
	expr[[8]] <- index
	
	results <- eval(expr, values)
	
	return(results)
},

slice = function(chrom, start, end, asObject=FALSE) {
"Extract elements in the specified window as a data.frame.
- chrom      : single integer, numeric or character value, the chromosomal location.
- start      : single integer or numeric value, inferior boundary of the window.
- end        : single integer or numeric value, superior boundary of the window.
- asObject   : if TRUE results will be served in the same class as the current object."
	
	if(is.numeric(chrom)) chrom <- as.integer(chrom)
	if(is.numeric(start)) start <- as.integer(start)
	if(is.numeric(end))   end <- as.integer(end)
	
	expr <- subtrack
	expr[[5]] <- chrom
	expr[[6]] <- start
	expr[[7]] <- end
	expr[[8]] <- index
	
	results <- eval(expr, values)
	
	if(isTRUE(asObject)) {
		# New object with same meta-data
		output <- new(class(.self))
		for(fieldName in .self$metaFields()) output[[fieldName]] <- .self$field(fieldName)
		output$addDataFrame(results)
		results <- output
	}
	
	return(results)
}

	)
)

# Constructor
track.table <- function(..., .name, .parameters, .organism, .assembly, .chromosomes, .makeNames=FALSE, .orderCols=TRUE, warn=TRUE) {
	# Inheritance
	object <- new("track.table")
	if(!missing(.parameters)) object$parameters <- .parameters
	if(!missing(.organism))   object$organism <- .organism
	if(!missing(.assembly))   object$assembly <- .assembly
	if(!missing(.name))       object$name <- .name              ### 'drawable' field actually, but must not be part of "..." or refTable() will crash
	object$import(refTable(..., warn=FALSE))
	
	# Slicing calls computation
	object$buildCalls()
	
	# Make unique names
	if(isTRUE(.makeNames)) {
		# Name computation
		if(! "chrom" %in% object$getColNames()) stop("'chrom' column required")
		newNames <- make.unique(as.character(object$extract(,"chrom")), sep=".")
		newNames[ !grepl("\\.[0-9]+$", newNames) ] <- sprintf("%s.0", newNames[ !grepl("\\.[0-9]+$", newNames) ])
		newNames <- sprintf("chr%s", newNames)
		
		# Add to object
		if("name" %in% object$getColNames()) {
			object$fill(, "name", newNames)
			if(isTRUE(warn)) warning("'name' column replaced, as .makeNames=TRUE")
		} else {
			object$addColumn(newNames, "name")
		}
	}
	
	# Strand factor
	if(! "strand" %in% object$getColNames()) stop("'strand' column required")
	if(!is.factor(object$extract(0L,"strand")) || !identical(levels(object$extract(0L,"strand")), c("-","+"))) {
		if(all(object$extract(,"strand") %in% c(NA,"+","-"))) {
			object$coerce("strand", "factor", levels=c("-","+"))
			if(isTRUE(warn)) warning("'strand' column converted to factor")
		} else {
			stop("'strand' column can only contain '+', '-' or NA")
		}
	}
	
	# Chromosome factor
	if(! "chrom" %in% object$getColNames()) stop("'chrom' column required")
	if(!is.factor(object$extract(0L,"chrom"))) {
		if(missing(.chromosomes)) {
			object$coerce("chrom", "factor")
		} else {
			if(isTRUE(warn) && any(! object$extract(,"chrom") %in% .chromosomes)) warning("Some 'chrom' values are not in .chromosomes (turned into NA)")
			object$coerce("chrom", "factor", levels=.chromosomes)
		}
		if(isTRUE(warn)) warning("'chrom' column converted to factor")
	}
	
	# Column ordering
	if(isTRUE(.orderCols)) {
		annotColumns <- c("name", "chrom", "strand", "start", "end")
		if(any(! annotColumns %in% object$getColNames())) stop("Columns required : ", paste(annotColumns, collapse=", "))
		newOrder <- c(annotColumns, setdiff(object$getColNames(), annotColumns))
		object$colOrder(newOrder)
	}
	
	# Row ordering
	if(any(! c("chrom", "start") %in% object$getColNames())) stop("'chrom' and 'start' columns required")
	object$rowOrder(c("chrom", "start"), na.last=TRUE, decreasing=FALSE)
	
	# 'chrom' indexing
	object$buildIndex()
	
	# Check
	object$check(warn=warn)
	
	return(object)
}
