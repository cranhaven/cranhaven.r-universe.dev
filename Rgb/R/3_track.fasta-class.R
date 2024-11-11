# Track class for Binary Alignment Map files (SAMtools)
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

# R5 sub-class definition
setRefClass(
	Class = "track.fasta",
	contains = c("sliceable"),
	fields = list(
		files = "data.frame",
		organism = "character",
		assembly = "character"
	),
	methods = list(

check = function(warn=TRUE) {
"Raises an error if the object is not valid, else returns TRUE"
	
	# drawable
	callSuper(warn)
	
	# Fields
	if(length(organism) != 1)         stop("'organism' must be a single character value")
	if(length(assembly) != 1)         stop("'assembly' must be a single character value")

	# File description
	if(nrow(files) == 0) stop("'files' must not be empty")
	if(ncol(files) != 6) stop("'files' must have 6 columns")
	if(colnames(files)[1] != "file" || !is.character(files[[1]]))      stop("'files' column #1 must be 'file' (character)")
	if(colnames(files)[2] != "header" || !is.character(files[[2]]))    stop("'files' column #2 must be 'header' (character)")
	if(colnames(files)[3] != "startOffset" || !is.numeric(files[[3]])) stop("'files' column #3 must be 'startOffset' (numeric)")
	if(colnames(files)[4] != "lineLength" || !is.integer(files[[4]]))  stop("'files' column #4 must be 'lineLength' (integer)")
	if(colnames(files)[5] != "breakSize" || !is.integer(files[[5]]))   stop("'files' column #5 must be 'breakSize' (integer)")
	if(colnames(files)[6] != "contentSize" || !is.integer(files[[6]])) stop("'files' column #6 must be 'contentSize' (integer)")
	for(i in 1:nrow(files)) if(!file.exists(files[i,"file"]))          stop("File #", i, " does not exist")
	
	# Warnings
	if(isTRUE(warn)) {
		if(is.na(organism)) warning("'organism' should not be NA")
		if(is.na(assembly)) warning("'assembly' should not be NA")
	}
	
	return(TRUE)
},

chromosomes = function() {
"Returns the chromosome list as a vector"
	
	return(row.names(files))
},

defaultParams = function(...) {
"Returns class-specific defaults for graphical parameters. Inheriting class should overload it to define their own defaults.
- ...   : may be used by inheriting methods, especially for inter-dependant parameters."
	
	params <- callSuper(...)
	
	params$drawFun <- "draw.seq"
	params$height <- lcm(2)
	params$yaxs <- "i"
	
	return(params)
},

getChromEnd = function(chrom) {
"Returns as a single integer value the ending position of the object description of the given chromosome. NA (integer) is valid if non relevant, but should be avoided when possible.
- chrom   : single integer, numeric or character value, the chromosomal location. NA is not required to be handled."
	
	return(files[ chrom , "contentSize" ])
},


initialize = function(files=data.frame(), organism=NA_character_, assembly=NA_character_, ...) {
	callSuper(...)
	initFields(files=files, organism=organism, assembly=assembly)
},

show = function(include=FALSE, fieldWidth=10) {
"Interactive printing
- include   : single logical value, if TRUE class name will not be printed."
	
	# Class name
	if(!isTRUE(include)) { cat("\n  \"track.fasta\" reference class object\n")
	} else               { cat("\n  Extends \"track.fasta\"\n")
	}
	
	# Fields
	cat(sprintf("  %-*s : %s\n", fieldWidth, "organism", organism[1]))
	cat(sprintf("  %-*s : %s\n", fieldWidth, "assembly", assembly[1]))
	if(nrow(files) > 3)        { cat(sprintf("  %-*s : %s ...\n", fieldWidth, "references", paste(head(rownames(files), 3), collapse=", ")))
	} else if(nrow(files) > 0) { cat(sprintf("  %-*s : %s\n", fieldWidth, "references", paste(rownames(files), collapse=", ")))
	} else                     { cat(sprintf("  %-*s : %s\n", fieldWidth, "references", "<no file>"))
	}
	
	# Inherited show()
	callSuper(include=TRUE, fieldWidth=fieldWidth)
},

slice = function(chrom, start, end, multiple=TRUE, maxRange=.self$getParam("maxRange"), ...) {
"Extracts elements in the specified window, in a format suitable to draw().
- chrom      : single integer, numeric or character value, the chromosomal location. NA is not handled.
- start      : single integer or numeric value, inferior boundary of the window. NA is not handled.
- end        : single integer or numeric value, superior boundary of the window. NA is not handled.
- multiple   : single logical value, whether to return the sequence as a vector of letters or as a single string.
- maxRange   : single integer value, no extraction will be attempted if end and start are more than this value away (returns NULL).
"
	# Checks
	if(!as.character(chrom) %in% rownames(files)) stop("'chrom' not found")
	if(!is.numeric(start) || length(start) != 1L || is.na(start) || start < 0) stop("'start' must be a single non-NA positive numeric")
	if(!is.numeric(end) || length(end) != 1L || is.na(end) || end < 0)         stop("'end' must be a single non-NA positive numeric")
	if(end < start)                                                            stop("'start' must be <= 'end'")
	
	# Coercion
	chrom <- as.character(chrom)
	start <- as.integer(start)
	end <- as.integer(end)
	
	if(end - start < maxRange) {
		# Select appropriate file
		lineLength <- files[chrom,"lineLength"]
		breakSize <- files[chrom,"breakSize"]
		startOffset <- files[chrom,"startOffset"]
		file <- files[chrom,"file"]
	
		# Count line breaks to be encountered
		startLine <- (start - 1L) %/% lineLength
		endLine <- (end - 1L) %/% lineLength
		breakCount <- endLine - startLine
	
		# Total characters to read
		elements <- end - start + 1L + breakCount*breakSize
	
		# Starting offset
		offset <- startOffset + (start - 1L) + startLine*breakSize
	
		# Extract characters as raw
		con <- file(file, open="rb")
		on.exit(close(con))
		seek(con, offset)
		rawData <- readBin(con, what="raw", n=elements)
	
		# Breaking character indexes
		if(breakCount > 0) {
			firstBreak <- lineLength - ((start - 1L) %% lineLength) + 1L
			firstBreak <- seq(from=firstBreak, to=elements, by=lineLength+breakSize)
			breakAt <- firstBreak
			if(breakSize > 1) for(i in 1:(breakSize-1L)) breakAt <- c(breakAt, firstBreak+i)
	
			# Drop breaking characters
			rawFiltered <- rawData[ -breakAt ]
		} else {
			# Nothing to remove
			rawFiltered <- rawData
		}
	
		# Convert to characters
		output <- rawToChar(rawFiltered, multiple=multiple)
	} else {
		# Too many bases to collect
		output <- NULL
	}
	
	return(output)
}

	)
)

# Constructor (single multi-FASTA file, indexed by faidx)
track.fasta.multi <- function(fastaFile, indexFile, .name, .organism, .assembly, .parameters, warn=TRUE) {
	# Check
	if(length(fastaFile) != 1) stop("Please provide a single multi-FASTA file")
	if(length(indexFile) != 1) stop("Please provide a single multi-FASTA index file")
	
	# Meta data
	object <- new("track.fasta")
	if(!missing(.organism))   object$organism <- .organism
	if(!missing(.assembly))   object$assembly <- .assembly
	if(!missing(.parameters)) object$parameters <- .parameters
	
	# Parse and reshape FASTA index
	index <- utils::read.table(indexFile, sep="\t", header=FALSE, col.names=c("NAME", "LENGTH", "OFFSET", "LINEBASES", "LINEWIDTH"), stringsAsFactors=FALSE)
	object$files <- data.frame(
		file = normalizePath(fastaFile),
		header = as.character(NA),
		startOffset = index$OFFSET,
		lineLength = index$LINEBASES,
		breakSize = index$LINEWIDTH - index$LINEBASES,
		contentSize = index$LENGTH,
		stringsAsFactors = FALSE,
		row.names = index$NAME
	)
	
	# Check
	object$check(warn=warn)
	
	return(object)
}

# Constructor (FASTA file colection)
track.fasta.collection <- function(files, chromosomes, .name, .organism, .assembly, .parameters, warn=TRUE) {
	# Check
	if(length(files) == 0) stop("Please provide 'files' to build a track upon")
	if(length(files) != length(chromosomes)) stop("Please provide chromosome names for each FASTA file")
	
	# Meta data
	object <- new("track.fasta")
	if(!missing(.organism))   object$organism <- .organism
	if(!missing(.assembly))   object$assembly <- .assembly
	if(!missing(.parameters)) object$parameters <- .parameters
	
	# Probe FASTA file formats
	tab <- NULL
	for(i in 1:length(files)) tab <- rbind(tab, data.frame(probeFasta(files[i]), row.names=chromosomes[i], stringsAsFactors=FALSE))
	object$files <- tab
	
	# Check
	object$check(warn=warn)
	
	return(object)
}

