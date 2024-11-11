# Track class for Binary Alignment Map files (SAMtools)
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

# R5 sub-class definition
setRefClass(
	Class = "track.bam",
	contains = c("sliceable"),
	fields = list(
		bamPath = "character",
		baiPath = "character",
		header = "data.frame",
		index = "list",
		organism = "character",
		assembly = "character",
		addChr = "logical",
		compression = "numeric"
	),
	methods = list(

check = function(warn=TRUE) {
"Raises an error if the object is not valid, else returns TRUE"
	
	# drawable
	callSuper(warn)
	
	# Fields
	if(!file.exists(bamPath))                    stop("'bamPath' file does not exist")
	if(!is.na(baiPath) && !file.exists(baiPath)) stop("'baiPath' file does not exist")
	if(length(organism) != 1)                    stop("'organism' must be a single character value")
	if(length(assembly) != 1)                    stop("'assembly' must be a single character value")
	if(length(addChr) != 1)                      stop("'addChr' must be a single logical value")
	if(is.na(addChr))                            stop("'addChr' cannot be NA")
	if(length(compression) != 1)                 stop("'compression' must be a single numeric value")
	if(is.na(compression))                       stop("'compression' cannot be NA")
	if(!"SN" %in% colnames(header))              stop("'header' must contain at least a 'SN' column")
	if(length(index) != nrow(header))            stop("'index' must describe as many reference sequences as 'header'")
	
	# BAM magic number
	con <- gzfile(bamPath, open="rb")
	magic <- readBin(con, what="raw", n=4)
	close(con)
	if(!identical(magic, charToRaw("BAM\1"))) return("'bamPath' does not refer to a valid BAM file (wrong magic number)")
	
	# BAI magic number
	if(!is.na(baiPath)) {
		con <- gzfile(baiPath, open="rb")
		magic <- readBin(con, what="raw", n=4)
		close(con)
		if(!identical(magic, charToRaw("BAI\1"))) return("'baiPath' does not refer to a valid BAI file (wrong magic number)")
	}
	
	# Warnings
	if(isTRUE(warn)) {
		if(is.na(organism))   warning("'organism' should not be NA")
		if(is.na(assembly))   warning("'assembly' should not be NA")
		if(nrow(header) == 0) warning("'header' is empty (unaligned BAM file ?)")
		if(is.na(baiPath))    warning("'baiPath' is NA (unaligned BAM file ?)")
	}
	
	return(TRUE)
},

chromosomes = function() {
"Returns the chromosome list as a vector"
	
	if(addChr) { return(sub("^chr", "", header$SN))
	} else     { return(header$SN)
	}
},

coverage = function(chrom, start=NA, end=NA, tracks=TRUE, binLevel=5L, rawSize=FALSE) {
"Fast estimation of depth coverage in a genomic window, from indexing data. Values are normalized into [0:1] over the genomic window.
- chrom      : single integer, numeric or character value, the chromosomal location.
- start      : single integer or numeric value, inferior boundary of the window. If NA, the whole chromosome is considered.
- end        : single integer or numeric value, superior boundary of the window. If NA, the whole chromosome is considered.
- tracks     : single logical value, whether to return a data.frame or a track.table.
- binLevel   : single integer value, the higher bin order to allow
               0 = 537Mb, 1 = 67Mb, 2 = 8Mb, 3 = 1Mb, 4 = 130kb, 5 = 16kb
               incrementing this value enhances boundary precision but discards reads located at bin junctions.
- rawSize    : single logical value, whether to output raw size or normalize by the maximum encountered."
	
	# Coordinate check
	if(is.numeric(start)) start <- as.integer(start)
	if(is.numeric(end))   end <- as.integer(end)
	
	# Translate chrom names into indexes
	if(length(chrom) != 1L) stop("'chrom' must refer to a single chromosome")
	if(isTRUE(addChr)) chrom <- sprintf("chr%s", chrom)
	chromIndex <- match(chrom, header$SN)
	if(is.na(chromIndex)) stop("'chrom' not found in BAM header")
	
	if(is.na(start) || is.na(end)) {
		# All registered bins on the chromosome (except pseudo-bins)
		bins <- as.integer(names(index[[ chromIndex ]]$bins))
		bins <- setdiff(bins, 37450L)
	} else {
		# All bins overlapping the region (whatever the chromosome)
		if(start > end) stop("'start' must be lesser or equal to 'end'")
		bins <- coord2bins(start=start, end=end)
	}
	
	# Bin level filtering
	if(binLevel > 0L) {
		blockStarts <- as.integer(c(0, 1, 9, 73, 585, 4681))
		blockEnds <- as.integer(c(0, 8, 72, 584, 4680, 37449))
		for(i in 1:binLevel) bins <- setdiff(bins, blockStarts[i]:blockEnds[i])
	}
	
	if(length(bins) > 0L) {
		# Extract non-NA chunks from these bins
		chunkList <- index[[ chromIndex ]]$bins[ as.character(bins) ]
		chunkList <- chunkList[ !is.na(names(chunkList)) ]
		chunks <- do.call(rbind, args=chunkList)
		
		# Translate into real coordinates
		coffsets <- chunks %/% 2^16
		uoffsets <- chunks %% 2^16
		
		# Estimate uncompressed size
		csizes <- coffsets[,2L] - coffsets[,1L]
		usizes <- uoffsets[,2L] - uoffsets[,1L]
		chunkSizes <- csizes * compression + usizes
		
		# Aggregate by bin number
		chunkBins <- rep(names(chunkList), sapply(chunkList, nrow))
		sizes <- tapply(chunkSizes, chunkBins, sum)
		if(!isTRUE(rawSize)) sizes <- sizes / max(sizes)
		
		# Convert bin numbers into genomic coordinates
		coord <- bins2coord(bins=as.integer(names(sizes)))
		
		# Output
		results <- data.frame(
			name = sprintf("bin#%s", names(sizes)),
			chrom = factor(chrom, levels=header$SN),
			start = coord$start,
			end = coord$end,
			strand = factor(NA, levels=c("-","+")),
			value = as.double(sizes),
			stringsAsFactors = FALSE
		)
	} else {
		# Empty table
		results <- data.frame(
			name = character(0),
			chrom = factor(character(0), levels=header$SN),
			start = integer(0),
			end = integer(0),
			strand = factor(character(0), levels=c("-","+")),
			value = double(0),
			stringsAsFactors = FALSE
		)
	}
	
	# Output
	if(isTRUE(tracks)) {
		# track.table
		return(
			track.table(
				results,
				.organism = organism,
				.assembly = assembly,
				.name = sprintf("%s (coverage)", name)
			)
		)
	} else return(results)
},

crawl = function(chrom, start, end, addChr=.self$addChr, maxRange=.self$getParam("maxRange"), maxRangeWarn=TRUE, verbosity=0, ..., init, loop, final) {
"Apply a custom processing to reads in a genomic window (used by 'depth', 'extract' and 'pileup' methods).
- chrom          : single integer, numeric or character value, the chromosomal location. NA is not handled.
- start          : single integer or numeric value, inferior boundary of the window. NA is not handled.
- end            : single integer or numeric value, superior boundary of the window. NA is not handled.
- addChr         : single logical value, whether to systematically add 'chr' in front of the 'chrom' value or not.
- maxRange       : single integer value, no extraction will be attempted if end and start are more than this value away (returns NULL).
- maxRangeWarn   : single logical value, whether to throw a warning when 'maxRange' is exceeded and NULL is returned or not.
- verbosity      : single integer value, the level of verbosity during processing (0, 1 or 2).
- ...            : arguments to be passed to 'init', 'loop' or 'final'.
- init           : a function taking a single storage environment as argument, to be evaluated before looping on reads for initialization.
                   This environment has R 'base' environment as parent and contains :
                   * all arguments passed to crawl()
                   * a 'self' reference to the current object.
                   * 'earlyBreak', a single logical value forcing crawl() to return immediately if set to TRUE.
                   * 'output', a place-holder for the variable to be returned by crawl().
                   * 'totalReads', the number of matching reads seen since the beginning of the whole looping process.
                   * 'blockReads', the number of matching reads seen since the beginning of the current BGZF block.
                   The 'init', 'loop' and 'final' functions defined by the user can freely store additionnal variables in this environment to share them.                   
- loop           : a function taking a list-shapped read and the storage environment, to be evaluated for each read with matching coordinates.
- final          : a function taking the storage environment as argument, to be evaluated once all reads were processed for finalization."
	
	if(is.numeric(start)) start <- as.integer(start)
	if(is.numeric(end))   end <- as.integer(end)
	
	if(end - start < maxRange) {
		# Translate chrom name into index
		SN <- header$SN
		if(isTRUE(addChr)) chrom <- sprintf("chr%s", chrom)
		chromIndex <- match(chrom, SN)
		if(is.na(chromIndex)) stop("'chrom' not found in BAM header")
	
		if(verbosity > 0) message("Get chunks for given window ...")
		offsets <- getOffsets(index=index, chromIndex=chromIndex, start=start, end=end)
		
		# Prepare environment with arguments for functions
		if(verbosity > 0) message("Prepare environment ...")
		env <- list2env(
			x = list(
				chrom = chrom,
				start = start,
				end = end,
				addChr = addChr,
				maxRange = maxRange,
				verbosity = verbosity,
				...,
				self = .self,
				earlyBreak = FALSE,
				output = NULL,
				totalReads = as.integer(NA),
				blockReads = as.integer(NA)
			),
			parent = baseenv()
		)
		
		# Custom initialization
		if(verbosity > 0) message("Evaluate initialization ...")
		init(env)
		
		# BAI may return multiple intervals to collect
		env$totalReads <- 0L
		if(nrow(offsets) > 0) for(h in 1:nrow(offsets)) {
			if(verbosity > 0) message("Chunk #", h)
			
			# The interval defined in BAI may cover multiple BGZF blocks
			blockStart <- offsets[h,"c.start"]
			while(blockStart <= offsets[h,"c.end"]) {
				if(verbosity > 1) message("New BGZF block")
				
				# Opening file connection (no compression)
				if(verbosity > 1) message("Opening binary connection")
				con <- file(bamPath, "rb")
				
				# Extracting BGZF (compressed) block size
				if(verbosity > 1) message("Collecting BGZF block size")
				seek(con, origin="start", where=blockStart+16L)
				bsize <- readBin(con, what=0L, signed=FALSE, n=1L, size=2L) + 1L
				if(length(bsize) == 0) {
					if(verbosity > 0) message("End of file reached")
					close(con)
					break
				}
				
				# Seeking compressed file
				if(verbosity > 0) message("Seeking coffset ", blockStart)
				seek(con, origin="current", where=-18L)
				
				# Switching to compressed connection
				if(verbosity > 1) message("Enable on-the-fly uncompression")
				con <- gzcon(con)
				
				# Seeking uncompressed stream (only in first BGZF block)
				# Keep manual record of current uoffset as gzcon() does not
				if(blockStart == offsets[h,"c.start"]) {
					if(verbosity > 0) message("Seeking uoffset ", offsets[h,"u.start"])
					if(offsets[h,"u.start"] > 0) invisible(readBin(con, what="raw", n=offsets[h,"u.start"], size=1L))
					u <- offsets[h,"u.start"]
				} else { u <- 0L
				}
				
				# Loop on reads parsed
				env$blockReads <- 0L
				repeat {
					# End on chunk reached, stop parsing
					if(blockStart == offsets[h,"c.end"] && u >= offsets[h,"u.end"]) {	### FIXME last read to be kept or not ?
						if(verbosity > 0) message("End of chunk reached")
						break
					}
					
					if(verbosity > 1) message("Parsing read #", env$totalReads + 1L, " at uoffset ", u, appendLF=FALSE)
					block_size  <- readBin(con, what=0L, n=1L, size=4L, signed=TRUE)
					if(length(block_size) <= 0) {
						if(verbosity > 1) message(", cancelled as reaching end of BGZF block")
						break
					}
					env$blockReads <- env$blockReads + 1L
					env$totalReads <- env$totalReads + 1L
					
					# Add (or not) to read list
					read <- readRead(con, block_size=block_size, start=start, end=end, SN=SN, verbosity=verbosity)
					
					if(is.null(read)) {
						# Discarded read (out of region)
						env$totalReads <- env$totalReads - 1L
					} else {
						# Read custom processing
						loop(read, env)
					}
					
					# Requested break
					if(isTRUE(env$earlyBreak)) {
						if(verbosity > 1) message("Early break requested by looping function")
						break
					}
					
					# Update uoffset
					u <- u + 4L + block_size
				}
				
				# Close gzip connection
				close(con)
				
				# Requested break
				if(isTRUE(env$earlyBreak)) break
				
				# Next BGZF block
				blockStart <- blockStart + bsize
				if(verbosity > 0) message("Going to next BGZF block, ", env$blockReads, " reads parsed in current one")
			}
			
			if(verbosity > 0) message("Parsing complete, ", env$totalReads, " reads parsed")
		}
		
		# Final custom processing
		if(verbosity > 0) message("Evaluate finalization ...")
		final(env)
		
		# Get results
		output <- env$output
	} else {
		# Range too large
		if(isTRUE(maxRangeWarn)) { warning("maxRange (", maxRange, ") reached, no processing")
		} else if(verbosity > 0) { message("maxRange (", maxRange, ") reached, no processing")
		}
		output <- NULL
	}
	
	if(verbosity > 0) message("All done")
	
	return(output)
},

defaultParams = function(...) {
"Returns class-specific defaults for graphical parameters. Inheriting class should overload it to define their own defaults.
- ...   : may be used by inheriting methods, especially for inter-dependant parameters."
	
	params <- callSuper(...)
	
	params$ylab <- .self$name
	params$ysub <- .self$assembly
	
	params$mode <- "pileup"
	params$drawFun <- "draw.pileup"
	params$yaxt <- "s"
	params$yaxs <- "i"
	
	params$qBase <- 13L
	params$qMap <- as.integer(NA)
	
	return(params)
},

depth = function(..., qBase=.self$getParam("qBase"), qMap=.self$getParam("qMap")) {
"Counts covering bases for each genomic position, similarly to SAMtools' depth.
- ...     : arguments to be passed to the crawl() method.
- qBase   : single integer value, minimal base quality for a base to be counted.
- qMap    : single integer value, minimal mapping quality for a base to be counted."

	crawl(
		...,
		qBase = qBase,
		qMap = qMap,
		init = depth.init,
		loop = depth.loop,
		final = depth.final
	)
},

extract = function(...) {
"Extract reads as a list, similarly to SAMtools' view.
- ...     : arguments to be passed to the crawl() method."

	crawl(
		...,
		init = extract.init,
		loop = extract.loop,
		final = extract.final
	)
},

getBlocks = function(limit=NA, quiet=FALSE) {
"Jump from BGZF blocks to blocks, recording compressed (bsize) and uncompressed (isize) block sizes
- limit   : single integer value, the amount of blocks to evaluate (NA for the whole BAM file, may be very time consuming).
- quiet   : single logical value, whether to throw diagnostic messages or not."

	# BGZF block magic number
	h <- c(31L, 139L, 8L, 4L)

	# File size
	bamSize <- file.info(bamPath)[1,"size"]

	# To store valid coffsets
	coffsets <- integer(0)
	bsizes <- integer(0)
	isizes <- integer(0)

	# Parse BAM file
	con <- file(bamPath, "rb")
	on.exit(close(con))

	# Jump from block to block
	i <- 0; while(i < bamSize && (is.na(limit) || length(coffsets) < limit)) {
		# Check BGZF block header
		x <- readBin(con, what="integer", size=1L, n=4L, signed=FALSE)
		if(identical(x, h)) {
			# Retain valid offset
			coffsets <- c(coffsets, i)
			if(!isTRUE(quiet)) message("BGZF block found at coffset = ", format(i, big.mark="."), " / ", format(bamSize, big.mark="."))
		
			# Get compressed block size
			seek(con, origin="current", where=12L)
			bsize <- readBin(con, what="integer", signed=FALSE, n=1L, size=2L) + 1L
			i <- i + bsize
		
			# Get uncompressed block size
			seek(con, origin="current", where=bsize-4L-12L-2L-4L)
			isize <- readU32(con)
		
			# Store sizes
			bsizes <- c(bsizes, bsize)
			isizes <- c(isizes, isize)			
		} else stop("Broken path")
	}
	
	# Collect results
	output <- data.frame(
		coffset = coffsets,
		bsize = bsizes,
		isize = isizes
	)
	
	return(output)
},

getCompression = function(sample=500L) {
"Estimate BGZF block compression level from a sample of blocks
- sample   : single integer value, the amount of blocks to use for estimation (the first block is ignored)."
	
	tab <- getBlocks(limit=sample+1L, quiet=TRUE)
	output <- mean(tab[-1L,"isize"]) / mean(tab[-1L,"bsize"])
	return(output)
},

getChromEnd = function(chrom, addChr=.self$addChr) {
"Returns as a single integer value the ending position of the object description of the given chromosome. NA (integer) is valid if non relevant, but should be avoided when possible.
- chrom   : single integer, numeric or character value, the chromosomal location. NA is not required to be handled."
	
	# Optional tag in BAM header
	if("LN" %in% names(header)) {
		if(isTRUE(addChr)) { return(header[ header$SN == sprintf("chr%s", chrom) , "LN" ])
		} else             { return(header[ header$SN == chrom , "LN" ])
		}
	} else return(as.integer(NA))
},

initialize = function(bamPath=NA_character_, baiPath=NA_character_, header=data.frame(), index=list(), organism=NA_character_, assembly=NA_character_, ...) {
	callSuper(...)
	initFields(bamPath=bamPath, baiPath=baiPath, header=header, index=index, organism=organism, assembly=assembly)
},

pileup = function(..., qBase=.self$getParam("qBase"), qMap=.self$getParam("qMap")) {
"Counts each nucleotide type for each genomic position, similarly to SAMtools' mpileup.
- ...     : arguments to be passed to the crawl() method.
- qBase   : single integer value, minimal base quality for a base to be counted.
- qMap    : single integer value, minimal mapping quality for a base to be counted."

	crawl(
		...,
		qBase = qBase,
		qMap = qMap,
		init = pileup.init,
		loop = pileup.loop,
		final = pileup.final
	)
},

show = function(include=FALSE, fieldWidth=10) {
"Interactive printing
- include   : single logical value, if TRUE class name will not be printed."
	
	# Class name
	if(!isTRUE(include)) {
		cat("\n  \"track.bam\" reference class object\n")
	} else {
		cat("\n  Extends \"track.bam\"\n")
	}
	
	# Fields
	cat(sprintf("  %-*s : %s\n", fieldWidth, "name", name[1]))
	cat(sprintf("  %-*s : %s\n", fieldWidth, "organism", organism[1]))
	cat(sprintf("  %-*s : %s\n", fieldWidth, "assembly", assembly[1]))
	cat(sprintf("  %-*s : %s\n", fieldWidth, "BAM file", bamPath[1]))
	if(nrow(header) > 3)        { cat(sprintf("  %-*s : %s ...\n", fieldWidth, "references", paste(head(header$SN, 3), collapse=", ")))
	} else if(nrow(header) > 0) { cat(sprintf("  %-*s : %s\n", fieldWidth, "references", paste(header$SN, collapse=", ")))
	} else                      { cat(sprintf("  %-*s : %s\n", fieldWidth, "references", "<no @SQ>"))
	}
	
	# Inherited show()
	callSuper(include=TRUE, fieldWidth=fieldWidth)
},

slice = function(chrom, start, end, mode=.self$getParam("mode"), maxRangeWarn=FALSE, ...) {
"Extracts elements in the specified window, in a format suitable to draw().
- chrom          : single integer, numeric or character value, the chromosomal location. NA is not handled.
- start          : single integer or numeric value, inferior boundary of the window. NA is not handled.
- end            : single integer or numeric value, superior boundary of the window. NA is not handled.
- mode           : single character value, the name of the method to actually call to extract data (typically 'pileup' or 'coverage').
- maxRangeWarn   : single logical value, whether to throw a warning when 'maxRange' is exceeded and NULL is returned or not.
- ...            : to be passed to the crawl() method.
"
	eval(
		expr = substitute(
			expr = .self$FUN(chrom=CHROM, start=START, end=END, maxRangeWarn=maxRangeWarn, ...),
			env = list(FUN=mode, CHROM=chrom, START=start, END=end, maxRangeWarn=maxRangeWarn, ...)
		)
	)
},

summary = function(chrom=NA, tracks=TRUE, binLevel=5L, rawSize=FALSE) {
"Fast estimation of depth coverage for the whole genome, from indexing data. Values are normalized into [0:1] over the whole genome.
- chrom      : character vector, the names of the chromosome to query. If NA, all chromosomes will be queried.
- tracks     : single logical value, whether to return a data.frame or a track.table.
- binLevel   : single integer value, the higher bin order to allow
               0 = 537Mb, 1 = 67Mb, 2 = 8Mb, 3 = 1Mb, 4 = 130kb, 5 = 16kb
               incrementing this value enhances boundary precision but discards reads located at bin junctions
- rawSize    : single logical value, whether to output raw size or normalize by the maximum encountered."
	
	if(is.na(chrom)) {
		if(isTRUE(addChr)) { chrom <- sub("^chr", "", header$SN)
		} else             { chrom <- header$SN
		}
	}
	
	results <- NULL
	for(chr in chrom) {
		# Run coverage() on each chromosome
		results <- rbind(
			results,
			.self$coverage(chrom=chr, start=NA, end=NA, tracks=FALSE, binLevel=binLevel, rawSize=TRUE)
		)
	}
	
	# Normalize size
	if(!isTRUE(rawSize)) results$value <- results$value / max(results$value)
	
	# Output
	if(isTRUE(tracks)) {
		# track.table
		return(
			track.table(
				results,
				.organism = organism,
				.assembly = assembly,
				.name = sprintf("%s (coverage)", name)
			)
		)
	} else return(results)
}

	)
)

# Constructor
track.bam <- function(bamPath, baiPath, addChr, quiet=FALSE, .name, .organism, .assembly, .parameters, warn=TRUE) {
	# Meta data
	object <- new("track.bam")
	if(!missing(.organism))   object$organism <- .organism
	if(!missing(.assembly))   object$assembly <- .assembly
	if(!missing(.parameters)) object$parameters <- .parameters
	
	# Guess BAI path
	if(missing(baiPath)) {
		# .bam.bai
		baiPath <- dir(dirname(bamPath), pattern=sprintf("%s.bai", basename(bamPath)), ignore.case=TRUE, full.names=TRUE)
		if(length(baiPath) != 1) {
			# .bai
			baiPath <- dir(dirname(bamPath), pattern=sub("\\.bam$", ".bai", basename(bamPath), ignore.case=TRUE), ignore.case=TRUE, full.names=TRUE)
			if(length(baiPath) != 1) stop("Unable to guess 'baiPath', please provide it")
		}
	}
	
	# Normalize paths
	object$bamPath <- normalizePath(bamPath)
	if(is.na(baiPath)) { object$baiPath <- as.character(NA)
	} else             { object$baiPath <- normalizePath(baiPath)
	}
	
	# Parse BAM header
	object$header <- read.bam.header(bamPath)
	
	# Parse BAI
	if(is.na(baiPath)) {
		# Unaligned BAM file
		stop("Unaligned BAM files are not yet handled")
	} else {
		# Aligned BAM file
		object$index <- read.bai(baiPath, quiet=quiet)
	}
	
	# Add 'chr' default
	if(missing(addChr)) { object$addChr <- nrow(object$header) != 0 && any(grepl("^chr", object$header$SN, ignore.case=TRUE))
	} else              { object$addChr <- addChr
	}
	
	# Default name
	if(missing(.name)) { object$name <- basename(bamPath)
	} else             { object$name <- .name
	}
	
	# Compute compression level
	object$compression <- object$getCompression()
	
	# Check
	object$check(warn=warn)
	
	return(object)
}

