# R expressions for the track.bam$crawl() method, to counts cover depth in the genomic window
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

depth.init <- function(env) {
	# Prepare storage
	env$output <- integer(env$end - env$start + 1L)
	names(env$output) <- env$start : env$end
	
	# Default optional arguments
	if(!exists("qMap", env))  qMap <- NA
	if(!exists("qBase", env)) qBase <- NA
	
	return(TRUE)
}

depth.loop <- function(read, env) {
	# Not sufficient mapping quality
	if(!is.na(env$qMap) && read$MAPQ < env$qMap) return(TRUE)
	
	# Position in read sequence and in genome
	pRead <- 1L
	pGenome <- readStart <- read$POS
	readEnd <- read$.end
	
	# Loop on CIGAR operations
	opSizes <- read$CIGAR
	opTypes <- names(opSizes)
	for(o in 1:length(opSizes)) {
		opType <- opTypes[o]
		opSize <- opSizes[o]
		if(opType == "M" || opType == "=" || opType == "X") {
			# In SEQ and in reference
			# Cells to increment
			range <- pGenome - env$start + 1L:opSize
			cells <- range
			
			# Start filter
			if(readStart < env$start) { startFilter <- range > 0
			} else                    { startFilter <- TRUE
			}
			
			# End filter
			if(readEnd > env$end) { endFilter <- range < n
			} else                { endFilter <- TRUE
			}
			
			# Quality filter
			if(!is.na(env$qBase)) {
				qual <- read$QUAL[ pRead + 1L:opSize - 1L ]
				qualFilter <- qual >= env$qBase
			} else {
				qualFilter <- TRUE
			}
			
			# Do filter
			cells <- cells[ startFilter & endFilter & qualFilter ]
			
			# Do increment (in 'env' directly)
			if(length(cells) > 0L) {
				env$cells <- cells
				eval(expression(output[ cells ] <- output[ cells ] + 1L), envir=env)
			}
			
			# Update positions
			pRead <- pRead + opSize
			pGenome <- pGenome + opSize
		} else if(opType == "D" || opType == "N") {
			# Not in SEQ but in reference
			pGenome <- pGenome + opSize
		} else if(opType == "I" || opType == "S") {
			# In SEQ but not in reference
			pRead <- pRead + opSize
		} ### Else "P", "H"
	}
	
	return(TRUE)
}

depth.final <- function(env) {
	return(TRUE)
}

