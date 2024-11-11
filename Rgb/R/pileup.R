# R expressions for the track.bam$crawl() method, to count each nucleotide type in the genomic window
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

pileup.init <- function(env) {
	# Prepare storage
	env$alpha <- c("A", "C", "G", "T")
	env$output <- matrix(0L, nrow=env$end - env$start + 1L, ncol=4L, dimnames=list(env$start : env$end, env$alpha))
	
	# Default optional arguments
	if(!exists("qMap", env))  qMap <- NA
	if(!exists("qBase", env)) qBase <- NA
	
	return(TRUE)
}

pileup.loop <- function(read, env) {
	# Not sufficient mapping quality
	if(!is.na(env$qMap) && read$MAPQ < env$qMap) return(TRUE)
	
	# Position in read sequence and in genome
	pRead <- 1L
	pGenome <- readStart <- read$POS
	readEnd <- read$.end
	
	# Loop on CIGAR operations
	opSizes <- read$CIGAR
	opTypes <- names(opSizes)
	if(length(opSizes) > 0L) for(o in 1:length(opSizes)) {
		opType <- opTypes[o]
		opSize <- opSizes[o]
		if(opType == "M" || opType == "=" || opType == "X") {
			# In SEQ and in reference
			# Cells to increment
			range <- pGenome - env$start + 1L:opSize
			bases <- read$SEQ[ pRead + 1L:opSize - 1L ]
			cells <- cbind(range, match(bases, env$alpha))
			
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
			cells <- cells[ startFilter & endFilter & qualFilter , , drop=FALSE ]
			
			# Do increment (in 'env' directly)
			if(nrow(cells) > 0L) {
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

pileup.final <- function(env) {
	return(TRUE)
}

