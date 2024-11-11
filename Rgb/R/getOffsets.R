# Returns the compressed and uncompressed BAM indexes for the queried genomic window
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

getOffsets <- function(index, chromIndex, start, end, merge=TRUE) {
	# All bins overlapping the region (whatever the chromosome)
	if(is.numeric(start)) start <- as.integer(start)
	if(is.numeric(end))   end <- as.integer(end)
	if(start > end)       stop("'start' must be lesser or equal to 'end'")
	bins <- coord2bins(start=start, end=end)
	
	# Extract chunks from these bins
	chunkList <- index[[ chromIndex ]]$bins[ as.character(bins) ]
	chunks <- do.call(rbind, args=chunkList)
	
	if(!is.null(chunks)) {
		# Linear index filtering
		limit <- index[[ chromIndex ]]$intervals[ bitwShiftR(start, 14L) + 1L ]
		chunks <- chunks[ chunks[,"end"] > limit , , drop=FALSE ]
		
		if(isTRUE(merge) && nrow(chunks) > 1L) {
			# Merge consecutive / overlapping chunks
			chunks <- chunks[ order(chunks[,"start"]) , , drop=FALSE ]
			groups <- 1L
			for(i in 2:nrow(chunks)) {
				opened <- groups[1L]
				if(chunks[i,"start"] <= chunks[opened,"end"]) { chunks[opened,"end"] <- max(chunks[c(i,opened),"end"])
				} else                                        { groups <- c(i, groups)
				}
			}
			chunks <- chunks[ groups , , drop=FALSE ]
		}
		
		# Cosmetics
		rownames(chunks) <- NULL
		
		# Decomposes BGZF virtual offsets into compressed and uncompressed offsets
		offsets <- cbind(
			chunks %/% 2^16,   # coffset = block start (unsigned)
			chunks %% 2^16     # uoffset = position in uncompressed block (unsigned)
		)
		colnames(offsets) <- c("c.start", "c.end", "u.start", "u.end")
	} else {
		# Not indexed, probably empty
		offsets <- matrix(as.double(NA), nrow=0, ncol=4, dimnames=list(NULL, c("c.start", "c.end", "u.start", "u.end")))
	}
	
	return(offsets)
}

