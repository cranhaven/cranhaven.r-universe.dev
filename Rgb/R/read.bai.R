# Parses a BAM index file (.bam.bai)
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

read.bai <- function(fileName, quiet=FALSE) {
	# Binary parsing
	con <- file(fileName, open="rb")
	on.exit(close(con))
	
	# Magic number check
	magic <- readBin(con, what="raw", n=4)
	if(!identical(magic, charToRaw("BAI\1"))) stop("Wrong magic number")
	
	# Get reference count
	n_ref <- readBin(con=con, what="integer", n=1L, size=4L, signed=TRUE)
	
	# Allocate reference list
	out <- vector(mode="list", length=n_ref)
	
	dn <- list(NULL, c("start", "end"))
	if(n_ref > 0) for(r in 1:n_ref) {
		# Get bin count
		n_bin <- readBin(con=con, what="integer", n=1L, size=4L, signed=TRUE)
		
		# Allocate bin list
		refBins <- vector(mode="list", length=n_bin)
		binIds <- character(n_bin)
		
		if(!isTRUE(quiet)) message("Parsing reference #", r, " (", n_bin, " bins)")
		
		if(n_bin > 0) for(b in 1:n_bin) {
			# Get bin ID
			binIds[b] <- readU32(con)
			
			# Get chunk count
			n_chunk <- readBin(con=con, what="integer", n=1L, size=4L, signed=TRUE)
			
			# Get chunk coordinates (2 by chunk)
			coord <- readU64s(con, n=n_chunk*2L)
			
			# Allocate chunk matrix
			refBins[[b]] <- matrix(coord, ncol=2L, nrow=n_chunk, byrow=TRUE, dimnames=dn)
		}
		
		# Store bin IDs
		names(refBins) <- binIds
		
		# Get interval count
		n_intv <- readBin(con=con, what="integer", n=1L, size=4L, signed=TRUE)
		
		# Allocate and fill interval vector
		refIntervals <- readU64s(con, n=n_intv)
		
		# Link to output
		out[[r]] <- list(
			bins = refBins,
			intervals = refIntervals
		)
	}
	
	return(out)
}

