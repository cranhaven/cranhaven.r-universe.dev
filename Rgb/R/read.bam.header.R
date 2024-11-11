# Parses a BAM file (.bam) header
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

read.bam.header <- function(fileName) {
	# Magic number check
	con <- gzfile(fileName, open="rb")
	magic <- readBin(con, what="raw", n=4)
	close(con)
	if(!identical(magic, charToRaw("BAM\1"))) return("Wrong magic number")
	
	# EOF block check
	con <- file(fileName, open="rb")
	seek(con, where=-28, origin="end")
	fileEOF <- readBin(con, what="raw", n=28)
	close(con)
	expectedEOF <- as.raw(strtoi(c("1f", "8b", "08", "04", "00", "00", "00", "00", "00", "ff", "06", "00", "42", "43", "02", "00", "1b", "00", "03", "00", "00", "00", "00", "00", "00", "00", "00", "00"), base=16))
	if(!identical(fileEOF, expectedEOF)) return("Truncated BAM file")
	
	# Read first BGZF block
	con <- gzcon(file(fileName, open="rb"))
	header <- iconv(suppressWarnings(readLines(con)), to="ASCII")
	close(con)
	
	# Split reference tag/value pairs
	ref <- grep("^@SQ", header, value=TRUE)
	if(length(ref) > 0) {
		ref <- strsplit(ref, split="\t")
		ref <- lapply(ref, "[", -1)
	
		# Vectorize
		lin <- rep(1:length(ref), sapply(ref, length))
		ref <- unlist(ref)
		tag <- sub("^([A-Z0-9]{2}):(.*)$", "\\1", ref)
		val <- sub("^([A-Z0-9]{2}):(.*)$", "\\2", ref)
	
		# Fill in a matrix
		mtx <- matrix(data=as.character(NA), ncol=length(unique(tag)), nrow=max(lin), dimnames=list(NULL, unique(tag)))
		mtx[ cbind(lin, match(tag, colnames(mtx))) ] <- val
	
		# Convert to data.frame
		tab <- as.data.frame(mtx, stringsAsFactors=FALSE)
		for(k in 1:ncol(tab)) tab[[k]] <- utils::type.convert(tab[[k]], as.is=TRUE)
	} else {
		# Empty header
		tab <- data.frame(SN=character(0), stringsAsFactors=FALSE)
	}
	
	return(tab)
}

