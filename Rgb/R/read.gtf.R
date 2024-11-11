# Parses a Gene Transfer Format file into a data.frame
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html
# Specification : http://genome.ucsc.edu/FAQ/FAQformat.html#format4, http://www.sequenceontology.org/gff3.shtml

read.gtf <- function(file, attr=c("split", "intact", "skip"), features=NULL, quiet=FALSE) {
	# Checks
	attr <- match.arg(attr)
	
	if(!isTRUE(quiet)) message("File parsing ... ", appendLF=FALSE)
	
	# Parsing
	columns <- list("seqname"=character(0), "source"=character(0), "feature"=character(0), "start"=integer(0), "end"=integer(0), "score"=double(0), "strand"=character(0), "frame"=integer(0), "attributes"=character(0))
	content <- scan(file=file, what=columns, sep="\t", dec=".", comment.char="#", na.strings=".", quote="\"", quiet=TRUE)
	names(content) <- names(columns)
	
	# Strand
	content$strand[ is.na(content$strand) ] <- "."
	content$strand[ content$strand == "?" ] <- NA
	content$strand <- factor(content$strand, levels=c("-","+","."))
	
	# As data.frame
	class(content) <- "data.frame"
	rownames(content) <- 1:length(content$seqname)
	
	# Feature filtering
	if(!is.null(features)) content <- content[ content$feature %in% features , , drop=FALSE ]
	
	if(!isTRUE(quiet)) message(nrow(content), " rows processed")
	
	# Attributes
	if(attr == "skip") {
		# No attribute
		content$attributes <- NULL
	} else if(attr == "split") {
		
		if(!isTRUE(quiet)) message("Attribute splitting ... ", appendLF=FALSE)
		
		# Split attributes of each row
		att <- strsplit(content$attributes, split=" *; *")
		
		# Vectorize all attributes, keeping a parallel row index for each
		attRows <- rep.int(1:length(att), times=unlist(lapply(att, length)))
		att <- unlist(att)
		
		# Split all name-value pairs
		regex <- regexpr(pattern="^(?<id>[A-Za-z][A-Za-z0-9_]*).(?<value>.+)$", text=att, perl=TRUE)
		attNames <- substr(att, 1, attr(regex, "capture.length")[,"id"])
		attValues <- substr(att, attr(regex, "capture.start")[,"value"], nchar(att))
		
		if(!isTRUE(quiet)) message(length(attValues), " pairs processed")
		
		if(!isTRUE(quiet)) message("Attribute sorting ... ", appendLF=FALSE)
		
		# Initialize a storage matrix (character)
		allNames <- unique(attNames)
		attMtx <- matrix(as.character(NA), nrow=nrow(content), ncol=length(allNames), dimnames=list(NULL, allNames))
		
		# Fill the storage matrix
		attMtx[ cbind(attRows, match(attNames, allNames)) ] <- attValues
		
		if(!isTRUE(quiet)) message(ncol(attMtx), " tags found")
		
		if(!isTRUE(quiet)) message("Attribute binding ...")
		
		# Convert character matrix to typed data.frame
		attDf <- as.data.frame(attMtx, stringsAsFactors=FALSE)
		for(i in 1:ncol(attDf)) attDf[[i]] <- utils::type.convert(attDf[[i]], as.is=TRUE)
		
		# Append to content
		content$attributes <- NULL
		content <- cbind(content, attDf, stringsAsFactors=FALSE)
	}
	
	if(!isTRUE(quiet)) message("done")
	
	# Return
	return(content)
}

