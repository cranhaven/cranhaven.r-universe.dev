# Collects data on FASTA file content for future extraction
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

probeFasta <- function(file) {
	### Assuming first line is always header
	### Assuming fixed common width for all sequence lines
	
	# Opening file
	con <- file(file, open="rb")
	on.exit(close(con))
	
	# Header line
	header <- readLines(con, n=1L)
	if(length(header) == 1 && substr(header,1,1) != ">") stop("'file' is not a regular FASTA file (no '>' header)")
	headerSize <- nchar(header)
	headerEnd <- seek(con)
	
	# Line breaking detection
	seek(con, where=-2L, origin="current")
	breaks <- readBin(con, what="raw", n=2L)
	if(all(breaks == charToRaw("\r\n")))    { breakSize <- 2L
	} else if(breaks[2] == charToRaw("\n")) { breakSize <- 1L
	} else                                  { stop("Unknown line breaking character pattern")
	}
	
	# First line (to guess the FASTA width)
	firstLine <- readLines(con, n=1L)
	lineLength <- nchar(firstLine)
	
	# Ending detection
	seek(con, where=-20L, origin="end")
	ending <- readBin(con, "raw", n=20L)
	i <- 0L
	while(ending[20L-i] %in% charToRaw("\r\n")) i <- i + 1L
	endingSize <- i
	
	# Sequence size estimation from file size
	contentSize <- as.integer(file.info(file)[1,'size']) - (headerSize + breakSize) - endingSize
	contentBreaks <- as.integer(floor(contentSize / (lineLength + breakSize)))
	contentSize <- contentSize - contentBreaks*breakSize
	
	# Aggregate data
	out <- list(
		file = normalizePath(file),
		header = header,
		startOffset = headerEnd,
		lineLength = lineLength,
		breakSize = breakSize,
		contentSize = contentSize
	)
	
	return(out)
}

