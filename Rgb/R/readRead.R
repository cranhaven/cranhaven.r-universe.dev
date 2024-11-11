# Di-nt dictionnary, for sequence conversion
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html
alpha <- strsplit("=ACMGRSVTWYHKDBN", split="")[[1]]
dict <- character(0)
for(N1 in alpha) {
	for(N2 in alpha) {
		bits <- c(intToBits(match(N1, alpha)-1L)[1:4], intToBits(match(N2, alpha)-1L)[1:4])
		n <- sum(as.integer(bits)*2^(0:7))
		dict[n] <- paste(N2, N1, sep="")
	}
}
rm(alpha)

# Reads a single read from a file connection
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html
readRead <- function(con, block_size, start, end, SN, verbosity) {
	# Parse read content
	refID       <- readBin(con=con, what="integer", n=1L, size=4L, signed=TRUE)
	pos         <- readBin(con=con, what="integer", n=1L, size=4L, signed=TRUE) + 1L
	l_read_name <- readBin(con=con, what="integer", n=1L, size=1L, signed=FALSE)
	mapq        <- readBin(con=con, what="integer", n=1L, size=1L, signed=FALSE)
	bin         <- readBin(con=con, what="integer", n=1L, size=2L, signed=FALSE)
	n_cigar_op  <- readBin(con=con, what="integer", n=1L, size=2L, signed=FALSE)
	flag        <- readBin(con=con, what="integer", n=1L, size=2L, signed=FALSE)
	l_seq       <- readBin(con=con, what="integer", n=1L, size=4L, signed=TRUE)
	next_refID  <- readBin(con=con, what="integer", n=1L, size=4L, signed=TRUE)
	next_pos    <- readBin(con=con, what="integer", n=1L, size=4L, signed=TRUE) + 1L
	tlen        <- readBin(con=con, what="integer", n=1L, size=4L, signed=TRUE)
	read_name   <- readBin(con=con, what="character", n=1L, size=l_read_name, signed=FALSE)
	cigar       <- readCigar(con, n=n_cigar_op)
	seq_size    <- floor((l_seq+1L)/2L)
	seq         <- readBin(con=con, what="integer", n=seq_size, size=1L, signed=FALSE)
	seqChar     <- unlist(strsplit(dict[ seq ], split=""))[ 1L : l_seq ]
	qual        <- readBin(con=con, what="integer", n=l_seq, size=1L, signed=FALSE)

	# TODO optionnal tag-values
	opt_size    <- block_size - (32L + l_read_name + n_cigar_op * 4L + seq_size + l_seq)
	opt         <- readBin(con=con, what="raw", n=opt_size, size=NA_integer_, signed=FALSE)

	# Unmapped
	if(refID == -1L)      pos <- refID <- NA_integer_
	if(next_refID == -1L) next_pos <- next_refID <- NA_integer_
	if(tlen == 0L)        tlen <- NA_integer_

	# Ending coordinate in reference
	pos_end <- pos + sum(cigar[ names(cigar) %in% c("M", "=", "X", "D", "N", "P") ]) - 1L
	
	# Read in queried window or not
	if(pos <= end && pos_end >= start) {
		if(verbosity > 1) message(", retained")
		out <- list(
			QNAME = read_name,
			FLAG = flag,
			RNAME = SN[ refID + 1L ],
			POS = pos,
			MAPQ = mapq,
			CIGAR = cigar,
			RNEXT = SN[ next_refID + 1L ],
			PNEXT = next_pos,
			TLEN = tlen,
			SEQ = seqChar,
			QUAL = qual,
			OPT = opt,
			.end = pos_end
		)
		### TODO 'strand' in the flag
	} else {
		if(verbosity > 1) message(", discarded")
		out <- NULL
	}
	
	return(out)
}

