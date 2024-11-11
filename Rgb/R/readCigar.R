# Reads a CIGAR from a connexion, provided its operation count (n_cigar_op)
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

readCigar <- function(con, n) {
	# Working with integer bytes
	x <- readBin(con=con, what="integer", n=n*4L, size=1L, signed=FALSE)
	if(length(x) > 0L) {
		xint <- matrix(x, nrow=4L)
	
		# First 4 bits = operation
		op <- c("M", "I", "D", "N", "S", "H", "P", "=", "X")[ bitwAnd(xint[1L,], 15L) + 1L ]
	
		# 28 remaining bits = length
		xint[1L,] <- bitwShiftR(xint[1L,], 4L)
		xint <- xint * as.integer(2L ^ c(0L, 4L, 12L, 20L))
		out <- as.integer(colSums(xint))
	
		names(out) <- op
		return(out)
	} else {
		# Unavailable ("*")
		return(integer(0))
	}
}

