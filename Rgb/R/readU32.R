# Reads a single unsigned 32 bit integer (stored as double)
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

readU32 <- function(con) {
	bytes <- readBin(con=con, what="integer", n=2L, size=2L, signed=FALSE)
	output <- bytes[2L] * 65536 + bytes[1L]
	return(output)
}

