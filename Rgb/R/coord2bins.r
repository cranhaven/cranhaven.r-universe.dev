# Convert genomic start/end coordinates into BAM index bin numbers
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

coord2bins <- function(start, end) {
	# Checks
	if(!is.integer(start))           stop("'start' must be an integer vector")
	if(!is.integer(end))             stop("'end' must be an integer vector")
	if(length(start) != length(end)) stop("'start' and 'end' must have same lengths")
	
	# Indexed region limit
	start <- max(start, 0L)
	end <- min(end, as.integer(2^29))
	
	# All bins overlapping the region (whatever the chromosome)
	bins <- c(
		0L,
		(1L + bitwShiftR(start, 26L)):(1L + bitwShiftR(end, 26L)),
		(9L + bitwShiftR(start, 23L)):(9L + bitwShiftR(end, 23L)),
		(73L + bitwShiftR(start, 20L)):(73L + bitwShiftR(end, 20L)),
		(585L + bitwShiftR(start, 17L)):(585L + bitwShiftR(end, 17L)),
		(4681L + bitwShiftR(start, 14L)):(4681L + bitwShiftR(end, 14L))
	)
	if(length(bins) == 0) stop("Out of indexed region")
	
	return(bins)
}

