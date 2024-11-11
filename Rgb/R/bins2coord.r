# Convert BAM index bin numbers into genomic start/end coordinates
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

bins2coord <- function(bins) {
	# Checks
	if(!is.integer(bins)) stop("'bins' must be an integer vector")
	
	# Bin sizes, as defined in BAM specification
	resolutions <- as.integer(2 ^ c(29, 26, 23, 20, 17, 14))
	blockStarts <- as.integer(c(0, 1, 9, 73, 585, 4681))
	blockEnds <- as.integer(c(0, 8, 72, 584, 4680, 37449))
	
	# Compute bin boundaries
	start <- end <- resolution <- rep(as.integer(NA), length(bins))
	for(i in 1:length(resolutions)) {
		concerned <- bins >= blockStarts[i] & bins <= blockEnds[i]
		resolution[concerned] <- resolutions[i]
		start[concerned] <- (bins[concerned] - blockStarts[i]) * resolution[concerned]
		end[concerned] <- start[concerned] + resolution[concerned]
	}
	
	return(list(bin=bins, start=start, end=end))
}

