# R expressions for the track.bam$crawl() method, to get full read data in the genomic window
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

extract.init <- function(env) {
	env$output <- vector("list", 5000L)
	
	return(TRUE)
}

extract.loop <- function(read, env) {
	# Extend output list if necessary
	if(env$totalReads > length(env$output)) {
		eval(expression(output <- c(output, vector("list", 5000L))), envir=env)
	}
	
	# Store whole read
	env$read <- read
	eval(expression(output[[ totalReads ]] <- read), envir=env)
	
	return(TRUE)
}

extract.final <- function(env) {
	# Remove unused read slots
	eval(
		expression({
			# Remove unused read slots
			output <- output[ 1:totalReads ]
			
			# Apply S3 class
			attr(output, "bam") <- self$bamPath
			attr(output, "chrom") <- chrom
			attr(output, "start") <- start
			attr(output, "end") <- end
			class(output) <- "bamSlice"
		}),
		envir = env
	)
	
	return(TRUE)
}

# S3 method for BAI printing
print.bamSlice <- function(x, ...) {
	cat("Read list\n")
	cat("- BAM    : ", attr(x, "bam"), "\n", sep="")
	cat("- Region : ", attr(x, "chrom"), ":", attr(x, "start"), "-", attr(x, "end"), "\n", sep="")
	
	if(length(x) > 0) {
		reads <- x[[1]]$QNAME
		if(length(x) > 1) reads <- sprintf("%s ...", reads)
		reads <- sprintf(" (%s)", reads)
	} else {
		reads <- ""
	}
	cat("- Reads  : ", length(x), reads, "\n", sep="")
	invisible(x)
}

