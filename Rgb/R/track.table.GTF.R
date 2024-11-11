# Custom track from the UCSC Table Browser content exported as Gene Transfer Format file
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

track.table.GTF = function(
		file,
		name = NA,
		attr = "split",
		features = "exon",
		quiet = FALSE,
		.chromosomes,
		...
		)
	{
	# GTF parsing
	gtf <- read.gtf(file=file, attr=attr, features=features, quiet=quiet)
	
	# Filtering
	if(!is.null(features)) gtf <- gtf[ gtf$feature %in% features ,]
	
	# Consider "." strand as NA
	gtf$strand <- factor(gtf$strand, levels=c("-","+"))
	
	# Consider "seqname" as chromosome name
	if(missing(.chromosomes)) { gtf$chrom <- factor(sub("^chr", "", gtf$seqname))
	} else                    { gtf$chrom <- factor(sub("^chr", "", gtf$seqname), levels=.chromosomes)
	}
	gtf$seqname <- NULL
	
	# Use "source" as name if unique
	if(is.na(name) && length(unique(gtf$source)) == 1) {
		name <- unique(gtf$source)
		gtf$source <- NULL
	}
	
	# Track object
	object <- track.table(gtf, .makeNames=TRUE, .name=name, ...)
	
	return(object)
}
