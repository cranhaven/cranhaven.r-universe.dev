# Chromosomes cytobands track from UCSC
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html
# Example file : http://hgdownload.cse.ucsc.edu/goldenPath/hg19/database/cytoBand.txt.gz

track.bands.UCSC = function(
		file,
		name = "UCSC bands",
		...
		)
	{
	# Extraction
	UCSC = utils::read.table(
		file = file,
		header = FALSE,
		colClasses = c("character", "integer", "integer", "character", "character"),
		col.names = c("chrom", "chromStart", "chromEnd", "name", "gieStain"),
		sep = "\t",
		quote = "",
		comment.char = ""
	)
	
	# Rounding errors
	UCSC[ UCSC$chromStart == 0 , "chromStart" ] = 1
	
	# Chromosomes
	chrom <- gsub(UCSC$chrom, pattern="^chr(.+)$", replacement="\\1")
	
	# Track object
	object <- track.bands(
		# Columns
		name = sprintf("%s%s", chrom, UCSC$name),
		chrom = chrom,
		start = as.integer(UCSC$chromStart),
		end = as.integer(UCSC$chromEnd),
		strand = factor(rep("+", nrow(UCSC)), levels=c("-","+")),
		stain = UCSC$gieStain,
		
		# Meta-data
		.name = name,
		...
	)
	
	return(object)
}
