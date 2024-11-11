# Exon track from CCDS
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html
# Example file : ftp://ftp.ncbi.nlm.nih.gov/pub/CCDS/current_human/CCDS.current.txt

track.exons.CCDS = function(
		file,
		name = "CCDS exons",
		...
		)
	{
	# Extraction
	CCDS = utils::read.table(
		file = file,
		header = TRUE,
		sep = "\t",
		quote = "",
		comment.char = "",
		stringsAsFactors = FALSE
	)
	
	# Filtering
	CCDS = CCDS[ CCDS$ccds_status == "Public" ,]
	
	# Exon split
	exons = sub("^\\[(.*)\\]", "\\1", CCDS$cds_locations)
	exons = strsplit(exons, "(, )|\\-")
	exonsStart = lapply(exons, "[", c(TRUE, FALSE))
	exonsEnd = lapply(exons, "[", c(FALSE, TRUE))
	exonsCount = sapply(exonsStart, length)
	
	# Exon number
	exonsNum = lapply(exonsCount, seq.int, from=1, by=1)
	for(i in 1:nrow(CCDS)) { exonsNum[[i]] = 1:exonsCount[i] }
	exonsNum[ CCDS$cds_strand == "-" ] = lapply(exonsNum[ CCDS$cds_strand == "-" ], rev)
	
	# Track object
	object <- track.exons(
		# Columns
		name = sprintf("%s|%i", rep(CCDS$ccds_id, exonsCount), unlist(exonsNum)),
		chrom = rep(CCDS$X.chromosome, exonsCount),
		start = as.integer(unlist(exonsStart)),
		end = as.integer(unlist(exonsEnd)),
		strand = factor(rep(CCDS$cds_strand, exonsCount), levels=c("-","+")),
		transcript = sprintf("%s (%s)", rep(CCDS$gene, exonsCount), sub("^CCDS", "", rep(CCDS$ccds_id, exonsCount))),
		groupPosition = unlist(exonsNum),
		groupSize = rep(exonsCount, exonsCount),
		
		# Meta-data
		.name = name,
		...
	)
	
	return(object)
}

