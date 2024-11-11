# CNV track from the new version of DGV
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html
# Example file : http://dgvbeta.tcag.ca/dgv/app/downloads (Release Version)

track.CNV.DGV = function(
		file,
		name = "DGV CNV",
		...
		)
	{
	# Extraction
	DGV <- utils::read.table(
		file = file,
		header = TRUE,
		sep = "\t",
		quote = "",
		comment.char = "",
		stringsAsFactors = FALSE
	)
	
	# Filtering out other features
	DGV <- DGV[ DGV$varianttype == "CNV" ,]
	
	# Track object
	object <- track.CNV(
		# Columns
		name = as.character(DGV$variantaccession),
		chrom = as.character(DGV$chr),
		start = as.integer(DGV$start),
		end = as.integer(DGV$end),
		strand = factor(rep("+", nrow(DGV)), levels=c("-","+")),
		type = factor(DGV$variantsubtype),
		
		# Meta-data
		.name = name,
		...
	)	
	
	return(object)
}
