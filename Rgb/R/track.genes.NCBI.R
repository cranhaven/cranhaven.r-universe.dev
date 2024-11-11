# Genes track from NCBI
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html
# Example file : ftp://ftp.ncbi.nih.gov/genomes/MapView/Homo_sapiens/sequence/current/initial_release/seq_gene.md.gz

track.genes.NCBI = function(
		file,
		name = "NCBI genes",
		selection,
		...
		)
	{
	# Extraction
	NCBI = utils::read.table(
		file = file,
		header = TRUE,
		sep = "\t",
		quote = "",
		comment.char = "",
		stringsAsFactors = FALSE
	)
	
	# Filtering out alternative assemblies
	if(missing(selection)) stop("'selection' can not be missing. Choose between the following values : ", paste(unique(NCBI$group_label), collapse=", " ))
	NCBI = NCBI[ NCBI$group_label %in% selection ,]
	
	# Filtering out other features
	NCBI = NCBI[ NCBI$feature_type == "GENE" ,]
	
	# Filtering out special chromosomes
	NCBI = NCBI[ !grepl(NCBI$chromosome, pattern="\\|") ,]
	
	# Gene ID
	geneId <- as.integer(sub("^.*GeneID:([0-9]+).*$", "\\1", NCBI$feature_id))
	
	# Track object
	object <- track.genes(
		# Columns
		name = NCBI$feature_name,
		chrom = NCBI$chromosome,
		start = as.integer(NCBI$chr_start),
		end = as.integer(NCBI$chr_stop),
		strand = factor(NCBI$chr_orient, levels=c("-","+")),
		GeneID = geneId,
		
		# Meta-data
		.name = name,
		...
	)
	
	return(object)
}
