# Exon track from GENCODE
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html
# Example file : ftp://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_29/gencode.v29.annotation.gtf.gz

track.exons.GENCODE = function(
		file,
		name = "GENCODE exons",
		extra = c("gene_id", "gene_name", "exon_id"),
		...
		)
	{
	# Parse as a GTF file
	track <- track.table.GTF(file, name=name, ...)

	# Remove unwanted columns
	mandatory <- c("name", "chrom", "strand", "start", "end", "transcript_id", "transcript_name")
	columns <- union(mandatory, extra)
	track$delColumns(setdiff(track$getColNames(), columns))

	# Rename transcript column
	track$setColNames("transcript_name", "transcript")

	# Group by transcript
	track$buildGroupPosition(groupBy="transcript")
	track$buildGroupSize(groupBy="transcript")

	# Use transript name to build name
	track$fill(, "name", paste(sub("^ENST0+", "", track$extract(,"transcript_id")), track$extract(,"groupPosition"), sep="|"))

	# Coerce to track.exons
	object <- new("track.exons")
	object$import(track)
	
	return(object)
}

