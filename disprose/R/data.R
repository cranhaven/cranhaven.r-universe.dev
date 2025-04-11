#' Chlamydia pneumoniae genome annotation.
#'
#' A dataset containing Chlamydia pneumoniae TW-183 (complete sequence, NC_005043.1.) genome annotation
#'
#' @format A data frame with 2218 rows and 9 variables:
#' \describe{
#'   \item{seqid}{sequence identification number}
#'   \item{source}{source database name}
#'   \item{type}{type of annotated region}
#'   \item{start}{region's start position}
#'   \item{end}{region's end position}
#'   \item{score}{score}
#'   \item{strand}{strand}
#'   \item{phase}{phase}
#'   \item{attribute}{region description}
#' }
#' @source \url{https://www.ncbi.nlm.nih.gov/}
"ann.data"
#'
#' Metadata of all available Chlamydia pneumoniae's sequences.
#'
#' A dataset containing metadata of all Chlamydia pneumoniae's nucleotide sequences
#' that were downloaded from NCBI Nucleotide database (November, 2021)
#'
#' @format A data frame with 9062 rows and 21 variables:
#' \describe{
#'   \item{uid}{sequence identification number}
#'   \item{gi}{sequence identification number}
#'   \item{GB_AcNum}{sequence identification number}
#'   \item{createdate}{date of note's creation}
#'   \item{updatedate}{date of last note's update}
#'   \item{source_db}{database}
#'   \item{organism}{organism name}
#'   \item{title}{sequence title}
#'   \item{strain}{strain}
#'   \item{taxid}{taxon identificator}
#'   \item{length}{sequence length}
#'   \item{biomol}{biomolecule}
#'   \item{moltype}{molecular type}
#'   \item{genome}{genome type}
#'   \item{complete}{sequence completness}
#'   \item{geneticcode}{type of genetic and codon codes}
#'   \item{strand}{strand}
#'   \item{host}{host}
#'   \item{country}{country}
#'   \item{isolation_source}{isolation material}
#'   \item{collection_date}{collection date}
#'   }
#' @source \url{https://www.ncbi.nlm.nih.gov/}
"meta.all"
#'
#' Metadata of target Chlamydia pneumoniae's sequences.
#'
#' A dataset containing target nucleotide sequences of Chlamydia pneumoniae
#' that were downloaded from NCBI Nucleotide database (November, 2021).
#' Target sequences are chosen from all available sequences as targets for discriminating probes.
#'
#' @format A data frame with 183 rows and 21 variables:
#' \describe{
#'   \item{uid}{sequence identification number}
#'   \item{gi}{sequence identification number}
#'   \item{GB_AcNum}{sequence identification number}
#'   \item{createdate}{date of note's creation}
#'   \item{updatedate}{date of last note's update}
#'   \item{source_db}{database}
#'   \item{organism}{organism name}
#'   \item{title}{sequence title}
#'   \item{strain}{strain}
#'   \item{taxid}{taxon identificator}
#'   \item{length}{sequence length}
#'   \item{biomol}{biomolecule}
#'   \item{moltype}{molecular type}
#'   \item{genome}{genome type}
#'   \item{complete}{sequence completness}
#'   \item{geneticcode}{type of genetic and codon codes}
#'   \item{strand}{strand}
#'   \item{host}{host}
#'   \item{country}{country}
#'   \item{isolation_source}{isolation material}
#'   \item{collection_date}{collection date}
#'   }
#' @source \url{https://www.ncbi.nlm.nih.gov/}
"meta.target"
#'
#' Local BLAST results.
#'
#' Result of BLAST of 5 probes against local database of target nucleotide sequences
#' of Chlamydia pneumoniae. Local BLAST was performed with blast_local () function.
#'
#' @format A data frame with 72 rows and 19 variables:
#' \describe{
#'   \item{probe}{probe sequence}
#'   \item{probe.length}{probe sequence's length}
#'   \item{Qid}{query identification number}
#'   \item{Qstart}{query start position}
#'   \item{Qend}{query end position}
#'   \item{Rgi}{subject GenInfo Identifier number}
#'   \item{Racc}{subject NCBI accession number}
#'   \item{Rtitle}{subject title}
#'   \item{Rtaxid}{subject taxon identificator}
#'   \item{Rstart}{subject start position}
#'   \item{Rend}{subject end position}
#'   \item{alig.length}{length of alignment}
#'   \item{mismatch}{amount of mismatches}
#'   \item{gaps}{amount of gaps}
#'   \item{ident.number}{amount of identical positions}
#'   \item{score}{alignment score}
#'   \item{bitscore}{alignment bitscore}
#'   \item{Evalue}{alignment e-value}
#'   \item{Qcover}{query coverage, \%}
#'   }
"blast.raw"
#'
#' Local BLAST results with added content.
#'
#' Result of BLAST of 5 probes against local database of target nucleotide sequences
#' of Chlamydia pneumoniae. Local BLAST was performed with blast_local () function.
#' Subjects' Genbank Identifiers are added with fill_blast_result () function.
#'
#' @format A data frame with 72 rows and 19 variables:
#' \describe{
#'   \item{probe}{probe sequence}
#'   \item{probe.length}{probe sequence's length}
#'   \item{Qid}{query identification number}
#'   \item{Qstart}{query start position}
#'   \item{Qend}{query end position}
#'   \item{Rgi}{subject GenInfo Identifier number}
#'   \item{Racc}{subject NCBI accession number}
#'   \item{Rtitle}{subject title}
#'   \item{Rtaxid}{subject taxon identificator}
#'   \item{Rstart}{subject start position}
#'   \item{Rend}{subject end position}
#'   \item{alig.length}{length of alignment}
#'   \item{mismatch}{amount of mismatches}
#'   \item{gaps}{amount of gaps}
#'   \item{ident.number}{amount of identical positions}
#'   \item{score}{alignment score}
#'   \item{bitscore}{alignment bitscore}
#'   \item{Evalue}{alignment e-value}
#'   \item{Qcover}{query coverage, \%}
#'   }
"blast.fill"
