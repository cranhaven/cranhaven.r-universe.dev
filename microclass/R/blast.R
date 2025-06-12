#' @name blastClassify16S
#' @title Classifying using BLAST
#' 
#' @description A 16S based classification based on BLAST.
#' 
#' @param sequence Character vector of 16S sequences to classify.
#' @param bdb Name of BLAST data base, see \code{\link{blastDbase16S}}.
#' 
#' @details A vector of 16S sequences (DNA) are classified by first using BLAST \code{blastn} against
#' a database of 16S DNA sequences, and then classify according to the nearest-neighbour principle.
#' The nearest neighbour of a query sequence is the hit with the largest bitscore. The blast+
#' software  \url{https://blast.ncbi.nlm.nih.gov/Blast.cgi?PAGE_TYPE=BlastDocs&DOC_TYPE=Download}
#' must be installed on the system. Type \code{system("blastn -help")} in the Console window,
#' and a sensible Help-text should appear.
#' 
#' The database must contain 16S sequences where the Header starts with a token specifying the taxon. 
#' More specifically, the tokens must look like:
#' 
#' <taxon>_1
#' 
#' <taxon>_2
#' 
#' ...etc
#' 
#' where <taxon> is some proper taxon name. Use \code{\link{blastDbase16S}} to make such databases.
#' 
#' The identity of each alignment is also computed. This should be close to 1.0 for a classification
#' to be trusted. Identity values below 0.95 could indicate uncertain classifications, but this will
#' vary between taxa.
#' 
#' @return A \code{data.frame} with two columns: Taxon is the predicted taxon for each \code{sequence}
#' and Identity is the corresponding identity-value. If no BLAST hit is seen, the sequence is 
#' \code{"unclassified"}.
#' 
#' @author Lars Snipen.
#' 
#' @seealso \code{\link{blastDbase16S}}.
#' 
#' @examples 
#' data("small.16S")
#' \dontrun{
#' dbase <- blastDbase16S("test", small.16S$Sequence, word(small.16S$Header, 2, 2))
#' reads <- str_sub(small.16S$Sequence, 100, 550)
#' blastClassify16S(reads, dbase) %>% 
#'   bind_cols(small.16S) -> tbl
#' }
#' 
#' @importFrom utils read.table
#' @importFrom microseq writeFasta
#' @importFrom dplyr rename arrange distinct mutate desc
#' @importFrom stringr str_remove
#' @importFrom rlang .data
#' 
#' @export blastClassify16S
#' 
blastClassify16S <- function(sequence, bdb){
  qry <- data.frame(Header = paste("Query", 1:length(sequence), sep = "_"),
                    Sequence = sequence,
                    stringsAsFactors = F)
  tfa <- tempfile(fileext = ".fasta")
  tft <- tempfile(fileext = ".txt")
  writeFasta(qry, out.file = tfa)
  cmd <- paste("blastn",
                "-query", tfa,
                "-db", bdb,
               "-num_alignments", 1,
                "-out", tft,
               "-outfmt \"6 qseqid qlen sseqid length pident bitscore\"")
  system(cmd)
  read.table(tft, sep="\t", header = F, stringsAsFactors = F) %>% 
    rename(Query = .data$V1, Qlen = .data$V2, Hit = .data$V3, Alen = .data$V4, Perc = .data$V5, Bitscore = .data$V6) %>% 
    arrange(desc(.data$Bitscore)) %>% 
    distinct(.data$Query, .keep_all = T) %>% 
    mutate(Taxon = str_remove(.data$Hit, "_[0-9]+$")) %>% 
    mutate(Idty = (.data$Perc / 100) * .data$Alen / .data$Qlen + pmax(0, .data$Qlen - .data$Alen) / 4) -> b.tbl
  
  res.tbl <- data.frame(Taxon.hat = rep("unclassified", length(sequence)),
                        Identity = rep(0, length(sequence)),
                        stringsAsFactors = F)
  idx <- match(b.tbl$Query, qry$Header)
  res.tbl$Taxon.hat[idx] <- b.tbl$Taxon
  res.tbl$Identity[idx] <- b.tbl$Idty
  ok <- file.remove(c(tfa, tft))
  return(res.tbl)
}



#' @name blastDbase16S
#' @title Building a BLAST database
#' 
#' @description Building a BLAST database for 16S based classification.
#' 
#' @param name The name of the database (text).
#' @param sequence A character vector with 16S sequence data.
#' @param taxon A character vector with taxon information.
#' 
#' @details This functions builds a database using the \code{makeblastdb} program of the
#' BLAST+ software \url{https://blast.ncbi.nlm.nih.gov/Blast.cgi?PAGE_TYPE=BlastDocs&DOC_TYPE=Download}.
#' Thus, this software must be available on the system when using this
#' function. If you type \code{system("makeblastdb -help")} in the Console window some meaningful
#' Help-text should be displayed.
#' 
#' This function is most typically used prior to \code{\link{blastClassify16S}} to set up the database
#' before searching and classifying. It can be seen as the 'training step' of a BLAST-based
#' classification procedure.
#' 
#' The \code{sequence} must be a vector of DNA-sequences (16S sequences). The \code{taxon} is a vector of the same 
#' length as \code{sequence}, containing the correpsonding taxon information. 
#' 
#' @return The database files are created, and the name of the database (\code{name}) is returned.
#' 
#' @author Lars Snipen.
#' 
#' @seealso \code{\link{blastClassify16S}}.
#' 
#' @examples # See examples for blastClassify16S.
#' 
#' @importFrom dplyr %>% mutate
#' @importFrom stringr str_to_upper str_replace_all str_c
#' @importFrom rlang .data
#' 
#' @export blastDbase16S
#' 
blastDbase16S <- function(name, sequence, taxon){
  tfa <- tempfile(fileext = ".fasta")
  data.frame(Header = str_c(taxon, 1:length(sequence), sep="_"),
             Sequence = str_to_upper(sequence),
             stringsAsFactors = F) %>% 
    mutate(Sequence = str_replace_all(.data$Sequence, "U", "T")) %>% 
    mutate(Sequence = str_replace_all(.data$Sequence, "[^ACGTNRYSWKMBDHV]", "N")) %>% 
    writeFasta(out.file = tfa)
  system(paste("makeblastdb -dbtype nucl -in", tfa, "-out", name))
  file.remove(tfa)
  return(name)
}


