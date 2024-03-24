#' readNumFASTA
#' @aliases readrandFASTA readNum readFASTA
#' @description Load data in to the R system. 
#'
#' @param FASTAfile Path to a multifasta file \code{FASTAfile}
#' @param size Number of sequences to be loaded \code{size}
#' @param rand Sequences choose mode random or not. TRUE or FALSE \code{rand}
#' @param pni Cutoff percentage for maximum of non-ACTG bases in the sequences \code{pni}
#'
#' @return DNAStringSet object
#'
#' @keywords sequences random non-ACTG 
#'
#' @export
#' 
#' @examples
#'dir <-system.file("extdata", package="mnmer")
#'human <-readNumFASTA((file.path(dir, "human_vir.fasta")), 10, TRUE, 0.50)
#' 
readNumFASTA <- function (FASTAfile, size=0, rand=FALSE, pni=0.20)
{
    if (!file.exists(FASTAfile)) 
        stop(paste0("File: ", FASTAfile, " does not found!"))
    if (!rand) {
        utils::globalVariables(c("DNAStringSet"))
        seqs <- readDNAStringSet(FASTAfile)
        seqid <- c()
        tab <- data.frame()
        maxsize <- ifelse(size == 0, length(seqs), size)
        dsqs <- DNAStringSet()
        for (i in 1:maxsize) {
            ni <- alphabetFrequency(seqs[i], baseOnly = TRUE, 
                                    as.prob = TRUE)
            if (ni[5] < pni) {
                dsqs <- c(dsqs, seqs[i])
            }
            else {
                message("Warning: ", names(seqs)[i], " has a proportion of N + IUPAC bases = ", 
                        ni[5])
            }
        }
        return(dsqs)
    }

    fasstr <- .Call("readrandFASTA", FASTAfile, size, pni)

    if (fasstr == "101")
        stop ("ERROR: Number of random dataset size is greater than the FASTA file dataset!")

    fastab <- read.table(text = fasstr, header = FALSE, sep = "\t")
    dsqs <- DNAStringSet(fastab[, 2])
    names(dsqs) <- fastab[, 1]
    return(dsqs)
}