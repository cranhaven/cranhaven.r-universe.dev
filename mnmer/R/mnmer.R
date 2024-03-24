#' mnmer
#' @aliases cmnmer mnmer MNmer
#' @description Generates the feature matrix using conditional probability. As default, all sequences with N+IUPAC content higher 10% than are removed.
#'
#' @param seqs DNAStringSet object \code{seqs}
#' @param m Int value of m \code{m}
#' @param n Int value of n \code{n}
#'
#' @return Outputs a dataframe 
#'
#' @keywords (m,n)-mer k-mers featureMatrix
#'
#' @export
#' 
#' @examples
#' dir <-system.file("extdata", package="mnmer")
#' human <- readDNAStringSet(file.path(dir, "human_vir.fasta"))
#' human_02mer <- mnmer(human,2,0)
#' 
mnmer <- function (seqs, m, n) 
{
    m <- as.integer(m)
    n <- as.integer(n)

    if (typeof(m) !="integer" || typeof(n)!="integer" || m == 0)
        stop("ERROR: parameters m and n must be integer types and m must be different zero.")

    seqid <- c()
    tab <- data.frame()
    pb = txtProgressBar(min = 0, max = length(seqs), initial = 0) 

    for (i in 1:length(seqs)){
        seqid <- c(seqid,names(seqs)[i])
        sqs <- as.character(seqs[[i]])
        ctab <- .Call("cmnmer", sqs, m, n)
        ts <- read.csv(text = ctab, header = TRUE)
        tab <- rbind (tab,ts)
        setTxtProgressBar(pb,i)
    }

    close(pb)
    
    tab <- cbind (seqid,tab)

    return(tab)
}