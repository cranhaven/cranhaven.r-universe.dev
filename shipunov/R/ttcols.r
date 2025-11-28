Ttcols <- function(text, missed=NA, ...) {
 ## empty strings are exceptions in strsplit():
 ## remember their position and convert them into NAs
 emptypos <- which(text == "")
 text[emptypos] <- NA
 ## strsplit(...) each element
 lst <- strsplit(text, ...)
 ## rbind() warnings suppressed because in this particular case, they are not useful at all
 suppressWarnings(mat <- do.call(rbind, lst))
 ## true length of each element of 'lst'
 len <- sapply(lst, length)
 ## lengths of true (from 'lst') and added (from recycling) elements in 'mat'
 ## added are always 'ncol(mat)-len'
 ## to interleave them, use c(rbind(...)) to read matrix into vector column-wise
 interleaved <- c(rbind(len, ncol(mat)-len))
 ## repeat TRUE and FALSE as many times as rows of 'mat'
 ## and then repeat each of them using interleaved lengths
 ind <- rep(rep(c(TRUE, FALSE), nrow(mat)), interleaved)
 ## make indexing matrix made of logical elements
 indm <- matrix(ind, nrow=nrow(mat), byrow=TRUE)
 ## index and replace indexed with 'missed'
 mat[!indm] <- missed
 ## bring empty strings (if any) back
 mat[emptypos, 1] <- ""
 mat
}
