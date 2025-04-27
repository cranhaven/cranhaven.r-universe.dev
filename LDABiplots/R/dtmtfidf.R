#' @title Term Frequency - Inverse Document Frequency calculation
#' @description Term Frequency - Inverse Document Frequency calculation.
#' Averaged by each term.
#' @param dtm an object class "dgCMatrix"
#' @return a vector with tfidf values, one for each term in the \code{dtm} matrix
#' @export
dtmtfidf <- function(dtm){
  ## number of times word appears / number of words in document, on average if non-missing
  ## times log2(# documents / #documentsXwords)
  terms <- colnames(dtm)
  m <- Matrix::summary(dtm)
  term_tfidf <- tapply(m$x/Matrix::rowSums(dtm)[m$i], m$j, mean) *
    log2(nrow(dtm)/Matrix::colSums(dtm > 0))
  names(term_tfidf) <- terms
  term_tfidf
}