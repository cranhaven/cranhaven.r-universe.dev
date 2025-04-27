#' @title Remove terms from a Document-Term-Matrix and documents with no terms based on the term frequency inverse document frequency
#' @description Remove terms from a Document-Term-Matrix and documents with no terms based on the term frequency inverse document frequency.
#' Either giving in the maximum number of terms (argument \code{top}), the tfidf cutoff (argument \code{cutoff})
#' or a quantile (argument \code{prob})
#' @param dtm an object class "dgCMatrix"
#' @param top integer with the number of terms which should be kept as defined by the highest mean tfidf
#' @param cutoff numeric cutoff value to keep only terms in \code{dtm} where the tfidf obtained by \code{dtmtfidf} is higher than this value
#' @param prob numeric quantile indicating to keep only terms in \code{dtm} where the tfidf obtained by \code{dtmtfidf} is higher than
#' the \code{prob} percent quantile
#' @param remove_emptydocs logical indicating to remove documents containing no more terms after the term removal is executed. Defaults to \code{TRUE}.
#' @return a sparse Matrix as returned by \code{sparseMatrix}
#' where terms with high tfidf are kept and documents without any remaining terms are removed
#' @export
dtmremovetfidf <- function(dtm, top, cutoff, prob, remove_emptydocs = TRUE){
  tfidf <- dtmtfidf(dtm)
  if(!missing(top)){
    terms <- utils::head(sort(tfidf, decreasing = TRUE), n = top)
    terms <- names(terms)
  }else if(!missing(cutoff)){
    terms <- tfidf[tfidf >= cutoff]
    terms <- names(terms)
  }else if(!missing(prob)){
    cutoff <- stats::quantile(tfidf, prob)
    terms <- tfidf[tfidf >= cutoff]
    terms <- names(terms)
  }else{
    stop("either provide top, cutoff or prob")
  }
  if(length(terms) == 0){
    stop("no terms found in reducing based on tfidf, consider increasing top or decreasing cutoff/prob")
  }
  dtm <- dtm[, which(colnames(dtm) %in% terms), drop = FALSE]
  if(remove_emptydocs){
    dtm <- dtm[Matrix::rowSums(dtm) > 0, , drop = FALSE]
  }
  dtm
}