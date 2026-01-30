#########1#########2#########3#########4#########5#########6#########7#########8
#' Find Informative Documents in a Corpus
#'
#' Find the documents in a corpus that have the most high frequency
#' phrases and return a corpus with just those documents
#'
#' @param co A corpus with documents
#' @param num Integer with the number of documents to return
#' @param n Integer with the number of high frequency phrases to use
#' @param pd phraseDoc object for the corpus in \code{co}; if NULL, a
#' phraseDoc will be created for it.
#' @return A corpus with the \code{num} documents that have the most
#' high frequency phrases, in order of the number of high frequency
#' phrases. The corpus returned will have the meta field oldIdx set
#' to the index of the document in the original corpus, and the meta
#' field hfPhrases to the number of high frequency phrases it contains.
#' @examples
#' v1=c("Here is some text to test phrase mining","phrase mining is fun",
#'   "Some text is better than no text","No text, no phrase mining")
#' co=tm::VCorpus(tm::VectorSource(v1))
#' pd=phraseDoc(co,min.freq=2)
#' bestDocs(co,2,2,pd)
#' @import tm
#' @export
################################################################################
bestDocs<-function(co,num=3L,n=10L,pd=NULL) {
  if (!inherits(co,"Corpus")) stop("co must be a corpus")
  if (!isInt(num)|num<1) stop("num must be an integer >= 1")
  if (!isInt(n)|n<1) stop("n must be an integer >= 1")
  if (!is.null(pd)&!inherits(pd,"phraseDoc")) stop("pd must be a phraseDoc")
  if (is.null(pd)) pd=phraseDoc(co, min.freq=3,silent=TRUE)
  mostf=freqPhrases(pd,n)
  pdm2=getDocs(pd,rownames(mostf),F)
  colcounts=sort(apply(pdm2,2,function(x) sum(x>0)),decreasing=T)
  idx=as.integer(names(head(colcounts,num)))
  co2=co[idx]
  for (i in 1:num) {
    co2[[i]]$meta$oldIdx=idx[i]
    co2[[i]]$meta$hfPhrases=colcounts[i]
  }
  co2
}
