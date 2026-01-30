#########1#########2#########3#########4#########5#########6#########7#########8
#R Functions dealing with a phraseDoc
################################################################################
# External Functions
################################################################################
library(smallstuff)
################################################################################
# Internal Functions
################################################################################
#########1#########2#########3#########4#########5#########6#########7#########8
#' Words that Principal Phrases do not Start with
#'
#' Create a vector with words that principal phrases should not start with.
#'
#' @return vector with words
#' @examples
#' stopStartWords()
#' @export
################################################################################
stopStartWords<-function() {
  myStopwords <- c(tm::stopwords('english'), letters, "0","1","2","3","4","5",
                   "6","7","8","9", "=","+/-",">/=","<",">","and/or", "-","+",
                   "_","#","%","&","*")
  myStopwords[-which(myStopwords=="not"|myStopwords=="no")]
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Words that Principal Phrases do not End with
#'
#' Create a vector with words that principal phrases should not end with.
#'
#' @return vector with words
#' @examples
#' stopEndWords()
#' @export
################################################################################
stopEndWords<-function() {
  myStopwords <- c(tm::stopwords('english'), letters, "0","1","2","3","4","5",
                   "6","7","8","9", "=","+/-",">/=","<",">","and/or", "-","+",
                   "_","#","$","&","*")
  myStopwords
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Phrases that are not Principal Phrases
#'
#' Create a vector with phrases that are not principal phrases.
#'
#' @return vector with phrases
#' @examples
#' stopPhrases()
#' @export
################################################################################
stopPhrases<-function() {""}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Print a phraseDoc Object
#'
#' @param x Object of type phraseDoc
#' @param ... Additional arguments
#' @examples
#' tst=c("This is a test text",
#'       "This is a test text 2",
#'       "This is another test text",
#'       "This is another test text 2",
#'       "This girl will test text that man",
#'       "This boy will test text that man")
#' (pd=phraseDoc(tst))
#' @method print phraseDoc
#' @export
################################################################################
print.phraseDoc <- function(x,...) {
  writeLines(paste("<<",class(x),">>"))
  writeLines(paste(length(x$phrases$phrase),"principal phrases"))
  writeLines(paste(length(x$docs),"documents"))
  invisible(x)
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Convert a phraseDoc Object to a Matrix
#'
#' @param x A phraseDoc object.
#' @param ids A logical value with TRUE (default) to use ids (if available), 
#' FALSE to use indices
#' @param sparse A logical value indicates whether a sparse matrix should be
#' returned (default FALSE)
#' @param ... Additional arguments
#' @return A matrix with phrases as rows, texts as columns, and elements
#' containing the number of times the phrase occurs in the text
#' @examples
#' tst=c("This is a test text",
#'       "This is a test text 2",
#'       "This is another test text",
#'       "This is another test text 2",
#'       "This girl will test text that man",
#'       "This boy will test text that man")
#' pd=phraseDoc(tst)
#' as.matrix(pd)
#' @import data.table
#' @method as.matrix phraseDoc
#' @export
################################################################################
as.matrix.phraseDoc <- function(x,ids=TRUE,sparse=FALSE,...) {
  if (!inherits(ids,"logical")) stop("ids must be a logical value")
  if (!inherits(sparse,"logical")) stop("sparse must be a logical value")
  dta=data.table::data.table(phrase=x$phrase,doc=x$doc)
  y=dta[,.N,by=c("doc", "phrase")]
  if (ids) docs=x$docs else docs=1:length(x$docs)
  sm=Matrix::sparseMatrix(y$phrase,y$doc,x=y$N,
                          dims=c(length(x$phrases$phrase),length(x$docs)),
                          dimnames=list(phrases=x$phrases$phrase,
                                        docs=as.character(docs)))
  if (sparse) sm else as.matrix(sm)
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Remove Phrases from phraseDoc Object
#'
#' Remove a set of phrases from a phraseDoc object.
#'
#' @param pd A phraseDoc object.
#' @param phrs A set of phrases.
#' @return A phraseDoc object with the phrases in \code{phrs} removed.
#' @examples
#' tst=c("This is a test text",
#'       "This is a test text 2",
#'       "This is another test text",
#'       "This is another test text 2",
#'       "This girl will test text that man",
#'       "This boy will test text that man")
#' pd=phraseDoc(tst)
#' removePhrases(pd, c("test text","another test text"))
#' @import data.table
#' @export
################################################################################
removePhrases=function(pd,phrs) {
  #Error checking of the input arguments
  if (!inherits(pd,"phraseDoc")) stop("pd must be a phraseDoc")
  if (!inherits(phrs,"character")) stop("phrs must be a character vector")
  phrs=trimws(phrs)
  x=removePhrases_cpp(pd$phrase,pd$doc,pd$block,pd$pos,pd$phrases$phrase,
                      pd$phrases$freq, pd$phrases$pwrds,phrs)
  
  x$docs=pd$docs
  class(x) <- "phraseDoc"
  x
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Display Frequent Principal Phrases
#'
#' Display the most frequent principal phrases in a phraseDoc object.
#'
#' @param pd A phraseDoc object.
#' @param n Number of principal phrases to display.
#' @return A vector with the \code{n} most frequent principal phrases and their
#'         frequencies.
#' @examples
#' tst=c("This is a test text",
#'       "This is a test text 2",
#'       "This is another test text",
#'       "This is another test text 2",
#'       "This girl will test text that man",
#'       "This boy will test text that man")
#' pd=phraseDoc(tst)
#' freqPhrases(pd, 2)
#' @export
################################################################################
freqPhrases=function(pd,n=10) {
  if (!inherits(pd,"phraseDoc")) stop("pd must be a phraseDoc")
  if (!isInt(n)||n<1) stop("n must be an integer greater than or equal to 1")
  n=min(n,length(pd$phrases$freq))
  x=tail(pd$phrases$freq[order(pd$phrases$freq)],n)
  names(x)=tail(pd$phrases$phrase[order(pd$phrases$freq)],n)
  m=as.matrix(rev(x))
  colnames(m)="frequency"
  m
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Display Frequency Matrix for Phrases
#'
#' Display a frequency matrix containing all the documents that contain any of
#' the phrases in phrs and the number of times they occur in that document.
#'
#' @param pd A phraseDoc object.
#' @param phrs A set of phrases.
#' @param ids A logical value with TRUE (default) to return ids (if available), 
#' FALSE to return indices.
#' @return A matrix with the documents and # of occurrences for the phrases in
#' phrs.
#' @examples
#' tst=c("This is a test text",
#'       "This is a test text 2",
#'       "This is another test text",
#'       "This is another test text 2",
#'       "This girl will test text that man",
#'       "This boy will test text that man")
#' pd=phraseDoc(tst)
#' getDocs(pd, c("test text","another test text"))
#' @import data.table
#' @export
################################################################################
getDocs=function(pd,phrs,ids=TRUE) {
  #Error checking of the input arguments
  if (!inherits(pd,"phraseDoc")) stop("pd must be a phraseDoc")
  if (!inherits(phrs,"character")) stop("phrs must be a character vector")
  if (!inherits(ids,"logical")) stop("ids must be a logical value")
  phrs=trimws(phrs)
  idx=which(pd$phrases$phrase %in% phrs)
  if (length(idx)==0) stop("No such phrases in the phraseDoc")
  idx2=which(pd$phrase %in% idx)
  dta=data.table(phrase=pd$phrase[idx2],doc=pd$doc[idx2])
  x=dta[,.N,keyby=c("doc", "phrase")]
  docs=factor(x$doc)
  phrs=factor(x$phrase)
  p=length(levels(docs))
  n=length(levels(phrs))
  i=as.numeric(phrs)
  j=as.numeric(docs)
  m=rep(0,n*p)
  m[((i-1)*p)+j]=x$N
  M=matrix(m,n,byrow=TRUE)
  
  if (ids) colnames(M)=pd$docs[as.numeric(levels(docs))] else {
    colnames(M)=levels(docs)
  }
  rownames(M)=pd$phrases$phrase[as.numeric(levels(phrs))]
  M
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Display Frequency Matrix for Documents
#'
#' Display a frequency matrix containing all the documents for which the indices
#' are given in docs with their principal phrases and the number of times they
#' occur in each document.
#'
#' @param pd A phraseDoc object
#' @param doc An integer vector containing indices of documents, or a character
#' vector containing the ids of documents (column names)
#' @param ids A logical value with TRUE (default) to return ids (if available), 
#' FALSE to return indices
#' @return A matrix with the documents and # of occurrences of principal phrases
#' for the documents in \code{docs}
#' @examples
#' tst=c("This is a test text",
#'       "This is a test text 2",
#'       "This is another test text",
#'       "This is another test text 2",
#'       "This girl will test text that man",
#'       "This boy will test text that man")
#' pd=phraseDoc(tst)
#' getPhrases(pd, c(1,3))
#' @import data.table
#' @importFrom smallstuff isInt
#' @export
################################################################################
getPhrases <- function(pd,doc,ids=TRUE) {
  if (!inherits(pd,"phraseDoc")) stop("pd must be a phraseDoc")
  if (inherits(doc,"character")) {
    doc=which(pd$docs %in% doc)
  } else {
    #if we're not dealing with an integer vector, then all entries must be integers
    if (!inherits(doc,"integer") && sum(isInt(doc))!=length(doc))
      stop("doc must be a vector of indices or document names")
  }
  if (length(doc)==0) stop("No such documents found")
  if (!inherits(ids,"logical")) stop("ids must be a logical value")
  idx = which(pd$doc %in% doc)
  if (length(idx) == 0) stop("No such documents in the phraseDoc")
  dta = data.table(phrase = pd$phrase[idx], doc = pd$doc[idx])
  x=dta[,.N,keyby=c("doc", "phrase")]
  docs=factor(x$doc)
  phrs=factor(x$phrase)
  p=length(levels(docs))
  n=length(levels(phrs))
  i=as.numeric(phrs)
  j=as.numeric(docs)
  m=rep(0,n*p)
  m[((i-1)*p)+j]=x$N
  M=matrix(m,n,byrow=TRUE)
  if (ids) colnames(M)=pd$docs[as.numeric(levels(docs))] else {
    colnames(M)=levels(docs)
  }
  rownames(M)=pd$phrases$phrase[as.numeric(levels(phrs))]
  M
}
