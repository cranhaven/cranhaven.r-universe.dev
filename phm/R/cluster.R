#########1#########2#########3#########4#########5#########6#########7#########8
#' Cluster a Term-Document Matrix
#'
#' Combine documents (columns) into k clusters that have texts that are most
#' similar based on their text distance. Documents with no terms are assigned
#' to the last cluster.
#'
#' @param M A term document matrix with terms on the rows and documents on 
#' the columns.
#' @param k A positive integer with the number of clusters needed
#' @param mx Maximum number of times to iterate (default 100)
#' @param md Maximum number of documents to use for the initial setup (default 
#' 10*\code{k}).
#' @param silent TRUE if you do not want progress messages.
#' @return A textcluster object with three items; cluster, centroids, and size,
#' where cluster contains a vector indicating for each column in \code{M} what
#' cluster they have been assigned to, centroids contains a matrix with each
#' column the centroid of a cluster, and size a named vector with the size of
#' each cluster.
#' @examples
#' M=matrix(c(0,1,0,2,0,10,0,14,12,0,8,0,1,0,1,0),4)
#' colnames(M)=1:4;rownames(M)=c("A","B","C","D")
#' textCluster(M,2)
#' @import data.table
#' @importFrom smallstuff isInt
#' @export
################################################################################
textCluster=function(M,k,mx=100,md=10*k,silent=TRUE) {
  if (!inherits(M,c("matrix","phraseDoc","dgCMatrix"))) 
    stop("M must be a matrix or PhraseDoc")
  if (!isInt(k)||k<2) stop("k must be an integer greater than 1")
  if (!isInt(mx)||mx<1) stop("mx must be a positive integer")
  if (!isInt(md)||md<k) stop("md must be an integer greater than k")
  if (!inherits(silent,"logical")) stop("silent must be logical")
  
  if (inherits(M,"matrix")) {
    if (!isInt(M[1,1])) stop("M must be a text matrix")
    M=Matrix::Matrix(M,sparse=TRUE) #Make it sparse
  }
  if (inherits(M,"dgCMatrix")) { #Sparse matrix
    
    # Check if there are enough non-empty documents for clustering
    nc = length(M@p)-1; nuc = sum(diff(M@p) > 0)
    min_docs <- if (nc == nuc) 2*k else 2*(k-1)
    if (nuc < min_docs) stop("Too many clusters: not enough documents with content")
    
    x<-textCluster_cpp(M@i,M@x,M@p,k,mx,md,nrow(M),silent)
    if (!is.null(rownames(M))) {
      rownames(x$centroids) <- rownames(M)
    } else rownames(x$centroids) <- as.character(seq_len(nrow(M)))
    if (!is.null(colnames(M))) {
      names(x$cluster) <- colnames(M)
    } else names(x$cluster) <- as.character(seq_len(length(x$cluster)))
    
  } else { # A phraseDoc
    dta=data.table::data.table(phrase=M$phrase,doc=M$doc)
    y=dta[,.N,by=c("doc", "phrase")]
    data.table::setorder(y,doc,phrase)
    
    # Compute zero-based Mp directly
    ncol=length(M$docs)
    Mp <- integer(ncol + 1L)
    Mp[-1] <- cumsum(tabulate(y$doc, nbins = ncol))
    
    # Check if there are enough non-empty documents for clustering
    nc = length(M$docs); nuc = sum(diff(Mp) > 0)
    min_docs <- if (nc == nuc) 2*k else 2*(k-1)
    if (nuc < min_docs) stop("Too many clusters: not enough documents with content")
    
    nr=length(M$phrases$phrase)
    x<-textCluster_cpp(y$phrase-1,y$N,Mp,k,mx,md,nr,silent)
    rownames(x$centroids) <- M$phrases$phrase
    names(x$cluster) <- M$docs
  }
  
  x$cluster <- x$cluster + 1L
  colnames(x$centroids) <- as.character(seq_len(ncol(x$centroids)))
  names(x$size) <- as.character(seq_len(length(x$size)))
  class(x)="textCluster"
  x
}

# Tell R CMD check that these are global variables
utils::globalVariables(c("doc", "phrase"))

#########1#########2#########3#########4#########5#########6#########7#########8
#' Show Cluster Contents
#'
#' Show all documents and their non-zero terms in a cluster, with the terms
#' first ordered by highest number of documents the term appears in, then total
#' frequency.
#'
#' @param tdm A term frequency matrix.
#' @param clust A vector indicating for each column in \code{tdm} what cluster
#' they belong to
#' @param cl Cluster number
#' @param n Integer showing the maximum number of terms to be returned (default 10)
#' @return A matrix with document names of \code{tdm} on the columns and terms
#' on the rows for all columns in the cluster, where terms that appear in the
#' most documents (columns), and within that have the highest frequency in the
#' cluster, are shown first. Two columns are added at the end of the matrix
#' with the the number of documents each term appears in and its total frequency
#' in the cluster. The number of terms displayed equals the number in \code{n}, 
#' or less if there are less terms in the cluster.
#' If there are no terms at all in the cluster, a list is output with the items
#' docs and note, where docs is a vector with all document names of documents in 
#' the cluster, and the note stating that the cluster has no terms. 
#' @examples
#' M=matrix(c(0,1,0,2,0,10,0,14,12,0,8,0,1,0,1,0),4)
#' colnames(M)=1:4;rownames(M)=c("A","B","C","D")
#' tc=textCluster(M,2)
#' showCluster(M,tc$cluster,1)
#' @export
################################################################################
showCluster <- function(tdm,clust,cl,n=10L) {
  if (!inherits(tdm,"matrix")||!inherits(tdm[1,1],c("integer","numeric"))) 
    stop("tdm must be a term-document matrix")
  if (length(clust)!=ncol(tdm)) stop("clust must contain the cluster for each
                                     column of tdm")
  if (!(cl %in% clust)) stop("cl must be a cluster in clust")
  if (!smallstuff::isInt(n)||n<1) stop("n must be an integer greater than 0")
  M=tdm[,clust==cl,drop=F]
  frq=rowSums(M)
  frq2=apply(M,1,function(x) {sum(x!=0)})
  n=min(n,sum(frq!=0))
  if (n==0) return(list(docs=names(clust[clust==cl]),
                        note="Documents have no terms"))
  idx=order(frq2,frq,decreasing=TRUE)[1:n]
  cbind(M[idx,,drop=F],nDocs=frq2[idx],totFreq=frq[idx])
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Print a textCluster Object
#'
#' @param x Object of type textCluster
#' @param ... Additional arguments
#' @return The total number of clusters and total number of documents are 
#' printed. There is no return value.
#' @examples
#' M=matrix(c(0,1,0,2,0,10,0,14,12,0,8,0,1,0,1,0),4)
#' colnames(M)=1:4;rownames(M)=c("A","B","C","D")
#' tc=textCluster(M,2)
#' tc
#' @method print textCluster
#' @export
################################################################################
print.textCluster <- function(x,...) {
  writeLines(paste("<<",class(x),">>"))
  writeLines(paste(length(x$size),"clusters"))
  writeLines(paste(length(x$cluster),"documents"))
}

