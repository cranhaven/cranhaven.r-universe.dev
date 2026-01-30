#########1#########2#########3#########4#########5#########6#########7#########8
#' Calculate Text Distance (dense version)
#'
#' When two vectors are given, this calculates the text distance between them;
#' text distance is calculated as the proportion of unmatched frequencies, i.e.,
#' the number of unmatched frequencies divided by the total frequencies among
#' the two vectors. However, if neither vector has any values
#' at all, their distance equals the number provided in the zeroes argument,
#' which is .5 by default. When two matrices are given, the text distance
#' between corresponding columns is calculated.
#'
#' @param x A numeric vector or matrix
#' @param y A numeric vector or matrix of the same dimension as x
#' @param zeroes Text distance when both vectors are zero vectors; default is .5
#' @return When x and y are vectors, the text distance between them. For
#' example, between vectors (1,2,0) and (0,1,1), a total of 5 frequencies are 
#' present. However, position 1 matches nothing when it could have
#' matched 1 frequency, position 2 matches 1 frequency when it could have  
#' matched both positions, so 1 remains unmatched. Position 3 matches nothing  
#' when it could have matched 1. So we have 3 unmatched positions divided by 5 
#' frequencies, resulting in a text distance of 3/5=.6. If x and y are matrices, 
#' a vector with the text distance between corresponding columns is returned. So 
#' for two 4x2 matrices, a vector with two values is returned, one with the text 
#' distance between the first columns of the matrices, and the second one with 
#' the text distance between the second columns of the matrices. For large sets 
#' of data, it is recommended to use matrices as it is much more efficient than 
#' calculating column by column. 
#' @examples
#' #text distance between two vectors
#' textDist(c(1,2,0),c(0,1,1))
#' (M1=matrix(c(0,1,0,2,0,10,0,14),4))
#' (M2=matrix(c(12,0,8,0,1,3,1,2),4))
#' #text distance between corresponding columns of M1 and M2
#' textDist(M1,M2)
#' @export
################################################################################
textDist=function(x,y,zeroes=.5) {
  if (!inherits(zeroes,"numeric")) stop("zeroes must be numeric")
  if (zeroes[1]<0 | zeroes[1]>1) stop("zeroes must be between zero and 1")
  if (!inherits(x,c("integer","numeric","matrix","dgCMatrix"))) {
    stop("x must be a numeric vector or a matrix")
  }
  # Matrix / sparse matrix case
  if (inherits(x, c("matrix", "dgCMatrix"))) {
    if (!inherits(y, c("matrix", "dgCMatrix"))) stop("y must be a matrix")
    if (nrow(x) != nrow(y) || ncol(x) != ncol(y)) stop("x and y must have same dimensions")
    if (nrow(x) < 1) stop("x and y are empty")
    
    if (inherits(x, "dgCMatrix") && inherits(y, "dgCMatrix")) {
      out<-textDistM_sparse_cpp(x@i,x@p,x@x,y@i,y@p,y@x, zeroes)
      if (!is.null(colnames(x))) names(out) <- colnames(x)
      return(out)
    }
    #Dense matrices
    out<-textDistM_cpp(x, y, zeroes)
    if (!is.null(colnames(x))) names(out) <- colnames(x)
    return(out)
  }
  
  # Vector case
  if (!inherits(y,c("integer","numeric"))) stop("y must be a numeric vector")
  if (length(x)!=length(y)) stop("x and y must have equal length")
  if (length(x)<1) stop("x and y are empty")
  return(textDist_cpp(x,y,zeroes))
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Calculate Text Distance (sparse version)
#'
#' When two vectors are given, this calculates the text distance between them;
#' text distance is calculated as the proportion of unmatched frequencies, i.e.,
#' the number of unmatched frequencies divided by the total frequencies among
#' the two vectors. However, if neither vector has any values
#' at all, their distance equals the number provided in the zeroes argument,
#' which is .5 by default. When two matrices are given, the text distance
#' between corresponding columns is calculated.
#'
#' @param xi An integer vector with the row ids of the vector x
#' @param xx A numeric vector with the values of the nonzero elements of x
#' @param yi An integer vector with the row ids of the vector y
#' @param yx A numeric vector with the values of the nonzero elements of y
#' @param zeroes Text distance when both vectors are zero vectors; default is .5
#' @return the text distance between the vectors x and y. 
#' @examples
#' x=c(1,0,0,2,4,0,0,0,2,1,0,0)
#' y=c(2,0,0,0,2,0,0,1,0,3,1,0)
#' textDist_sparse(which(x!=0),x[x!=0],which(y!=0),y[y!=0])
#' @importFrom smallstuff isInt
#' @export
################################################################################
textDist_sparse=function(xi,xx,yi,yx,zeroes=.5) {
  if (!isInt(xi[1])) stop("xi must contain row numbers")
  if (!isInt(yi[1])) stop("yi must contain row numbers")
  if (!inherits(xx,c("integer","numeric"))) stop("xx must be a numeric vector")
  if (!inherits(yx,c("integer","numeric"))) stop("yx must be a numeric vector")
  if (!inherits(zeroes,"numeric")) stop("zeroes must be numeric")
  if (zeroes[1]<0 | zeroes[1]>1) stop("zeroes must be between zero and 1")
  if (length(xi)!=length(xx)) stop("xi and xx must have equal length")
  if (length(yi)!=length(yx)) stop("yi and yx must have equal length")
  return(textDist_sparse_cpp((xi-1),xx,(yi-1),yx,zeroes))
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Calculate Canberra Distance
#'
#' When two vectors are given, this calculates the Canberra distance between
#' them; This is calculated as the sum of the absolute difference between 
#' corresponding elements divided by the sum of their absolute values, for 
#' elements that are not both zero only.
#'
#' @param x A numeric vector
#' @param y A numeric vector of the same dimension as x
#' @return The Canberra distance between x and y. For example, between vectors
#' (1,2,0) and (0,1,1), for position 1 we have (1-0)/1, for position 2 we have
#' (2-1)/3, and for position 3 we have abs(0-1)/1, added together this results
#' in 2 1/3, or 2.33. Note that a text distance of zero indicates that the
#' two vectors are equal, while a text distance of 1 indicates that they have
#' no terms in common.
#' @examples
#' canberra(c(1,2,0),c(0,1,1))
#' @export
################################################################################
canberra=function(x,y) {
  if (!inherits(x,c("integer","numeric"))) stop("x must be a numeric vector")
  if (!inherits(y,c("integer","numeric"))) stop("y must be a numeric vector")
  a=abs(x-y)
  b=(abs(x)+abs(y))
  sum(a[b!=0]/b[b!=0])
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Calculate a Distance Matrix
#'
#' Calculate a distance matrix for a numeric matrix, where a distance function
#' is used to calculate the distance between all combinations of the columns
#' of the matrix \code{M}.
#'
#' @param M A numeric matrix
#' @param fn The name of a distance function, default is "textDist".
#' @param ... Additional arguments to be passed to the distance function 
#' @return The distance matrix with the distance between all combinations
#' of the columns of \code{M} according to the distance function in \code{fn}.
#' @examples
#' M=matrix(c(0,1,0,2,0,10,0,14,12,0,8,0,1,0,1,0),4)
#' colnames(M)=1:4;rownames(M)=c("A","B","C","D")
#' M
#' #Text distance matrix
#' distMatrix(M)
#' #Canberra distance matrix
#' distMatrix(M,"canberra")
#' @export
################################################################################
distMatrix=function(M,fn="textDist",...) {
  if (!inherits(M,"matrix")||!inherits(M[1,1],c("integer","numeric"))) 
    stop("M must be a numeric matrix")
  if (!exists(fn, mode='function')) stop("fn must have the name of a distance
                                         function")
  fun=get(fn)
  x=utils::combn(ncol(M),2,function(x) {fun(M[,x[1]],M[,x[2]],...)})
  attr(x,"Size")=ncol(M)
  if (is.null(colnames(M))) lab=as.character(1:ncol(M)) else lab=colnames(M)
  attr(x,"Labels")=lab
  attr(x,"Diag")=FALSE
  attr(x,"Upper")=FALSE
  attr(x,"method")=fn
  class(x)="dist"
  x
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Calculate a Text Distance Matrix
#'
#' Calculate a distance matrix for a numeric matrix, using the textDist
#' function. It is used to calculate the text distance between all combinations 
#' of the columns of the matrix \code{M}.
#'
#' @param M A numeric matrix
#' @param zeroes Text distance when both vectors are zero vectors; default is .5
#' @return The text distance matrix with the text distance between all
#' combinations of the columns of \code{M}. This will give the same result as 
#' the function distMatrix when run with its default distance function 
#' "textDist"; however, for large matrices textDistMatrix is much more 
#' efficient. In addition, for very large matrices distMatrix may not run, 
#' while textDistMatrix will.
#' @examples
#' M=matrix(c(0,1,0,2,0,10,0,14,12,0,8,0,1,0,1,0),4)
#' colnames(M)=1:4;rownames(M)=c("A","B","C","D")
#' M
#' #Text distance matrix
#' textDistMatrix(M)
#' @import Matrix
#' @export
################################################################################
textDistMatrix=function(M,zeroes=.5) {
  if (!inherits(M,c("matrix","phraseDoc","dgCMatrix"))) stop("M must be a matrix or PhraseDoc")
  if (!inherits(zeroes,"numeric")) stop("zeroes must be numeric")
  if (zeroes[1]<0 | zeroes[1]>1) stop("zeroes must be between zero and 1")
  
  if (inherits(M,"matrix")) {
    if (!smallstuff::isInt(M[1,1])) stop("M must be a text matrix")
    M=Matrix::Matrix(M,sparse=TRUE) #Make it sparse
  }
  
  if (inherits(M,"dgCMatrix")) { #Sparse matrix
    li<-textDistMatrix_cpp(M@i ,M@p, M@x, zeroes)
    out=li$out
    attr(out,"comb")=li$comb + 1L
    
    if (is.null(colnames(M))) lab=as.character(1:ncol(M)) else lab=colnames(M)
    attr(out,"Size")=ncol(M)
    
  } else { # A phraseDoc
    dta=data.table::data.table(phrase=M$phrase,doc=M$doc)
    y=dta[,.N,by=c("doc", "phrase")]
    data.table::setorder(y,doc,phrase)
    
    # Compute zero-based Mp directly
    ncol=length(M$docs)
    Mp <- integer(ncol + 1L)
    Mp[-1] <- cumsum(tabulate(y$doc, nbins = ncol))
    
    li<-textDistMatrix_cpp(y$phrase-1,Mp,y$N,zeroes)
    out=li$out
    attr(out,"comb")=li$comb + 1L
    
    lab=as.character(M$docs)
    attr(out,"Size")=length(M$docs)
  }
  
  #create a dist object from out
  attr(out,"Labels")=lab
  attr(out,"Diag")=FALSE
  attr(out,"Upper")=FALSE
  attr(out,"method")="textDist"
  class(out)="dist"
  out
}
