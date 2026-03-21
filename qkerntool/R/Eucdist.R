## Computes the Euclidean(square Euclidean) distance matrix

setGeneric("Eucdist",function(x, y = NULL, ...) standardGeneric("Eucdist"))
setMethod("Eucdist", signature(x = "matrix"),
function(x, y = NULL, sEuclidean = FALSE)
{

     if(is(x,"vector"))
       x <- as.matrix(x)
     if(is(y,"vector"))
       y <- as.matrix(y)
     if(!is(y,"matrix")&&!is.null(y)) stop("y must be a matrix or a vector")

     if (is(x,"matrix") && is.null(y)){
       n <- dim(x)[1]
       res <- apply(x, 1, crossprod)
       res <- matrix(res, nrow=n, ncol=n)
       res <- res + t(res) - 2*tcrossprod(x)
       diag(res) <- 0
     }
     if (is(x,"matrix") && is(y,"matrix")){
       if (!(dim(x)[2]==dim(y)[2])) stop("matrixes must have the same number of columns")
       n <- dim(x)[1]
       m <- dim(y)[1]
       dotx <- apply(x, 1, crossprod)
       doty <- apply(y, 1, crossprod)
       dotx <- matrix(dotx, nrow=n, ncol=m)
       doty <- matrix(doty, nrow=m, ncol=n)
       res <- dotx + t(doty) - 2*tcrossprod(x,y)
       res[res < 0] <- 0
     }
     if(sEuclidean){
       res=sqrt(res)
     }
       return(res)
})







