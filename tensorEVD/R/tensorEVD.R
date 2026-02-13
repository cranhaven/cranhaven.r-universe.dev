
# EVD1 <- EVD2 <- NULL; d.min = .Machine$double.eps; verbose= TRUE
tensorEVD <- function(K1, K2, ID1, ID2,
                      alpha = 1.0, EVD1 = NULL, EVD2 = NULL,
                      d.min = .Machine$double.eps,
                      make.dimnames = FALSE,
                      verbose = FALSE)
{
    isEigen1 <- isEigen2 <- FALSE

    stopifnot(length(ID1) == length(ID2))
    n <- length(ID1)

    flag1 <- ifelse(is.list(EVD1),all(c("values","vectors")%in%names(EVD1))&(length(EVD1)==2),FALSE)
    flag2 <- ifelse(is.list(EVD2),all(c("values","vectors")%in%names(EVD2))&(length(EVD2)==2),FALSE)

    # For K1
    if(missing(K1)){
      if(!flag1){
        stop("A list type object 'EVD1' must be provided when 'K1' is missing\n",
             "  This should contain 'values' and 'vectors' as per the 'eigen' function")
      }
      dm1 <- dim(EVD1$vectors)
      if(length(dm1) != 2L){
         stop("'EVD1$vectors' must be a matrix with eigenvectors in columns")
      }
      if(dm1[2] != length(EVD1$values)){
        stop("Number of columns of 'EVD1$vectors' must be the same as length of 'EVD1$values'")
      }
      index1 <- match_ID(EVD1$vectors, ID1, check=FALSE)
      isEigen1 <- TRUE

    }else{
      index1 <- match_ID(K1, ID1, check=TRUE)

      EVD1 <- eigen(K1, symmetric=TRUE)
      if(has_names(K1)){
        rownames(EVD1$vectors) <- rownames(K1)
      }
      dm1 <- dim(EVD1$vectors)
    }

    # For K2
    if(missing(K2)){
      if(!flag2){
        stop("A list type object 'EVD2' must be provided when 'K2' is missing\n",
             "  This should contain 'values' and 'vectors' as per the 'eigen' function")
      }
      dm2 <- dim(EVD2$vectors)
      if(length(dm2) != 2L){
         stop("'EVD2$vectors' must be a matrix with eigenvectors in columns")
      }
      if(dm2[2] != length(EVD2$values)){
        stop("Number of columns of 'EVD2$vectors' must be the same as length of 'EVD2$values'")
      }
      index2 <- match_ID(EVD2$vectors, ID2, check=FALSE)
      isEigen2 <- TRUE

    }else{
      index2 <- match_ID(K2, ID2, check=TRUE)

      EVD2 <- eigen(K2, symmetric=TRUE)
      if(has_names(K2)){
        rownames(EVD2$vectors) <- rownames(K2)
      }
      dm2 <- dim(EVD2$vectors)
    }

    if(is.null(index1)){
      stop("'ID1' could not be matched to ",ifelse(isEigen1,"rows of 'EVD1'","'K1'"))
    }
    if(is.null(index2)){
      stop("'ID2' could not be matched to ",ifelse(isEigen2,"rows of 'EVD2'","'K2'"))
    }

    #dyn.load("c_tensor_evd.so")
    return(.Call('R_tensor_evd', n, dm1[1], dm1[2], dm2[1], dm2[2],
                                 EVD1$values, EVD1$vectors, EVD2$values, EVD2$vectors,
                                 as.numeric(d.min), index1, index2,
                                 as.numeric(alpha), make.dimnames, verbose))
    #dyn.unload("c_tensor_evd.so")
}
