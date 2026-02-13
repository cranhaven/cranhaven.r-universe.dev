
#====================================================================
# Hadamard product between matrices A and B
#====================================================================
# IDcolA = NULL; IDcolB = NULL; drop <- TRUE; make.dimnames <- inplace <- FALSE
Sum <- function(a = 1, A, b = 1, B, IDrowA, IDrowB,
                IDcolA = NULL, IDcolB = NULL,
                make.dimnames = FALSE,
                drop = TRUE, inplace = FALSE)
{

  if((length(dim(A)) != 2L)){
    A <- as.matrix(A, ncol=1L)
  }
  if((length(dim(B)) != 2L)){
    B <- as.matrix(B, ncol=1L)
  }
  dmA <- dim(A)
  dmB <- dim(B)

  if(!is.scalar(a)){
    stop("'a' must be a scalar")
  }

  if(!is.scalar(b)){
    stop("'b' must be a scalar")
  }

  # Match rows IDs
  fixedA <- fixedB <- c(FALSE,FALSE)
  if(missing(IDrowA)){
    irowA <- seq(0,dmA[1]-1)   # zero-based indices
    fixedA[1] <- TRUE
  }else{
    irowA <- match_ID(A, IDrowA, MARGIN=1, check=FALSE)
    if(is.null(irowA)){
      stop("'IDrowA' could not be matched to rows of 'A'")
    }
  }

  if(missing(IDrowB)){
    irowB <- seq(0,dmB[1]-1)   # zero-based indices
    fixedB[1] <- TRUE
  }else{
    irowB <- match_ID(B, IDrowB, MARGIN=1, check=FALSE)
    if(is.null(irowB)){
      stop("'IDrowB' could not be matched to rows of 'B'")
    }
  }

  # Checkpoint for rows IDs
  if(length(irowA) != length(irowB)){
    stop("No compatibility. Provide either matrices with equal number of rows\n",
         "  or 'IDrowA' and/or 'IDrowB' vectors of the same length")
  }

  # Match columns IDs
  icolA <- icolB <- NULL
  if(is.null(IDcolA)){
    if(ifelse(dmA[1]==dmA[2],all(rownames(A)==colnames(A)),FALSE)){
      icolA <- irowA
      fixedA[2] <- fixedA[1]
    }else{
      icolA <- seq(0,dmA[2]-1)   # zero-based indices
      fixedA[2] <- TRUE
    }
  }else{
    icolA <- match_ID(A, IDcolA, MARGIN=2, check=FALSE)
    if(is.null(icolA)){
      stop("'IDcolA' could not be matched to columns of 'A'")
    }
  }

  if(is.null(IDcolB)){
    if(ifelse(dmB[1]==dmB[2],all(rownames(B)==colnames(B)),FALSE)){
      icolB <- irowB
      fixedB[2] <- fixedB[1]
    }else{
      icolB <- seq(0,dmB[2]-1)   # zero-based indices
      fixedB[2] <- TRUE
    }
  }else{
    icolB <- match_ID(B, IDcolB, MARGIN=2, check=FALSE)
    if(is.null(icolB)){
      stop("'IDcolB' could not be matched to columns of 'B'")
    }
  }

  if(length(icolA) != length(icolB)){
    stop("No compatibility. Provide either matrices with equal number of columns\n",
         "  or 'IDcolA' and/or 'IDcolB' vectors of the same length")
  }

  if(inplace){
    inplace <- ifelse(all(fixedA),1,ifelse(all(fixedB),2,0))
    if(inplace == 0){
      stop("'inplace' calculation can be only applied when either 'A' or 'B' are not resized as per ",
           "the 'IDrow' and 'IDcol' parameters")
    }
  }else{
    inplace <- 0
  }

  #dyn.load("c_hadamard.so")
  return(.Call('R_sumvec', a, dmA[1], dmA[2], A, b, dmB[1], dmB[2], B,
                             irowA, icolA, irowB, icolB,
                             NULL, drop, make.dimnames, inplace))
  #dyn.unload("c_hadamard.so")
}
