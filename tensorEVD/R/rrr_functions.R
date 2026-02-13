
#====================================================================
# Obtain the indices for the kronecker product between matrices
# A (nrowA x ncolA) and B (nrowB x ncolB).
# Let n1 and n2 to be either nrow or ncol from A and B,
# the kronecker is obtained by multiplying elements
#     [1,1,...,1,2,2,...,2,...,n1,nA...,nA]  from A
# and elements
#     [1,2,...,nB,1,2,...,nB,...,1,2,...,nB] from B
#
#====================================================================
kronecker_index <- function(dimA, dimB, rows = NULL, cols = NULL,
                            swap = FALSE, zero.based = TRUE)
{
  if(length(dimA) == 1L){
    dimA <- c(dimA[1],dimA[1])
  }else{
    stopifnot(length(dimA) == 2L)
  }

  if(length(dimB) == 1L){
    dimB <- c(dimB[1],dimB[1])
  }else{
    stopifnot(length(dimB) == 2L)
  }

  if(!is.null(rows)){
    nrows <- dimA[1]*dimB[1]
    stopifnot(all(!is.na(rows)))
    if(any(rows<1) | any(rows>nrows)){
      stop("Input 'rows' must be integers between 1 and ",nrows)
    }
  }
  if(!is.null(cols)){
    ncols <- dimA[2]*dimB[2]
    stopifnot(all(!is.na(cols)))
    if(any(cols<1) | any(cols>ncols)){
      stop("Input 'cols' must be integers between 1 and ",ncols)
    }
  }

  #dyn.load("utils.so")
  return(.Call('R_kronecker_index', dimA[1], dimA[2], dimB[1], dimB[2],
                                    rows-1, cols-1, swap, zero.based))
  #dyn.unload("utils.so")
}

#====================================================================
#====================================================================

has_names <- function(A){
  dm <- dim(A)
  if(length(dm) == 2L){
    out <- length(unlist(dimnames(A))) == sum(dm)
  }else{
    out <- FALSE
  }

  out
}

#====================================================================
#====================================================================

is.scalar <- function(x){
  is.atomic(x) && length(x) == 1L
}

#====================================================================
# This function matches an ID vector to  row/column names of a matrix
#====================================================================
match_ID <- function(A = NULL, ID, MARGIN = 1, check = TRUE,
                     zero.based = TRUE){
  index <- NULL
  stopifnot(all(!is.na(ID)))

  if(is.null(A)){
    Names <- NULL
    n <- length(unique(ID))
  }else{
    if(is.scalar(A)){
      A <- as.matrix(A)
    }
    if(length(dim(A)) != 2L){
      stop("Input 'A' must be a scalar or a matrix")
    }
    Names <- dimnames(A)
    n <- dim(A)[MARGIN]
  }

  if(is.null(Names)){
    if(is.numeric(ID) | is.integer(ID)){
      index <- as.integer(ID)
    }else{ # This is new: it will create the index if character and A is NULL
      if(is.null(A)){
        if(is.factor(ID)){
          index <- as.numeric(ID)
        }else{
          if(is.character(ID)){
            index <- as.numeric(as.factor(ID))
          }
        }
      }
    }
    if(!is.null(index)){  # checkpoint
      rg <- range(index)
      if(!((1L<=rg[1]) & (rg[2]<=n))){
        index <- NULL
      }
    }
  }else{
    if((length(Names) == 2L)){
      if(check){
        if(!ifelse(length(Names[[1]])==length(Names[[2]]),all(Names[[1]]==Names[[2]]),FALSE)){
          stop("All entries in 'dimnames[[1]]' should be equal to those in 'dimnames[[2]]'")
        }
      }
      names0 <- Names[[MARGIN]]
    }else{
      names0 <- NULL
    }

    if(all(as.character(ID) %in% names0)){
      index <- match(as.character(ID), names0)
    }else{
      if(is.numeric(ID) | is.integer(ID)){
        ID <- as.integer(ID)
        rg <- range(ID)
        if((1L <= rg[1]) & (rg[2] <= n)){
          index <- ID
        }
      }
    }
  }
  if(!is.null(index) & zero.based){
    index <- index-1
  }
  return(index)
}

#====================================================================
#====================================================================

capitalize <- function(string){
  substr(string,1,1) <- toupper(substr(string,1,1))
  string
}

.onAttach <- function(libname, pkgname) {
  addchar <- function(n, char = " ")paste(rep(char,n), collapse="")

  tt1 <- paste0("Loaded '",pkgname,"' R-package. Version ",
                utils::packageVersion(pkgname)," (",utils::packageDate(pkgname),")")
  tt2 <- "Authors: Lopez-Cruz M, Perez-Rodriguez P, & de los Campos G"

  packageStartupMessage("
  |",addchar(70,"="),"|
  | ",tt1,addchar(70-nchar(tt1)-1," "),"|
  | ",tt2,addchar(70-nchar(tt2)-1," "),"|
  |",addchar(70,"="),"|
  ")

  tmp <- utils::old.packages(repos="https://cloud.r-project.org")
  if(pkgname %in% rownames(tmp)){
    packageStartupMessage(" Note: New version ",tmp[pkgname,"ReposVer"],
            " of this package is available on CRAN")
  }
}
