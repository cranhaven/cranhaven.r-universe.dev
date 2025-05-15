###% The following functions (.pos, .neg,.norm,.nndsvd) are from the NMF R package
###% (Gaujoux and Seoighe, 2010). These functions are hidden in the NMF package
##% (hence they have been copied here rather than pulled from the NMF package)
##% and were in turn adapted from Boutsidis and Gallopoulos (2008)
###% Seeding method: Nonnegative Double Singular Value Decomposition
###%
###% @author Renaud Gaujoux
###% @creation 17 Jul 2009


###% Auxliary functions

# .pos extracts all of the positive values from a matrix. This code is taken from the R package NMF
.pos <- function(x){ as.numeric(x>=0) * x }


# .neg extracts all of the negative values from a matrix. This code is taken from the NMF R package

.neg <- function(x){ - as.numeric(x<0) * x }


#.norm takes the Frobenius norm of a non-negative matrix. This code is taken from the NMF R package
.norm <- function(x){ sqrt(drop(crossprod(x))) }


#nnsvd performs non-negative double singular value decomposition This code is taken from the
#NMF R package and is a port to R of the MATLAB code from Boutsidis and Gallopoulos (2008).
.nndsvd <- function(A, k, flag=0,svd=FALSE){

  #check the input matrix
  if( any(A<0) ) stop('The input matrix contains negative elements !')

  #size of input matrix
  size = dim(A);
  m <- size[1]; n<- size[2]

  #the matrices of the factorization
  W = matrix(0, m, k);
  H = matrix(0, k, n);

  #1st SVD --> partial SVD rank-k to the input matrix A.
  if(svd){
  s = svd(A, k, k);
  U <- s$u; S <- s$d; V <- s$v
  }else{
    s=irlba(A,nv=k)
    U<-s$u; S<-s$d; V<-s$v
  }

  #-------------------------------------------------------
  # We also recommend the use of propack for the SVD
  # 1st SVD --> partial SVD rank-k ( propack )
  # OPTIONS.tol  = 0.00001;               % remove comment to this line
  # [U,S,X] = LANSVD(A,k,'L',OPTIONS);    % remove comment to this line
  #-------------------------------------------------------

  #choose the first singular triplet to be nonnegative
  W[,1] = sqrt(S[1]) * abs(U[,1]);
  H[1,] = sqrt(S[1]) * abs(t(V[,1]));

  # second SVD for the other factors (see table 1 in Boutsidis' paper)
  for( i in seq(2,k) ){
    uu = U[,i]; vv = V[,i];
    uup = .pos(uu); uun = .neg(uu) ;
    vvp = .pos(vv); vvn = .neg(vv);
    n_uup = .norm(uup);
    n_vvp = .norm(vvp) ;
    n_uun = .norm(uun) ;
    n_vvn = .norm(vvn) ;
    termp = n_uup %*% n_vvp; termn = n_uun %*% n_vvn;
    if (termp >= termn){
      W[,i] = sqrt(S[i] * as.vector(termp)) * uup / n_uup;
      H[i,] = sqrt(S[i] * as.vector(termp)) * vvp / n_vvp;
    }else{
      W[,i] = sqrt(S[i] * as.vector(termn)) * uun / n_uun;
      H[i,] = sqrt(S[i] * as.vector(termn)) * vvn / n_vvn;
    }
  }

  #------------------------------------------------------------

  #actually these numbers are zeros
  W[W<0.0000000001] <- 0;
  H[H<0.0000000001] <- 0;

  if( flag==1 ){ #NNDSVDa: fill in the zero elements with the average

    ind1 <- W==0 ;
    ind2 <- H==0 ;
    average <- mean(A);
    W[ind1] <- average;
    H[ind2] <- average;

  }else if( flag==2  ){#NNDSVDar: fill in the zero elements with random values in the space :[0:average/100]

    ind1 <- W==0;
    ind2 <- H==0;
    n1 <- sum(ind1);
    n2 <- sum(ind2);

    average = mean(A);
    W[ind1] =  (average * runif(n1, min=0, max=1) / 100);
    H[ind2] =  (average * runif(n2, min=0, max=1) / 100);

  }

  # return matrices W and H
  list(W=W, H=H)
}
#A function that tests whether a matrix is sparse
.is.sparseMatrix <- function(x) is(x, 'sparseMatrix')
#a function that sparsifies the matrices in all of the lists
.sparsify<-function(denselist){
  sparselist<-lapply(denselist,function(x) as(x,"sparseMatrix"))
  return(sparselist)
}
#List the columns of sparse matrices
.listCols<-function(m){
  res<-split(m@x,findInterval(seq_len(nnzero(m)),m@p,left.open=TRUE))
  names(res)<-colnames(m)
  res
}
#List the rows of sparse matrices
.listRows<-function(m){
  mt<-t(m)
  res<-split(mt@x,findInterval(seq_len(nnzero(mt)),mt@p,left.open=TRUE))
  names(res)<-colnames(mt)
  res
}
