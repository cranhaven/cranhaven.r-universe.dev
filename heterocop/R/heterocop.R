#' matrix_gen
#' 
#' @description This function enables the user to generate a sparse, nonnegative definite correlation matrix via the Cholesky decomposition
#' 
#' @param d the number of variables
#' @param gamma an initial sparsity parameter for the lower triangular matrices in the Cholesky decomposition, must be between 0 and 1
#'
#' @return a list containing the generated correlation matrix and its final sparsity parameter (ie the proportion of zeros)
#' @examples
#' matrix_gen(15,0.81)
#' 
#' @export

matrix_gen <- function(d,gamma){
  if(gamma >= 0 & gamma <=1){
    L <- matrix(0,d,d)
    params <- stats::runif(d*(d-1)/2,0.3,1)
    ind <- sample(1:(d*(d-1)/2),floor(gamma*d*(d-1)/2))
    params[ind] <- 0
    L[lower.tri(L)] <- params
    diag(L) <- 1
    R <- L%*%t(L)
    C <- round(stats::cov2cor(R),3)
    gamma_f = sum(C==0)/(d*d)
    return(list(as.matrix(C),paste0("sparsity = ",gamma_f)))
    }
  else{
    stop("gamma must be between 0 and 1")
  }
}

#' diag_block_matrix
#' 
#' @description This function enables the user to generate a diagonal block-matrix with homogeneous blocks
#' 
#' @param blocks a vector containing the sizes of the blocks
#' @param coeff a vector containing the coefficient corresponding to each block, the coefficients must be between 0 and 1
#'
#' @return a diagonal block-matrix containing the specified coefficients
#' @examples
#' diag_block_matrix(c(3,4,5),c(0.3,0.4,0.8))
#' 
#' @export

diag_block_matrix <- function(blocks, coeff){
  if(!is.numeric(blocks)){
    stop("The sizes of the blocks must be numeric")
  }else if(sum(round(blocks)==blocks)!=length(blocks)){
    stop("The sizes of the blocks must be integers.")
  }else if(!is.numeric(coeff)){
    stop("The coefficients must be numeric.")
  }else if(sum(coeff>1)!=0){
    stop("The coefficients must be between 0 and 1.")
  }else if(sum(coeff<0)!=0){
    stop("The coefficients must be between 0 and 1.")
    }else if(length(coeff)!=length(blocks)){
    stop("The number of blocks must be equal to the number of coefficients.")
  }else{
    step=c(0,cumsum(blocks))
    d = sum(blocks)
    R = matrix(0,d,d)
    for(i in 1:(length(step)-1)){
      R[((step[i]+1):step[i+1]),((step[i]+1):step[i+1])]=coeff[i]
    }
    diag(R)=1
    return(R)
  }
}

#' gauss_gen
#' 
#' @description This function enables the user to generate gaussian vectors with correlation matrix R
#' 
#' @param R a correlation matrix of size dxd
#' @param n the number of observations
#' 
#' @return a nxd data frame containing n observations of the d variables
#' @examples
#' M <- diag_block_matrix(c(3,4,5),c(0.7,0.8,0.2))
#' gauss_gen(M,20)
#' 
#' @export

gauss_gen <- function(R, n){
  if(sum(R>1 | R< -1)==0){ #checking that all coefficients are between -1 and 1
    if(matrixcalc::is.positive.semi.definite(R)){ # checking that the matrix is semi-positive definite
      d = dim(R)[1]
      A = as.matrix(eigen(R)$vectors%*%diag(sqrt(eigen(R)$values)))
      z = matrix(stats::rnorm(d*n, mean = 0, sd = 1), ncol=n)
      data <- data.frame(t(stats::pnorm(A%*%z)))
      return(data)
      }
    else{
      stop("The matrix is not positive semi-definite")
    }
  }
  else{
    stop("The coefficients in the matrix must be between -1 and 1")
  }
}

#' CopulaSim
#' 
#' @description This function enables the user to simulate data from a Gaussian copula and arbitrary marginal quantile functions
#' 
#' @param n the number of observations
#' @param R a correlation matrix of size dxd
#' @param qdist a vector containing the names of the marginal quantile functions as well as the number of times they are present in the dataset
#' @param random a boolean defining whether the order of the correlation coefficients should be randomized
#' 
#' @return a list containing an nxd data frame, the shuffled correlation matrix R, and the permutation leading to the new correlation matrix
#'
#' @examples
#' M <- diag_block_matrix(c(3,4,5),c(0.7,0.8,0.2))
#' CopulaSim(20,M,c(rep("qnorm(0,1)",6),rep("qexp(0.5)",4),rep("qbinom(4,0.8)",2)),random=TRUE)
#' 
#' @export

CopulaSim <- function(n, R, qdist, random = FALSE){
  if(!matrixcalc::is.positive.semi.definite(R)){
    stop("The correlation matrix must be semi-positive definite.")
  }else if(!(sum(R>1 | R< -1)==0)){
    stop("The correlation coefficients must be between -1 and 1")
  }else if(dim(R)[1]==length(qdist)){
    
    d = length(qdist)
    order <- 1:d
    
    if(random==TRUE){
      order <- sample(1:d)
      R <- R[order,order]
    }
    
    XY = gauss_gen(R,n)
    
    vars = matrix(0,n,d)
    for (j in 1:d){
      expr <- stringr::str_split_1(qdist[j],"\\(")
      vars[,j] = eval(parse(text=paste0(expr[1],"(XY[,j],",expr[2])))
    }
    vars <- data.frame(vars)
    
    return(list(vars,R[order,order],order))
  }else{
    stop("The total number of distribution replications must be equal to the size of the matrix")
  }
}

# auxiliary functions

fdr_d <- function(X, Type){
  if(!is.data.frame(X)){
    X <- as.data.frame(X)
  }
  F <- array(0,dim=c(dim(X)[1],dim(X)[2],2))
  F[,,2] <- sapply(X,rank,ties.method="max")/(dim(X)[1]+1)
  F_s <- rbind(0,apply(F[,,2],2,sort))
  for (i in 1:dim(X)[2]){
    if (Type[i] == "D"){
      for(j in 1:dim(X)[1]){
        F[j,i,1]=F_s[min(which(F_s[,i]==F[j,i,2]))-1,i]
      }
    }
  }
  return(F)
}

c_R_2D <- function(x1, x2, rho) + return(exp(-0.5*((rho**2)*(stats::qnorm(x1,0,1)**2+stats::qnorm(x2,0,1)**2)-2*rho*stats::qnorm(x1,0,1)*stats::qnorm(x2,0,1))/(1-rho**2))/sqrt(1-(rho**2)))

C_R_2D<- function(u1, u2, l1, l2, R){
  return(mvtnorm::pmvnorm(upper=c(stats::qnorm(u1,0,1), stats::qnorm(u2,0,1)),mean=c(0,0),lower=c(stats::qnorm(l1,0,1), stats::qnorm(l2,0,1)),corr = R, sigma=NULL, algorithm = mvtnorm::GenzBretz(), keepAttr=FALSE))
}


L_n_CC <- function(theta, F1, F2){
  delta = 10**-9
  mysummands <- c_R_2D(F1, F2, theta)
  L <- sum(log(mapply(max,mysummands,MoreArgs=list(delta))))
  return(-L)
}

L_n_DD <- function(theta, F1m, F1p, F2m, F2p){
  R = matrix(c(1, theta, theta, 1), 2, 2)
  delta = 10**-9
  mysummands <- mapply(C_R_2D,F1p, F2p, F1m, F2m, MoreArgs=list(R))
  L <- sum(log(mapply(max,mysummands,MoreArgs=list(delta))))
  return(-L)
}


L_n_CD <- function(theta, F1, F2m, F2p){
  delta=10**-9
  mysummands <- stats::pnorm(stats::qnorm(F2p),mean=theta*stats::qnorm(F1),sd=sqrt(1-theta**2)) - stats::pnorm(stats::qnorm(F2m),mean=theta*stats::qnorm(F1),sd=sqrt(1-theta**2))
  L <-sum(log(mapply(max,mysummands,MoreArgs=list(delta))))
  return(-L)
}

`%dopar%` <- foreach::`%dopar%`

#' rho_estim
#' 
#' @description This function enables the user to estimate the correlation matrix of the Gaussian copula for a given dataset
#' 
#' @param data an nxd data frame containing n observations of d variables
#' @param Type a vector containing the type of the variables, "C" for continuous and "D" for discrete
#' @param ncores an integer specifying the number of cores to be used for parallel computation. "1" by default, leading to non-parallel computation.
#' @return the dxd estimated correlation matrix of the Gaussian copula
#'
#' @examples
#' M <- diag_block_matrix(c(3,4,5),c(0.7,0.8,0.2))
#' data <- CopulaSim(20,M,c(rep("qnorm(0,1)",6),rep("qexp(0.5)",4),
#' rep("qbinom(4,0.8)",2)),random=FALSE)[[1]]
#' rho_estim(data,c(rep("C",10),rep("D",2)))
#' 
#' @export

rho_estim <- function(data,Type,ncores=1){
    
    F = fdr_d(data, Type)
    M_rho = diag(length(Type))
    
    if(ncores==1){
        
        for (i in 1:(length(Type)-1)){
            for (j in (i+1):length(Type)){
                if (Type[i] == "C" & Type[j] == "C"){
                    rho_ij <- stats::optimize(L_n_CC, c(-1,1), F1 = F[,i,2], F2 = F[,j,2], maximum = FALSE)$minimum
                }
                if(Type[i] == "C" & Type[j] == "D"){
                    rho_ij <- stats::optimize(L_n_CD, c(-1,1), F1 = F[,i,2], F2m = F[,j,1], F2p = F[,j,2],maximum = FALSE)$minimum
                }
                if(Type[j] == "C" & Type[i] == "D"){
                    rho_ij <- stats::optimize(L_n_CD, c(-1,1), F1 = F[,j,2], F2m = F[,i,1], F2p = F[,i,2],maximum = FALSE)$minimum
                }
                if(Type[j] == "D" & Type[i] == "D"){
                    rho_ij <- stats::optimize(L_n_DD, c(-1,1), F1m = F[,i,1], F1p = F[,i,2], F2m = F[,j,1], F2p = F[,j,2],maximum = FALSE)$minimum
                }
                M_rho[i,j] = rho_ij
                M_rho[j,i] = rho_ij
            }
        }
        rownames(M_rho) <- colnames(data)
        colnames(M_rho) <- rownames(M_rho)
        return(M_rho)
    }else{
      chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
      if (nzchar(chk) && chk == "TRUE") {
        Ncpus <- 2
      } else {
        Ncpus <- ncores
      }
        cl <- parallel::makeCluster(Ncpus, outfile="")
        doSNOW::registerDoSNOW(cl)
        pb <- utils::txtProgressBar(min=1, max=length(Type)-1, style=3)
        progress <- function(n) utils::setTxtProgressBar(pb, n)
        opts <- list(progress=progress)
        M_rho <- foreach::foreach(i=1:(length(Type)-1), .combine='rbind',.export=c("c_R_2D", "C_R_2D","L_n_CC", "L_n_CD","L_n_DD"),.options.snow=opts)%dopar%{
            rho_i <- c(rep(0,i-1),1) 
            for (j in (i+1):length(Type)){
                if (Type[i] == "C" & Type[j] == "C"){
                    rho_ij <- stats::optimize(L_n_CC, c(-1,1), F1 = F[,i,2], F2 = F[,j,2], maximum = FALSE)$minimum
                }
                if(Type[i] == "C" & Type[j] == "D"){
                    rho_ij <- stats::optimize(L_n_CD, c(-1,1), F1 = F[,i,2], F2m = F[,j,1], F2p = F[,j,2],maximum = FALSE)$minimum
                }
                if(Type[j] == "C" & Type[i] == "D"){
                    rho_ij <- stats::optimize(L_n_CD, c(-1,1), F1 = F[,j,2], F2m = F[,i,1], F2p = F[,i,2],maximum = FALSE)$minimum
                }
                if(Type[j] == "D" & Type[i] == "D"){
                    rho_ij <- stats::optimize(L_n_DD, c(-1,1), F1m = F[,i,1], F1p = F[,i,2], F2m = F[,j,1], F2p = F[,j,2],maximum = FALSE)$minimum
                }
                rho_i <- c(rho_i, rho_ij)
            }
            return(rho_i)
        }
        close(pb)
        M_rho <- rbind(M_rho, c(rep(0,length(Type)-1),1))
        parallel::stopCluster(cl)
        M_rho <- M_rho + t(M_rho)-diag(1,length(Type),length(Type))
        rownames(M_rho) <- colnames(data)
        colnames(M_rho) <- rownames(M_rho)
        return(M_rho)
    }
}

#' matrix_cor_ts
#' 
#' @description This function enables the user to threshold matrix coefficients
#' 
#' @param R a correlation matrix
#' @param TS a threshold
#' @param binary a boolean specifying whether the coefficients should be binarized, TRUE by defaut (zero if the coefficient is less than the threshold in absolute value, 1 otherwise)
#' 
#' @return the thresholded input matrix
#' 
#' @examples
#' M <- diag_block_matrix(c(3,4,5),c(0.7,0.8,0.2))
#' matrix_cor_ts(M,0.5)
#' 
#' @export

matrix_cor_ts <- function(R, TS, binary = TRUE){
  M_ = R
  M_[which(abs(M_) <= TS)] = 0
  if(binary==TRUE){
  M_[which(M_ != 0)] = 1
  }
  return(M_)
}

#' cor_network_graph
#' 
#' @description This function enables the user to plot the graph corresponding to the correlations of the Gaussian copula
#' 
#' @param R a correlation matrix of size dxd (d is the number of variables)
#' @param TS a threshold for the absolute values of the correlation matrix coefficients
#' @param binary a boolean specifying whether the coefficients should be binarized, TRUE by defaut (zero if the coefficient is less than the threshold in absolute value, 1 otherwise). If FALSE, the edge width is proportional to the coefficient value.
#' @param legend a vector containing the type of each variable used to color the vertices
#' 
#' @return a graph representing the correlations between the latent Gaussian variables
#'
#' @examples
#' R <- diag_block_matrix(c(3,4,5),c(0.7,0.8,0.2))
#' data <- CopulaSim(20,R,c(rep("qnorm(0,1)",6),rep("qexp(0.5)",4),
#' rep("qbinom(4,0.8)",2)),random=FALSE)[[1]]
#' cor_network_graph(R,TS=0.3,binary=TRUE,legend=c(rep("Normal",6),
#' rep("Exponential",4),rep("Binomial",2)))
#' 
#' @export

cor_network_graph <- function(R, TS, binary = TRUE, legend){
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  legend <- factor(legend,levels=unique(legend))
  network_ref <- igraph::graph_from_adjacency_matrix(matrix_cor_ts(R, TS, binary),mode="undirected", diag=F, weighted=TRUE)
  colors <- grDevices::hcl.colors(length(unique(legend)),palette="PinkYl")
  graphics::par(bg="white", mar=c(1,1,1,1))
  plot(network_ref,
       edge.width = igraph::E(network_ref)$weight,
       vertex.size=8,
       vertex.label = colnames(R),
       vertex.color= colors[legend],
       vertex.label.cex=0.8,
       vertex.label.color="black",
       vertex.frame.color="black",
       edge.color = "black")
  legend(1.2,0.8,cex=0.6, unique(legend),fill=colors)
  graphics::text(1.1,1, stringr::str_glue("threshold = ",TS) ,col="black", cex=1)
}


#' omega_estim
#' 
#' @description This function enables the user estimate the precision matrix of the latent variables via gLasso inversion
#' 
#' @param data a dataset of size nxd or a correlation matrix R of size dxd
#' @param Type a vector containing the type of the variables, "C" for continuous and "D" for discrete (in the case a data set is entered as the first parameter)
#' @param lambda a grid of penalization parameters to be evaluated
#' @param n the sample size used (in the case of a correlation matrix entered as the first parameter)
#' 
#' @return a list containing the correlation matrix, the optimal precision matrix, the optimal lambda, the minimal HBIC, all values of lambda, all corresponding HBIC values
#'
#' @examples
#' M <- diag_block_matrix(c(3,4,5),c(0.7,0.8,0.2))
#' data <- CopulaSim(20,M,c(rep("qnorm(0,1)",6),rep("qexp(0.5)",4),
#' rep("qbinom(4,0.8)",2)),random=FALSE)[[1]]
#' \dontrun{P <- omega_estim(data,c(rep("C",10),rep("D",2)),seq(0.01,1,0.05))}
#' 
#' @export

omega_estim <- function(data,Type,lambda,n){
  
  if(isSymmetric(as.matrix(data))){
    
    R <- data
    n <- n
    
    p <- dim(data)[1]
    
    names <- colnames(data)
    
  }else{
    
  n <- dim(data)[1]
  p <- dim(data)[2]
  R <- rho_estim(data,Type)
  names <- colnames(R)
  }
  
  HBIC <- c()
  for(l in 1:length(lambda)){
    
   OM <- huge::huge(R,lambda[l],method="glasso",verbose=F)$icov[[1]]
   crit <- matrixcalc::matrix.trace(R%*%OM)-log(det(OM))+log(log(n))*(log(p)/n)*sum(OM!=0)
   HBIC <- c(HBIC,crit)
  
   }
  
  HBIC_min <- min(HBIC,na.rm=T)
  lamb_min <- lambda[which(HBIC==HBIC_min)]
  
  OM <- huge::huge(R,lamb_min,method="glasso",verbose = F)$icov[[1]]
  
  colnames(OM) <- names
  rownames(OM) <- names
  
  return(list(R,OM,paste0("The minimal lambda is ", lamb_min),paste0("The minimal HBIC is ", HBIC_min), lambda, HBIC))
}



#' ICGC dataset
#'
#' Dataset containing RNA counts, protein expression and mutations measured on breast cancer tumors.
#'
#' @format A dataframe of 15 variables and 250 observations containing the following:
#' \describe{
#'   \item{ACACA, AKT1S1, ANLN,ANXA1,AR}{RNA counts (discrete)}
#'   \item{ACACA_P, AKT1S1_P, ANLN_P,ANXA_P,AR_P}{protein expression measurements (discrete) }
#'   \item{MU5219,MU4468,MU7870,MU4842,MU6962}{5 mutations (binary)}
#' }
"icgc_data"
