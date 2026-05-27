##' Estimates the unmixing and confounded sources of the groupICA model
##' X=A(S+H).
##'
##' For further details see the references.
##' @title groupICA
##' @param X data matrix. Each column corresponds to one predictor
##'   variable.
##' @param group_index vector coding to which group each sample
##'   belongs, with length(\code{group_index})=nrow(\code{X}). If no
##'   group index is provided a rigid grid with \code{groupsize}
##'   samples per group is used (which defaults to all samples if
##'   \code{groupsize} was not set).
##' @param partition_index vector coding to which partition each
##'   sample belongs, with
##'   length(\code{partition_index})=nrow(\code{X}). If no partition
##'   index is provided a rigid grid with \code{partitionsize} samples
##'   per partition is used.
##' @param n_components number of components to extract. If NA is
##'   passed, the same number of components as the input has
##'   dimensions is used.
##' @param n_components_uwedge number of components to extract during
##'   uwedge approximate joint diagonalization of the matrices. If NA
##'   is passed, the same number of components as the input has
##'   dimensions is used.
##' @param rank_components boolean, optional. When TRUE, the
##'   components will be ordered in decreasing stability.
##' @param pairing either 'complement' or 'allpairs'. If 'allpairs'
##'   the difference matrices are computed for all pairs of partition
##'   covariance matrices, while if 'complement' a one-vs-complement
##'   scheme is used.
##' @param groupsize int, optional. Approximate number of samples in
##'   each group when using a rigid grid as groups. If NA is passed,
##'   all samples will be in one group unless group_index is passed
##'   during fitting in which case the provided group index is used
##'   (the latter is the advised and preferred way).
##' @param partitionsize int, optional. Approxiate number of samples
##'   in each partition when using a rigid grid as partition. If NA is
##'   passed, a (hopefully sane) default is used, again, unless
##'   partition_index is passed during fitting in which case the
##'   provided partition index is used.
##' @param max_iter int, optional. Maximum number of iterations for
##'   the uwedge approximate joint diagonalisation during fitting.
##' @param tol float, optional. Tolerance for terminating the uwedge
##'   approximate joint diagonalisation during fitting.
##' @param silent boolean whether to supress status outputs.
##' 
##' @return object of class 'GroupICA' consisting of the following
##'   elements
##'
##' \item{V}{the unmixing matrix.}
##' 
##' \item{coverged}{boolean indicating whether the approximate joint
##' diagonalisation converged due to tol.}
##'
##' \item{n_iter}{number of iterations of the approximate joint
##' diagonalisation.}
##'
##' \item{meanoffdiag}{mean absolute value of the off-diagonal values
##' of the to be jointly diagonalised matrices, i.e., a proxy of the
##' approximate joint diagonalisation objective function.}
##' 
##' @export
##'
##' @import stats utils MASS
##'
##' @author Niklas Pfister and Sebastian Weichwald
##'
##' @references
##' Pfister, N., S. Weichwald, P. Bühlmann and B. Schölkopf (2017).
##' GroupICA: Independent Component Analysis for grouped data.
##' ArXiv e-prints (arXiv:1806.01094).
##'
##' Project website (https://sweichwald.de/groupICA/)
##'
##' @seealso The function \code{\link{uwedge}} allows to perform to
##'   perform an approximate joint matrix diagonalization.
##'
##' @examples
##' ## Example
##' set.seed(1)
##' 
##' # Generate data from a block-wise variance model
##' d <- 2
##' m <- 10
##' n <- 5000
##' group_index <- rep(c(1,2), each=n)
##' partition_index <- rep(rep(1:m, each=n/m), 2)
##' S <- matrix(NA, 2*n, d)
##' H <- matrix(NA, 2*n, d)
##' for(i in unique(group_index)){
##'   varH <- abs(rnorm(d))/4
##'   H[group_index==i, ] <- matrix(rnorm(d*n)*rep(varH, each=n), n, d)
##'   for(j in unique(partition_index[group_index==i])){
##'     varS <- abs(rnorm(d))
##'     index <- partition_index==j & group_index==i
##'     S[index,] <- matrix(rnorm(d*n/m)*rep(varS, each=n/m),
##'                                                      n/m, d)
##'   }
##' }
##' A <- matrix(rnorm(d^2), d, d)
##' A <- A%*%t(A)
##' X <- t(A%*%t(S+H))
##' 
##' # Apply groupICA
##' res <- groupICA(X, group_index, partition_index, rank_components=TRUE)
##' 
##' # Compare results
##' par(mfrow=c(2,2))
##' plot((S+H)[,1], type="l", main="true source 1", ylab="S+H")
##' plot(res$Shat[,1], type="l", main="estimated source 1", ylab="Shat")
##' plot((S+H)[,2], type="l", main="true source 2", ylab="S+H")
##' plot(res$Shat[,2], type="l", main="estimated source 2", ylab="Shat")
##' cor(res$Shat, S+H)
                

groupICA <- function(X,
                     group_index=NA,
                     partition_index=NA,
                     n_components=NA,
                     n_components_uwedge=NA,
                     rank_components=FALSE,
                     pairing='complement',
                     groupsize=1,
                     partitionsize=NA,
                     max_iter=1000,
                     tol=1e-12,
                     silent=TRUE){

  d <- dim(X)[2]
  n <- dim(X)[1]

  # generate group and partition indices as needed
  if(!is.numeric(group_index) & is.na(groupsize)){
    group_index <- rep(0, n)
  }
  else if(!is.numeric(group_index)){
    group_index <- rigidgroup(n, groupsize)
  }
  if(!is.numeric(partition_index) & is.na(partitionsize)){
    smallest_group <- min(unique(group_index, return_counts=TRUE)$counts)
    partition_index <- rigidpartition(group_index,
                                      max(c(d, floor(smallest_group/2))))
  }
  else if(!is.numeric(partition_index)){
    partition_index <- rigidpartition(group_index, partitionsize)
  }
  
  
  no_groups <- length(unique(group_index))
  
  if(!silent){
    print('groupICA: computing covmats')
  }
  
  # estimate covariances (depending on pairing)
  if(pairing == 'complement'){
    no_pairs <- 0
    for(env in unique(group_index)){
      if(length(unique(partition_index[group_index == env]))>1){
        no_pairs <- no_pairs+length(unique(partition_index[group_index == env]))
      }
    }
    covmats <- vector("list", no_pairs)
    idx <- 1
    for(env in unique(group_index)){
      if(length(unique(partition_index[group_index == env]))==1){
        warning(paste("Removing group", toString(env),
                      "since the partition is trivial, i.e., contains only one set"))
      }
      else{
        for(subenv in unique(partition_index[group_index == env])){
          ind1 <- ((partition_index == subenv) &
                     (group_index == env))
          ind2 <- ((partition_index != subenv) &
                     (group_index == env))
          covmats[[idx]] <- cov(X[ind1,]) - cov(X[ind2,])
          idx <- idx + 1
        }
      }
    }
  }
  else if(pairing == 'allpairs'){
    no_pairs <-  0
    subvec <- rep(0, no_groups)
    for(i in 1:no_groups){
      env <- unique(group_index)[i]
      subvec[i] <- length(unique(partition_index[group_index == env]))
      no_pairs <- no_pairs + subvec[i]*(subvec[i]-1)/2
    }
    covmats <- vector("list", no_pairs)
    idx <- 1
    for(count in 1:no_groups){
      env <- unique(group_index)[count]
      unique_subs <- unique(partition_index[group_index == env])
      if(subvec[count] == 1){
        warning(paste("Removing group", toString(env),
                      "since the partition is trivial, i.e., contains only one set"))
      }
      else{
        for(i in 1:(subvec[count]-1)){
          for(j in (i+1):subvec[count]){
            ind1 <- ((partition_index == unique_subs[i]) &
                       (group_index == env))
            ind2 <- ((partition_index == unique_subs[j]) &
                       (group_index == env))
            covmats[[idx]] <- cov(X[ind1,]) - cov(X[ind2,])
            idx <- idx + 1
          }
        }
      }
    }
  }
  else{
    stop('no appropriate pairing specified')
  }

  # check if there are sufficiently many covariance matrices
  if(length(covmats)<=0){
    stop("Not sufficiently many covariance matrices.")
  }

  # add total observational covariance for normalization
  covmats <- append(list(cov(X)), covmats)

  if(!silent){
    print('groupICA: computed cov matrices')
  }

  # joint diagonalisation
  adj_res <- uwedge(covmats,
                    init=NA,
                    rm_x0=TRUE,
                    return_diag=FALSE,
                    tol=tol,
                    max_iter=max_iter,
                    n_components=n_components_uwedge,
                    silent=silent)
  V <- adj_res$V
  
  if(!silent){
    print('groupICA: finished uwedge ajd')
  }

  # rank components
  if(rank_components | !is.na(n_components)){
    A <- ginv(V)
    colcorrs <- rep(0, dim(V)[1])
    # running average
    for(k in 1:length(covmats)){
      colcorrs <- colcorrs + diag(abs(cor(A, covmats[[k]] %*% t(V)))) / length(covmats)
    }
    sorting <- order(colcorrs, decreasing=FALSE)
    V <- V[sorting,]
  }
  if(!is.na(n_components)){
    V <- V[1:n_components,]
  }

  # source estimation
  Shat <- t(V %*% t(X))


  return(list(V=V,
              Shat=Shat,
              converged=adj_res$converged,
              n_iter=adj_res$iteration,
              meanoffdiag=adj_res$meanoffdiag))
}



rigidpartition <- function(group, nosamples){
  partition <- rep(0, length(group))
  for(e in unique(group)){
    partition[group == e] <- rigidgroup(sum(group == e),
                                               nosamples) + max(partition)
  }
  return(partition)
}


rigidgroup <- function(len, nosamples){
  groups <- floor(len / nosamples)
  changepoints <- round(seq(1, len, length.out=groups+1))
  index <- rep(0, len)
  for(i in 1:(length(changepoints)-1)){
    index[changepoints[i]:changepoints[i+1]] <- i
  }
  return(index)
}
