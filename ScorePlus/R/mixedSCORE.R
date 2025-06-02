#' Membership estimation algorithm called mixedSCORE
#'
#' @param A n-by-n binary symmtric adjacency matrix.
#' @param K number of communities.
#' @param verbose whether generate message
#'
#' @return A list containing \describe{
#'   \item{R}{n-by-(K-1) ratio matrix.}
#'   \item{L}{Selected tunning parameter used for vertex hunting algorithm.}
#'   \item{thetas}{A vector of the estimated degree heterogeniety parameters}
#'   \item{vertices}{K-by-(K-1) K vertices of the found convex hull}
#'   \item{centers}{L-by-(K-1) L centers by kmeans}
#'   \item{memberships}{n-by-K membership matrix.}
#'   \item{purity}{A vector of maximum membership of each node}
#'   \item{hard.cluster.labels}{A vector of integers indicating hard clutering labels, by assigning the node to the cluster with max membership}
#' }
#' @import RSpectra
#' @importFrom combinat permn
#' @import utils
#' @import limSolve
#' @import stats
#' @export
#' @examples
#' library(igraphdata)
#' library(igraph)
#' data('karate')
#' A = get.adjacency(karate)
#' karate.mixed.out = mixedSCORE(A, 2)
#' karate.mixed.out$memberships

mixedSCORE <- function(A, K, verbose = F){

  if(verbose){
    cat('Get ratios --------\n')
  }
  score.out = SCORE(A = A, K = K, threshold = Inf)
  R = score.out$R
  if(verbose){
    cat('Vertex hunting --------\n')
  }
  vh.out = vertexHunting(R = R, K = K,verbose = verbose)
  if(verbose){
    cat('Get the membership ----\n')
  }
  memb.out = getMembership(R = R,
                           vertices = vh.out$vertices,
                           K = K,
                           eig.values = score.out$eig.values,
                           eig.vectors = score.out$eig.vectors)
  memberships = memb.out$memberships
  degrees = memb.out$degrees

  if(verbose){
    cat('Get the purity scores and hard clustering results ----\n')
  }
  purity = apply(X = memberships, MARGIN = 1, FUN = max)
  major.labels = apply(X = memberships, MARGIN = 1, FUN = which.max)

  return(list(R = R,
              L = vh.out$L,
              vertices = vh.out$vertices,
              centers = vh.out$centers,
              memberships = memberships,
              degrees = degrees,
              purities = purity,
              major.labels = major.labels))
}




#' Vertex hunting algorithm to find the cluster centers
#'
#' @param R n-by-(K-1) ratio matrix
#' @param K number of communities.
#' @param verbose whether or not to show a progress bar
#'
#' @return A list containing \describe{
#'   \item{L}{Selected tunning parameter.}
#'   \item{vertices}{{K-by-(K-1) cluster center matrix shows the K community centers.}
#' }
#'
#' @export
vertexHunting <- function(R, K, verbose = F){

  L.candidate = (K+1): (3*K) # candidate Ls
  n.L = length(L.candidate)

  out.list = lapply(L.candidate, function(L){
    if(verbose){
      cat('L = ',L,'\n')
    }
    centers = kmeans(R, L, iter.max = 100, nstart = 100)$centers # L*(K-1)
    out = vertexSearch(centers, K = K)
    if(verbose){
      cat( 'dist = ', out$dist,'\n')
    }
    return(out)
  })


  delta.L = sapply(1:n.L, function(i){
    if( i == 1){
      V.Lminus1 =  kmeans(R, K, iter.max = 100, nstart = 100)$centers
    } else {
      V.Lminus1 = out.list[[i-1]]$vertices
    }
    V.L = out.list[[i]]$vertices

    perm = combinat::permn(1:K) # list of all the permutations
    delta = min(sapply(1:length(perm), function(p){
      max(rowSums(V.L[perm[[p]],] - V.Lminus1)^2)
    }))
    return(delta/(1+out.list[[i]]$dist))
  })

  L.ind = which.min(delta.L)
  L.select = L.candidate[L.ind]

  if(verbose){
    cat('Select L =', L.select,'\n')
  }

  return(list(L = L.select,
              vertices = out.list[[L.ind]]$vertices,
              centers = out.list[[L.ind]]$centers))
}

#' select the \code{K} vertices from given \code{L} centers
#'
#' @param centers L-by-(K-1) center matrix
#' @param K number of communities.
#'
#' @return A list containing \describe{
#'   \item{ind}{a vector of \code{K} integers indicating the index of selected \code{K} vertices out of \code{L} centers.}
#'   \item{dist}{The maximum distance from centers to the convex hull formed by the \code{K} selected vertice}
#' }
#'
vertexSearch <- function(centers, K){
  L = nrow(centers)

  index.matrix = utils::combn(L, K)  # K * (L K)
  n.c =  ncol(index.matrix )

  dist = sapply(1:n.c, function(i){
    getMaxDist(centers = centers,
               vertex.ind = index.matrix[,i])
  })

  ind = index.matrix[, which.min(dist)[1]]
  vertices = matrix(centers[ind,],nrow = K)

  return(list(ind = ind,
              dist = min(dist),
              vertices = vertices,
              centers = centers))
}



reorder <- function(centers){
  centers[order(apply(centers, 1, function(x) norm(x,'2')),decreasing = T),]
}

#' find the maxinum distance from the convex hull formed by the chosen K vertices
#'
#' @param centers L-by-(K-1) center matrix
#' @param vertex.ind index of the \code{K} centers that forms the convex hull
#'
#' @return the maximum distance
#'
getMaxDist <- function(centers, vertex.ind){
  L = nrow(centers)
  K = length(vertex.ind)

  nonvertex = matrix(centers[-vertex.ind,], nrow = L-K)
  vertex = matrix(centers[vertex.ind,], nrow = K)

  dist = sapply(1:(L-K), function(i){
    node = nonvertex[i,]
    # calculate the distance from the convex hull
    out = limSolve::lsei(A = t(vertex),
                         B = node,
                         E = rep(1,K), F = 1,
                         G = diag(K), H = rep(0,K),
                         type = 2)
    return(out$solutionNorm)
  })
  return(max(dist))
}

#' calculated the membership of each node given ratio matrix and community centers
#'
#' @param R n-by-(K-1) ratio matrix
#' @param vertices K-by-(K-1) community centers
#' @param K number of communities.
#' @param eig.values eigenvalues of adjacency matrix.
#' @param eig.vectors eigenvectors of adjacency matrix.
#' @return n-by-K membership matrix
#'
#' @export
getMembership <- function(R, vertices, K, eig.values, eig.vectors){
  n = nrow(R)
  memberships = matrix(0, nrow = n, ncol = K)

  for(i in 1:n){
    out = limSolve::lsei(A = t(vertices),
                         B = R[i,],
                         E = rep(1,K), F = 1,
                         G = diag(K), H = rep(0,K),
                         type = 2)
    memberships[i, ] = out$X
  }

  # truncate and normalize
  memberships[memberships > 1] = 1
  memberships[memberships < 0] = 0
  for(i in 1:n){
    memberships[i,] = memberships[i,]/sum(memberships[i,])
  }

  # recover the original memberships
  tildeV = cbind(rep(1,K), vertices)
  b1.inv = sqrt(diag(tildeV%*%diag(eig.values[1:K])%*%t(tildeV)))

  # get degrees
  degrees = abs(eig.vectors[,1] * rowSums(memberships %*% diag(b1.inv)) )

  # get tilted memberships
  memberships = memberships%*%diag( b1.inv)


  # normalize again
  for(i in 1:n){
    memberships[i,] = memberships[i,]/sum(memberships[i,])
  }

  return(list(memberships = memberships, degrees = degrees))
}

