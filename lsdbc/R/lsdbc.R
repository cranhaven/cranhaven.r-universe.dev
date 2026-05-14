#'@title Locally Scaled Density Based Clustering
#'
#'@description Generate a locally scaled density based clustering as proposed by Bicici and Yuret (2007).
#'
#'@references Bicici, E., & Yuret, D. (2007). Locally Scaled Density Based Clustering. International Conference on Adaptive and Natural Computing Algorithms (pp. 739-748). Berlin: Springer.
#'
#'@return list
#'
#'@example
#'x <- runif(20,-1,1)
#'y <- runif(20,-1,1)
#'dataset <- cbind(x,y)
#'l <- lsdbc(dataset, 7,3,"euclidean")
#'
#'@export

lsdbc <- function(data, k, alpha, jarak = c("euclidean", "manhattan", "canberra", "geodesic")){
  n <-nrow(data)
  temp <- data

  #Validasi Parameter
  if (k>n){
    stop("k exceeded maximum number allowed")
  }

  geodesic <- function(data){
    rad <- data*pi/180
    n <- nrow(data)
    r <- 6.371009
    dist <- matrix(data = NA, nrow = n,ncol = n)
    for (i in 1:n){
      for (j in i:n)
        dist[i,j] <- dist[j,i] <- r*sqrt(((rad[i,2]-rad[j,2])^2)+((cos((rad[i,2]+rad[j,2])/2)*(rad[i,1]-rad[j,1]))^2))
    }

    return(dist)
  }

  #Menghitung Matriks Jarak
  if (jarak=="geodesic"){
    dist_ <- as.matrix(geodesic(data))
  }else{
    dist_ <- as.matrix(dist(data, method=jarak, diag = T, upper = T))
  }

  #Mengurutkan epsilon dan menentukan tetangga
  distsort <- dist_
  for(i in 1:n){
    distsort[i,] <- sort(as.vector(distsort[i,]), decreasing = F)
  }
  neighbor <- matrix(nrow=n, ncol=k)
  eps <- distsort[,2:(k+1)]

  for (i in 1:n){
    neighbor[i,] <- pmatch(as.character(as.vector(eps[i,])), as.character(as.vector(dist_[i,])))
  }

  temp <- cbind(eps[,k],neighbor)
  temp <- as.data.frame(temp)
  temp <- cbind(c(1:n), temp)
  colnames(temp) <- c("Idx","Eps", c(1:k))
  temp <- temp[order(temp$Eps),]

  #Fungsi Pengecekan Maksimum Lokal
  localMax <- function(pointNum, k, temp){
    neighbor <- temp[pointNum,3:(k+2)]
    idx <- temp[pointNum,1]
    eps <- temp[order(temp[,1]),2]

    for (i in 1:k){
      if(eps[neighbor[[i]]]<eps[idx]){
        return(FALSE)
      } else return(TRUE)
    }
  }

  #Fungsi Ekspansi Klaster
  expandClust <- function(pointNum, k, clusterID, n, powerf, dat_fix){
    dat_fix[pointNum,(k+3)] <- clusterID
    neighbor <- dat_fix[pointNum,3:(k+2)]
    seed <- c()
    pembanding <- powerf*dat_fix[pointNum,2]
    currentP <- 0
    newneighbor <- c()

    for (i in 1:k){
      dat_fix[which(dat_fix$Idx==neighbor[[i]]),(k+3)] <- ifelse(dat_fix[which(dat_fix$Idx==neighbor[[i]]),(k+3)]==0,
                                                                 clusterID,dat_fix[which(dat_fix$Idx==neighbor[[i]]),(k+3)])
      if(dat_fix[which(dat_fix$Idx==neighbor[[i]]),(k+3)]==clusterID){
        seed <- c(seed,neighbor[[i]])
      }
    }

    while (length(seed>0)){
      currentP <- seed[[1]]
      if(dat_fix[which(dat_fix$Idx==currentP),3] <= pembanding){
        newneighbor <- dat_fix[which(dat_fix$Idx==currentP),3:(k+2)]
        for (j in 1:k){
          dat_fix[which(dat_fix$Idx==newneighbor[[j]]),(k+3)] <- ifelse(dat_fix[which(dat_fix$Idx==newneighbor[[j]]),(k+3)]==0,
                                                                        clusterID,dat_fix[which(dat_fix$Idx==newneighbor[[j]]),(k+3)])
          if(dat_fix[which(dat_fix$Idx==newneighbor[[j]]),(k+3)]==clusterID){
            seed <- c(seed,newneighbor[[j]])
          }
        }
      }
      seed <- seed[-1]
    }

    return(dat_fix)
  }

  #Inisiasi Klaster
  class_ <- rep(0, n)
  df_fix <- cbind(temp,class_)

  clusterID <- 1
  powerf <- 2^(alpha/n)

  #Main Loop
  for (i in 1:n){
    if(df_fix[i,(k+3)]==0 && localMax(i,k,df_fix)==TRUE){
      df_fix <- expandClust(i,k,clusterID,n,powerf,df_fix)
      clusterID <- clusterID+1
    }
  }

  #Output
  clust <- df_fix[order(df_fix$Idx),(k+3)]
  return(list(data=data, cluster=clust,
              parameter=rbind(c("k",k),c("alpha",alpha))))
}

