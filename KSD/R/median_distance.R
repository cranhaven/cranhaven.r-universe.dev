

find_median_distance <- function(Z){

      if(is.data.frame(Z)){
           Z = data.matrix(Z)
      }else{
            Z = as.array(Z)
      }
      size1 <- dim(Z)[1]
      size2 <- dim(Z)[2]

      # if size of Z is greater than 100, randomly sample 100 points
      if(size1 > 100){
            if(is.na(size2)){
                  Zmed <- Z[sample(size1,100)]
            }else{
                  Zmed <- Z[sample(size1,100),]
            }
            size1 = 100
      }else{
            Zmed <- Z
      }

      Zmedsq <- Zmed * Zmed;
      if(is.na(dim(Z)[2]))
            G <- Zmedsq
      else
            G <- rowSums(Zmedsq)

      # Create row/col repeated matrices
      Q <- rep.col(G,size1)
      R <- rep.row(t(G),size1)

      dists <- Q + R - 2 * Zmed %*% t(Zmed)
      dists[lower.tri(dists, diag = TRUE)] = 0
      dists <- array(dists,dim=c(size1^2,1))
      median_dist <- median(dists[dists > 0 ])

      return(median_dist)
}


ratio_median_heuristic <- function(Z, score_function){

      Z = as.array(Z)
      size1 <- dim(Z)[1]
      size2 <- dim(Z)[2]

      # if size of Z is greater than 100, randomly sample 100 points
      if(size1 > 100){
            if(is.na(size2)){
                  Zmed <- Z[sample(size1,100)]
            }else{
                  Zmed <- Z[sample(size1,100),]
            }
            size1 = 100
            print('Sampled (Heuristic)')
      }else{
            Zmed <- Z
            print('Original 100 dataset used (Heuristic)')
      }

      Zmedsq <- Zmed * Zmed;
      if(is.na(dim(Z)[2])){
            G <- Zmedsq
      } else{
            G <- rowSums(Zmedsq)
      }

      # Create row/col repeated matrices
      Q <- rep.col(G,size1)
      R <- rep.row(t(G),size1)

      dists <- Q + R - 2 * Zmed %*% t(Zmed)
      dists[lower.tri(dists, diag = TRUE)] = 0
      dists <- array(dists,dim=c(size1^2,1))
      median_dist <- median(dists[dists > 0 ])

      if(is.na(size2)){
            Zmed = as.double(Zmed)
      }
      sqx <- score_function(Zmed)
      sqxx <- sqx %*% t(sqx)
      sqxx[lower.tri(sqxx, diag = TRUE)] = 0
      sqxx <- array(sqxx,dim=c(size1^2,1))
      median_sqxx <- median(sqxx[sqxx > 0])

      h <- (median_dist / median_sqxx)^(1/4)
      medInfo <- list("h"=h, "median_dist" = median_dist, "median_sqxx" = median_sqxx)
}

