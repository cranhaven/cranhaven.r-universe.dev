# function that lists the total number of ways to permute the pencils in a flat such that they are linearly independent
.getpenciloptions <- function(t){
  basis <- matrix(0, nrow = 2^t, ncol = t)
  for(i in 1:t){
    basis[(floor((0:(2^t-1))/(2^(i-1))) %%2 == 1),i] <- 1
  }
  basis <- basis[-1,] # remove the null effect
  totalcount <- prod((2^t - 2^(0:(t-1))))
  options <- matrix(ncol = t, nrow = totalcount)
  pencils <- 1:(2^t-1)
  alloptions <- gtools::permutations(n=(2^t-1),r=t, v= pencils,repeats.allowed=FALSE)
  k <- 1
  for(i in 1:nrow(alloptions)){
    if((det(basis[alloptions[i,],]) %% 2) != 0){
      options[k,] <- alloptions[i,]
      k <- k + 1
    }
  }
  return(options)
}

# This is a function to get CxB, the collineation mapping x1,...xn to the canonical basis
.getCxB <- function(spr){
  n <- dim(spr)[1]
  t <- log(dim(spr)[2] + 1)/log(2)
  ell <- n/t
  mu <- (2^n - 1)/(2^t - 1)
  flatsize <- 2^t-1
  spacesize <- 2^n -1
  pencilmappingoptions <- .getpenciloptions(t)
  CxB <- matrix(nrow = n, ncol = n)
  for(i in 1:ell){
    for(j in 1:t){
      CxB[,t * (i-1) +j] <- spr[,pencilmappingoptions[1,j],i]
    }
  }
  if((det(CxB) %%2) == 0){
    #print("First ell flats are not independent")
    return(matrix(nrow = n, ncol = n))
  }
  return(solve(CxB) %% 2)
}

# reduced row echelon form modulo2
.rrefmod2 <- function(A)
{
  nr <- nrow(A)
  nc <- ncol(A)
  r <- 1
  for (i in 1:nc) {
    pivot <- which.max(abs(A[r:nr, i]))
    pivot <- r + pivot - 1
    m <- abs(A[pivot, i])
    if (m == 0) {
      A[r:nr, i] <- 0
    }
    else {
      A[c(pivot, r), i:nc] <- A[c(r, pivot), i:nc]
      if (r == 1) {
        ridx <- c((r + 1):nr)
      }
      else if (r == nr) {
        ridx <- c(1:(r - 1))
      }
      else {
        ridx <- c(1:(r - 1), (r + 1):nr)
      }
      A[ridx, i:nc] <- (A[ridx, i:nc] - A[ridx, i, drop = FALSE] %*%
                          A[r, i:nc, drop = FALSE]) %% 2
      if (r == nr)
        break
      r <- r + 1
    }
  }
  return(A)
}

.arrange_yates <- function(spread){
  ddd <- dim(spread)
  n <- ddd[1]
  t <- log(ddd[2] + 1)/log(2)
  alls <- matrix(0, nrow = 2^t, ncol = t)
  for(i in 1:t){
    alls[(floor((0:(2^t-1))/(2^(i-1))) %%2 == 1),i] <- 1
  }
  alls<- alls[-1,] # remove the null effect
  rearranged_spread <- array(NA, ddd)
  for(i in 1:ddd[3]){
    basis <- .rrefmod2(t(spread[,,i]))[1:t,]
    rearranged_spread[,,i] <- (t(basis) %*% t(alls)) %%2
  }
  return(rearranged_spread)
}

.arrange_flats_independent <- function(spread){
  arrangedwell <- F
  set.seed(1)
  ddd <- dim(spread)
  n <- ddd[1]
  t <- log(ddd[2] + 1)/log(2)
  indices <- 2^(0:(t-1))
  ninbase <- n/t
  perm <- 1:ddd[3]
  while(arrangedwell == FALSE){
    hopefulbase <- spread[,indices,perm[1]]
    for(j in 2:ninbase){
      hopefulbase <- cbind(hopefulbase, spread[,indices,perm[j]])
    }
    if(det(t(hopefulbase)) %% 2 != 0){
      return(spread[,,perm])
    }
    else{
      perm <- sample(1:ddd[3], ddd[3])
    }
  }
}
