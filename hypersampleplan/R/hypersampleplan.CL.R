# This program is used for computing Confidence Limits for hypergeometric distribution
# k <- the number of defective items k in the lot
# n <- sample size; N <- lot size; x <- observed number of defective units
# gamma <- for one-sided confidence limits (CL), lower CL is gamma while upper CL is 1-gamma
# 1-alpah <- two-sided confidence limits <- upper CL - lower CL == 1-alpha
# The confidence limits are directly showed on R console given (n, x, N)
# algorithm: fi(x, n) --> Si(x, n) --> cumulative hypergeometric probabilities

hypersampleplan.CL <- function (n, x, N)
{
  # compute fi(x, n)
  M <- vector(length=n+1)
  # c <- 0
  c <- 0
  M[1] <- 1
  M[2] <- 1-2*c/n
  for (i in 1:length(M)) {
    M[i+2] <- (((2*i+1)*(n-2*c)*M[i+1])-(i*(n+i+1)*M[i]))/((i+1)*(n-i))
    length(M) <- n+1
    i <- i+1
  }
  m <- vector(length=n+1)
  # c <- goes from 1 to n
  for (c in 1:n) {
    m[1] <- 1
    m[2] <- 1-2*c/n
    for (i in 1:length(m)) {
      m[i+2] <- (((2*i+1)*(n-2*c)*m[i+1])-(i*(n+i+1)*m[i]))/((i+1)*(n-i))
      length(m) <- n+1
      i <- i+1
    }
    # combine to form a (n+1)*(n+1) matrix
    M <- rbind(M,m)
    next
    c <- c+1
  }
  # compute Si(x, n) <- partial sum of fi(x, n) for obtaining upper and lower CL
  SA <- M
  SB <- M
  for(i in 1:(n+1)) {
    SA[,i] <- cumsum(SA[,i])
    SB[,i] <- (sum(SB[,i]))-(cumsum(SB[,i]))
    i <- i+1
  }
  # compute Si(x, n)/||ei^2||
  for (i in 1:(n+1)) {
    SA[,i] <- SA[,i]/(sum(M[,i]^2))
    SB[,i] <- SB[,i]/(sum(M[,i]^2))
    i <- i+1
  }
  SA <- t(SA)
  SB <- t(SB)
  # compute fi(M, N)
  V <- vector(length=n+1)
  # n <- 0
  c <- 0
  V[1] <- 1
  V[2] <- 1-2*c/N
  for (i in 1:(n+1)) {
    V[i+2] <- (((2*i+1)*(N-2*c)*V[i+1])-(i*(N+i+1)*V[i]))/((i+1)*(N-i))
    length(V) <- n+1
    i <- i+1
  }
  v <- vector(length=n+1)
  # n <- goes from 1 to N
  for (c in 1:N) {
    v[1] <- 1
    v[2] <- 1-2*c/N
    for (i in 1:(n+1)) {
      v[i+2] <- (((2*i+1)*(N-2*c)*v[i+1])-(i*(N+i+1)*v[i]))/((i+1)*(N-i))
      length(v) <- n+1
      i <- i+1
    }
    # combine to form a (N+1)*(n+1) matrix
    V <- rbind(V,v)
    next
    c <- c+1
  }
  # compute for cumulative hypergeometric probabilities
  # compute for upper confidence limits
  Y <- matrix(data=1, nrow=N+1, ncol=n+1)
  X.upper <- V %*% SA
  Z.upper <- Y-X.upper
  Z.upper[Z.upper <= 0.000001] <- 0
  # compute for lower confidence limits
  X.lower <- V %*% SB
  Z.lower <- Y-X.lower
  Z.lower <- Y-Z.lower
  Z.lower[Z.lower <= 0.000001] <- 0
  # combine to form a table to show the results
  i <- 1
  j <- 1
  z.upper <- as.vector(c((i-1), (j-1), Z.upper[i,j]))
  upper <- vector(length=3)
  for (i in 1:(N+1)) {
    if (Z.upper[i,x+1] <= 0.9800 & Z.upper[i,x+1] >= 0.9500) {
      upper[1] <- i-1
      upper[2] <- x
      upper[3] <- Z.upper[i,x+1]
      z.upper <- rbind(z.upper, upper)
    }
    i <- i+1
  }
  z.upper <- z.upper[-1,]
  nrow.upper <- nrow(z.upper)
  rownames(z.upper) <- c(paste("CL.Upper.", 1:(nrow.upper), sep=""))
  i <- 1
  j <- 1
  z.lower <- as.vector(c((i-1), j, Z.lower[i,j]))
  lower <- vector(length=3)
  for (i in 1:(N+1)) {
    if (Z.lower[i,x] <= 0.050 & Z.lower[i,x] >= 0.020) {
      lower[1] <- i-1
      lower[2] <- x
      lower[3] <- Z.lower[i,x]
      z.lower <- rbind(z.lower,lower)
    }
    i <- i+1
  }
  z.lower <- z.lower[-1,]
  nrow.lower <- nrow(z.lower)
  rownames(z.lower) <- c(paste("CL.Lower.", 1:(nrow.lower), sep=""))
  hyper.CL <- rbind(z.lower, z.upper)
  colnames(hyper.CL) <- c("n", "k", "Conf.Limits")
  # output data to R console
  return(hyper.CL)
}

