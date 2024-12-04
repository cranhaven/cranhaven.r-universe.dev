# This program is used for obtaining c according to pre-specified alpha, beta, k1, k2, n and N
# by Hypergeometric Distribution
# Producer's risk alpha (a) <- the probability of rejecting a lot for proportion defective p1
# Consumer's risk beta (b) <- the probability of accepting a lot for proportion defective p2
# n <- sample size; c <- number of defective units allowed in a lot which is accepted
# N <- lot size; k1 <- p1*N where p1 <- proportion defective associated with alpha
# N <- lot size; k2 <- p2*N where p2 <- proportion defective associated with beta
# algorithm: fi(x, n) --> Si(x, n) --> cumulative hypergeometric probabilities

hypersampleplan.fixedn <- function (a, b, k1, k2, n, N)
{
  # compute fi(x, n)
  M <- vector(length=k1+1)
  # c <- 0
  x <- 0
  M[1] <- 1
  M[2] <- 1-2*x/k1
  for (i in 1:length(M)) {
    M[i+2] <- (((2*i+1)*(k1-2*x)*M[i+1])-(i*(k1+i+1)*M[i]))/((i+1)*(k1-i))
    length(M) <- k1+1
    i <- i+1
  }
  m <- vector(length=k1+1)
  # c <- goes from 1 to k1
  for (x in 1:k1) {
    m[1] <- 1
    m[2] <- 1-2*x/k1
    for (i in 1:length(m)) {
      m[i+2] <- (((2*i+1)*(k1-2*x)*m[i+1])-(i*(k1+i+1)*m[i]))/((i+1)*(k1-i))
      length(m) <- k1+1
      i <- i+1
    }
    # combine to form a (k1+1)*(k1+1) matrix
    M <- rbind(M,m)
    next
    x <- x+1
  }
  # compute Si(x, n) <- partial sum of fi(x, n)
  S <- M
  for(i in 1:(k1+1)) {
    S[,i] <- cumsum(S[,i])
    i <- i+1
  }
  # compute Si(x, n)/||ei^2||
  for (i in 1:(k1+1)) {
    S[,i] <- S[,i]/(sum(M[,i]^2))
    i <- i+1
  }
  # transpose S
  S <- t(S)
  # compute fi(M, N)
  V <- vector(length=k1+1)
  # n <- 0
  x <- 0
  V[1] <- 1
  V[2] <- 1-2*x/N
  for (i in 1:(k1+1)) {
    V[i+2] <- (((2*i+1)*(N-2*x)*V[i+1])-(i*(N+i+1)*V[i]))/((i+1)*(N-i))
    length(V) <- k1+1
    i <- i+1
  }
  v <- vector(length=k1+1)
  # n <- goes from 1 to N
  for (x in 1:N) {
    v[1] <- 1
    v[2] <- 1-2*x/N
    for (i in 1:(k1+1)) {
      v[i+2] <- (((2*i+1)*(N-2*x)*v[i+1])-(i*(N+i+1)*v[i]))/((i+1)*(N-i))
      length(v) <- k1+1
      i <- i+1
    }
    # combine to form a (N+1)*(k1+1) matrix
    V <- rbind(V,v)
    next
    x <- x+1
  }
  # compute for cumulative hypergeometric probabilities
  X <- V %*% S
  Y <- matrix(data=1, nrow=N+1, ncol=k1+1)
  Z <- Y-X
  Z[Z <= 0.000001] <- 0
  # output data to make a table containing the values of (n, c, alpha)
  i <- 1
  j <- 1
  z <- as.vector(c(n=i-1, c=j-1, Z[i,j]))
  u <- vector(length=3)
  for (i in 1:(N+1)) {
    for (j in 1:(k1+1)) {
      if (Z[i, j] <= a & Z[i, j] > 0) {
        u[1] <- (i-1)
        u[2] <- (j-1)
        u[3] <- Z[i, j]
        z <- rbind(z, u)
      }
      j <- j+1
    }
    next
    i <- i+1
  }
  # compute the consumer's risk beta
  # compute fi(x, n)
  MA <- vector(length=k2+1)
  # c <- 0
  x <- 0
  MA[1] <- 1
  MA[2] <- 1-2*x/k2
  for (i in 1:length(MA)) {
    MA[i+2] <- (((2*i+1)*(k2-2*x)*MA[i+1])-(i*(k2+i+1)*MA[i]))/((i+1)*(k2-i))
    length(MA) <- k2+1
    i <- i+1
  }
  ma <- vector(length=k2+1)
  # c <- goes from 1 to k2
  for (x in 1:k2) {
    ma[1] <- 1
    ma[2] <- 1-2*x/k2
    for (i in 1:length(ma)) {
      ma[i+2] <- (((2*i+1)*(k2-2*x)*ma[i+1])-(i*(k2+i+1)*ma[i]))/((i+1)*(k2-i))
      length(ma) <- k2+1
      i <- i+1
    }
    # combine to form a (k2+1)*(k2+1) matrix
    MA <- rbind(MA,ma)
    next
    x <- x+1
  }
  # compute Si(x, n) <- partial sum of fi(x, n)
  SA <- MA
  for(i in 1:(k2+1)) {
    SA[,i] <- (sum(SA[,i]))-(cumsum(SA[,i]))
    i <- i+1
  }
  # compute Si(x, n)/||ei^2||
  for (i in 1:(k2+1)) {
    SA[,i] <- SA[,i]/(sum(MA[,i]^2))
    i <- i+1
  }
  SA <- t(SA)
  # compute fi(M, N)
  VE <- vector(length=k2+1)
  # n <- 0
  x <- 0
  VE[1] <- 1
  VE[2] <- 1-2*x/N
  for (i in 1:(k2+1)) {
    VE[i+2] <- (((2*i+1)*(N-2*x)*VE[i+1])-(i*(N+i+1)*VE[i]))/((i+1)*(N-i))
    length(VE) <- k2+1
    i <- i+1
  }
  ve <- vector(length=k2+1)
  # n <- goes from 1 to N
  for (x in 1:N) {
    ve[1] <- 1
    ve[2] <- 1-2*x/N
    for (i in 1:(k2+1)) {
      ve[i+2] <- (((2*i+1)*(N-2*x)*ve[i+1])-(i*(N+i+1)*ve[i]))/((i+1)*(N-i))
      length(ve) <- k2+1
      i <- i+1
    }
    # combine to form a (N+1)*(k2+1) matrix
    VE <- rbind(VE,ve)
    next
    x <- x+1
  }
  # compute for cumulative hypergeometric probabilities
  XB <- VE %*% SA
  YB <- matrix(data=1, nrow=N+1, ncol=k2+1)
  ZB <- YB-XB
  ZB[ZB <= 0.000001] <- 0
  # output data to make a table containing the values of (n, c, beta)
  i <- 1
  j <- 1
  zb <- as.vector(c(n=i-1, c=j-1, ZB[i,j]))
  ub <- vector(length=3)
  for (i in 1:(N+1)) {
    for (j in 1:(k2+1)) {
      if (ZB[i, j] <= b & ZB[i, j] > 0) {
        ub[1] <- as.numeric(i-1)
        ub[2] <- as.numeric(j-1)
        ub[3] <- ZB[i, j]
        zb <- rbind(zb, ub)
      }
      j <- j+1
    }
    next
    i <- i+1
  }
  z <- as.matrix(z)
  zb <- as.matrix(zb)
  # combine the table to show the acceptance plan (k1, n, c, alpha, k2, beta)
  plan1.0 <- vector(length=3)
  plan1 <- as.vector(c(n, k1, a))
  for (i in 1:(nrow(z))) {
    if (z[i,1] == n) {
      plan1.0[1] <- n
      plan1.0[2] <- z[i,2]
      plan1.0[3] <- z[i,3]
      plan1 <- rbind(plan1, plan1.0)
    }
    i <- i+1
  }
  plan2.0 <- vector(length=3)
  plan2 <- as.vector(c(n, k2, b))
  for (j in 1:(nrow(zb))) {
    if (zb[j,1] == n) {
      plan2.0[1] <- n
      plan2.0[2] <- zb[j,2]
      plan2.0[3] <- zb[j,3]
      plan2 <- rbind(plan2, plan2.0)
    }
    j <- j+1
  }
  plan1 <- as.matrix(plan1)
  plan2 <- as.matrix(plan2)
  sampling.plan.X <- vector(length=5)
  sampling.plan.parameter <- as.vector(c(n, k1, a, k2, b))
  for (i in 1:(nrow(plan1))) {
    for (j in 1:(nrow(plan2))) {
      if (plan1[i,2] == plan2[j,2]) {
        sampling.plan.X[1] <- n
        sampling.plan.X[2] <- plan1[i,2]
        sampling.plan.X[3] <- plan1[i,3]
        sampling.plan.X[4] <- k2
        sampling.plan.X[5] <- plan2[j,3]
        sampling.plan.parameter <- rbind(sampling.plan.parameter, sampling.plan.X)
      }
      j <- j+1
    }
    next
    i <- i+1
  }
  colnames(sampling.plan.parameter) <- c("n", "c", "alpha", "k2", "beta")
  # output the result to R console
  return(sampling.plan.parameter)
}
