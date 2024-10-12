naive_f <- function(link, M1,M2,M3, p,beta,b)
{
  d <- length(M1)
  K <- length(p)
  lambda <- sqrt(colSums(beta^2))

  # Compute beta x2,3 (self) tensorial products
  beta2 <- array(0, dim=c(d,d,K))
  beta3 <- array(0, dim=c(d,d,d,K))
  for (k in 1:K)
  {
    for (i in 1:d)
    {
      for (j in 1:d)
      {
        beta2[i,j,k] = beta[i,k]*beta[j,k]
        for (l in 1:d)
          beta3[i,j,l,k] = beta[i,k]*beta[j,k]*beta[l,k]
      }
    }
  }

  res <- 0
  for (i in 1:d)
  {
    term <- 0
    for (k in 1:K)
      term <- term + p[k]*.G(link,1,lambda[k],b[k])*beta[i,k]
    res <- res + (term - M1[i])^2
    for (j in 1:d)
    {
      term <- 0
      for (k in 1:K)
        term <- term + p[k]*.G(link,2,lambda[k],b[k])*beta2[i,j,k]
      res <- res + (term - M2[i,j])^2
      for (l in 1:d)
      {
        term <- 0
        for (k in 1:K)
          term <- term + p[k]*.G(link,3,lambda[k],b[k])*beta3[i,j,l,k]
        res <- res + (term - M3[i,j,l])^2
      }
    }
  }
  res
}

# TODO: understand why delta is so large (should be 10^-6 10^-7 ...)
test_that("naive computation provides the same result as vectorized computations",
{
  h <- 1e-7 #for finite-difference tests
  n <- 10
  for (dK in list( c(2,2), c(5,3)))
  {
    d <- dK[1]
    K <- dK[2]

    M1 <- runif(d, -1, 1)
    M2 <- matrix(runif(d^2, -1, 1), ncol=d)
    M3 <- array(runif(d^3, -1, 1), dim=c(d,d,d))

    for (link in c("logit","probit"))
    {
      # X and Y are unused here (W not re-computed)
      op <- optimParams(X=matrix(runif(n*d),ncol=d), Y=rbinom(n,1,.5),
        K, link, M=list(M1,M2,M3))
      op$W <- diag(d + d^2 + d^3)

      for (var in seq_len((2+d)*K-1))
      {
        p <- runif(K, 0, 1)
        p <- p / sum(p)
        beta <- matrix(runif(d*K,-5,5),ncol=K)
        b <- runif(K, -5, 5)
        x <- c(p[1:(K-1)],as.double(beta),b)

        # Test functions values (TODO: 1 is way too high)
        expect_equal( op$f(x)[1], naive_f(link,M1,M2,M3, p,beta,b), tolerance=1 )

        # Test finite differences ~= gradient values
        dir_h <- rep(0, (2+d)*K-1)
        dir_h[var] = h
        expect_equal( op$grad_f(x)[var], ((op$f(x+dir_h) - op$f(x)) / h)[1], tolerance=0.5 )
      }
    }
  }
})

test_that("W computed in C and in R are the same",
{
  tol <- 1e-8
  n <- 10
  for (dK in list( c(2,2))) #, c(5,3)))
  {
    d <- dK[1]
    K <- dK[2]
    link <- ifelse(d==2, "logit", "probit")
    theta <- list(
      p=rep(1/K,K),
      beta=matrix(runif(d*K),ncol=K),
      b=rep(0,K))
    io <- generateSampleIO(n, theta$p, theta$beta, theta$b, link)
    X <- io$X
    Y <- io$Y
    dd <- d + d^2 + d^3
    p <- theta$p
    beta <- theta$beta
    lambda <- sqrt(colSums(beta^2))
    b <- theta$b
    beta2 <- apply(beta, 2, function(col) col %o% col)
    beta3 <- apply(beta, 2, function(col) col %o% col %o% col)
    M <- c(
      beta  %*% (p * .G(link,1,lambda,b)),
      beta2 %*% (p * .G(link,2,lambda,b)),
      beta3 %*% (p * .G(link,3,lambda,b)))
    Id <- as.double(diag(d))
    E <- diag(d)
    v1 <- Y * X
    v2 <- Y * t( apply(X, 1, function(Xi) Xi %o% Xi - Id) )
    v3 <- Y * t( apply(X, 1, function(Xi) { return (Xi %o% Xi %o% Xi
      - Reduce('+', lapply(1:d, function(j)
        as.double(Xi %o% E[j,] %o% E[j,])), rep(0, d*d*d))
      - Reduce('+', lapply(1:d, function(j)
        as.double(E[j,] %o% Xi %o% E[j,])), rep(0, d*d*d))
      - Reduce('+', lapply(1:d, function(j)
        as.double(E[j,] %o% E[j,] %o% Xi)), rep(0, d*d*d))) } ) )
    Omega1 <- matrix(0, nrow=dd, ncol=dd)
    for (i in 1:n)
    {
      gi <- t(as.matrix(c(v1[i,], v2[i,], v3[i,]) - M))
      Omega1 <- Omega1 + t(gi) %*% gi / n
    }
    W <- matrix(0, nrow=dd, ncol=dd)
    Omega2 <- matrix( .C("Compute_Omega",
      X=as.double(X), Y=as.integer(Y), M=as.double(M),
      pnc=as.integer(1), pn=as.integer(n), pd=as.integer(d),
      W=as.double(W), PACKAGE="morpheus")$W, nrow=dd, ncol=dd )
    rg <- range(Omega1 - Omega2)
    expect_equal(rg[1], rg[2], tolerance=tol)
  }
})
