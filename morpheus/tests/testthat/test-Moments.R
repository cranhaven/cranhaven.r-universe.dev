test_that("both versions of Moments_Mi agree on various inputs",
{
  for (n in c(20,200))
  {
    for (d in c(2,10,20))
    {
      E <- diag(d)
      Id <- as.double(E)
      X <- matrix( rnorm(n*d), nrow=n )
      Y <- rbinom(n, 1, .5)
      M2 <- as.double(.Moments_M2(X,Y))
      M2_R <- colMeans(Y * t( apply(X, 1, function(Xi) Xi %o% Xi - Id) ))
      expect_equal(max(abs(M2 - M2_R)), 0)
      M3 <- as.double(.Moments_M3(X,Y))
      M3_R <- colMeans(Y * t( apply(X, 1, function(Xi) {
        return (Xi %o% Xi %o% Xi -
          Reduce('+', lapply(1:d, function(j)
            as.double(Xi %o% E[j,] %o% E[j,])), rep(0, d*d*d)) -
          Reduce('+', lapply(1:d, function(j)
            as.double(E[j,] %o% Xi %o% E[j,])), rep(0, d*d*d)) -
          Reduce('+', lapply(1:d, function(j)
            as.double(E[j,] %o% E[j,] %o% Xi)), rep(0, d*d*d))) } ) ))
      expect_equal(max(abs(M3 - M3_R)), 0)
    }
  }
})
