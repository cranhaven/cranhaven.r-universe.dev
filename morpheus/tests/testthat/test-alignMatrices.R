# Helper to generate a random series of matrices to align
.generateMatrices = function(d, K, N, noise)
{
  matrices = list( matrix(runif(d*K, min=-1, max=1),ncol=K) ) #reference
  for (i in 2:(N+1))
  {
    matrices[[i]] <- matrices[[1]][,sample(1:K)]
    if (noise)
      matrices[[i]] = matrices[[i]] + matrix(rnorm(d*K, sd=0.05), ncol=K)
  }
  matrices
}

test_that("labelSwitchingAlign correctly aligns de-noised parameters",
{
  N <- 30 #number of matrices
  d_K_list <- list(c(2,2), c(5,3))
  for (i in 1:2)
  {
    d <- d_K_list[[i]][1]
    K <- d_K_list[[i]][2]

    # 1] Generate matrix series
    Ms <- .generateMatrices(d, K, N, noise=FALSE)

    # 2] Call align function with mode=approx1
    aligned <- alignMatrices(Ms[2:(N+1)], ref=Ms[[1]], ls_mode="approx1")

    # 3] Check alignment
    for (j in 1:N)
      expect_equal(aligned[[j]], Ms[[1]])

    # 2bis] Call align function with mode=approx2
    aligned <- alignMatrices(Ms[2:(N+1)], ref=Ms[[1]], ls_mode="approx2")

    # 3bis] Check alignment
    for (j in 1:N)
      expect_equal(aligned[[j]], Ms[[1]])
  }
})

test_that("labelSwitchingAlign correctly aligns noisy parameters",
{
  N <- 30 #number of matrices
  d_K_list <- list(c(2,2), c(5,3))
  for (i in 1:2)
  {
    d <- d_K_list[[i]][1]
    K <- d_K_list[[i]][2]
    max_error <- d * 0.2 #TODO: what value to choose ?

    # 1] Generate matrix series
    Ms <- .generateMatrices(d, K, N, noise=TRUE)

    # 2] Call align function
    aligned <- alignMatrices(Ms[2:(N+1)], ref=Ms[[1]], ls_mode="exact")

    # 3] Check alignment
    for (j in 2:N)
      expect_lt( norm(aligned[[j]] - Ms[[1]]), max_error )
  }
})
