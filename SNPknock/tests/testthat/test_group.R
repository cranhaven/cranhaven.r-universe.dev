test_that("Group knockoffs for haplotypes are consistent with group HMM knockoffs", {
  # Parameters
  p = 200
  n = 1000
  group.size = 11

  # Load HMM
  r_file <- system.file("extdata", "haplotypes_rhat.txt", package = "SNPknock")
  alpha_file <- system.file("extdata", "haplotypes_alphahat.txt", package = "SNPknock")
  theta_file <- system.file("extdata", "haplotypes_thetahat.txt", package = "SNPknock")
  char_file <- system.file("extdata", "haplotypes_origchars", package = "SNPknock")
  hmm.large <- loadHMM(r_file, alpha_file, theta_file, char_file, compact=FALSE, phased=TRUE)
  hmm.large$Q = hmm.large$Q[1:(p-1),,]
  hmm.large$pEmit = hmm.large$pEmit[1:p,,]
  hmm.small <- loadHMM(r_file, alpha_file, theta_file, char_file)
  hmm.small$r = hmm.small$r[1:p]
  hmm.small$alpha = hmm.small$alpha[1:p,]
  hmm.small$theta = hmm.small$theta[1:p,]

  # Define groups
  groups = sample(p/group.size,p,replace=T)

  # Sample X from this HMM
  X = sampleHMM(hmm.large$pInit, hmm.large$Q, hmm.large$pEmit, n=n)
  # Generate group knockoffs with general algorithm
  Xk <- knockoffHMM(X, hmm.large$pInit, hmm.large$Q, hmm.large$pEmit, groups=groups, display_progress=F)
  # Generate group knockoffs with specialized algorithm
  Xk.hap <- knockoffHaplotypes(X, hmm.small$r, hmm.small$alpha, hmm.small$theta, groups=groups, display_progress=F)

  # Verify that the knockoffs are identical
  testthat::expect_equal(Xk, Xk.hap)
})

test_that("Group knockoffs for genotypes are consistent with group HMM knockoffs", {
  # Parameters
  p = 200
  n = 1000
  group.size = 11

  # Load HMM
  r_file <- system.file("extdata", "haplotypes_rhat.txt", package = "SNPknock")
  alpha_file <- system.file("extdata", "haplotypes_alphahat.txt", package = "SNPknock")
  theta_file <- system.file("extdata", "haplotypes_thetahat.txt", package = "SNPknock")
  char_file <- system.file("extdata", "haplotypes_origchars", package = "SNPknock")
  hmm.large <- loadHMM(r_file, alpha_file, theta_file, char_file, compact=FALSE, phased=FALSE)
  hmm.large$Q = hmm.large$Q[1:(p-1),,]
  hmm.large$pEmit = hmm.large$pEmit[1:p,,]
  hmm.small <- loadHMM(r_file, alpha_file, theta_file, char_file)
  hmm.small$r = hmm.small$r[1:p]
  hmm.small$alpha = hmm.small$alpha[1:p,]
  hmm.small$theta = hmm.small$theta[1:p,]

  # Define groups
  groups = sample(p/group.size,p,replace=T)

  # Sample X from this HMM
  X = sampleHMM(hmm.large$pInit, hmm.large$Q, hmm.large$pEmit, n=n)
  # Generate group knockoffs with general algorithm
  Xk <- knockoffHMM(X, hmm.large$pInit, hmm.large$Q, hmm.large$pEmit, groups=groups, display_progress=F)
  # Generate group knockoffs with specialized algorithm
  Xk.gen <- knockoffGenotypes(X, hmm.small$r, hmm.small$alpha, hmm.small$theta, groups=groups, display_progress=F)

  # Verify that the knockoffs are identical
  testthat::expect_equal(Xk, Xk.gen)
})
