test_that("HMM for haplotypes are loaded correctly", {
  # Load HMM from RData
  hmm.file <- system.file("extdata", "haplotypes_hmm.RData", package = "SNPknock")
  load(hmm.file)
  # Load fastPHASE parameter estimates from text files
  r.file <- system.file("extdata", "haplotypes_rhat.txt", package = "SNPknock")
  alpha.file <- system.file("extdata", "haplotypes_alphahat.txt", package = "SNPknock")
  theta.file <- system.file("extdata", "haplotypes_thetahat.txt", package = "SNPknock")
  char.file <- system.file("extdata", "haplotypes_origchars", package = "SNPknock")
  hmm.compact <- loadHMM(r.file, alpha.file, theta.file, char.file)
  hmm.large <- loadHMM(r.file, alpha.file, theta.file, char.file, compact=FALSE, phased=TRUE)
  # Verify that the parameters are identical
  testthat::expect_equal(hmm.hap$r, hmm.compact$r)
  testthat::expect_equal(hmm.hap$alpha, hmm.compact$alpha)
  testthat::expect_equal(hmm.hap$theta, hmm.compact$theta)
  testthat::expect_equal(hmm.hap$pInit, hmm.large$pInit)
  testthat::expect_equal(hmm.hap$Q, hmm.large$Q)
  testthat::expect_equal(hmm.hap$pEmit, hmm.large$pEmit)
})

test_that("HMM for genotypes are loaded correctly", {
  # Load HMM from RData
  hmm.file <- system.file("extdata", "genotypes_hmm.RData", package = "SNPknock")
  load(hmm.file)
  # Load fastPHASE parameter estimates from text files
  r.file <- system.file("extdata", "genotypes_rhat.txt", package = "SNPknock")
  alpha.file <- system.file("extdata", "genotypes_alphahat.txt", package = "SNPknock")
  theta.file <- system.file("extdata", "genotypes_thetahat.txt", package = "SNPknock")
  char.file <- system.file("extdata", "genotypes_origchars", package = "SNPknock")
  hmm.compact <- loadHMM(r.file, alpha.file, theta.file, char.file)
  hmm.large <-  loadHMM(r.file, alpha.file, theta.file, char.file, compact=FALSE, phased=FALSE)
  hmm.large$Q <- hmm.large$Q[1:4,,]
  hmm.large$pEmit <- hmm.large$pEmit[1:5,,]
  # Verify that the parameters are identical
  testthat::expect_equal(hmm.gen$r, hmm.compact$r)
  testthat::expect_equal(hmm.gen$alpha, hmm.compact$alpha)
  testthat::expect_equal(hmm.gen$theta, hmm.compact$theta)
  testthat::expect_equal(hmm.gen$pInit, hmm.large$pInit)
  testthat::expect_equal(hmm.gen$Q, hmm.large$Q)
  testthat::expect_equal(hmm.gen$pEmit, hmm.large$pEmit)
})

test_that("Haplotypes are correctly converted into INP format", {
  # Load haplotypes from RData and convert into INP
  hap.file <- system.file("extdata", "haplotypes.RData", package = "SNPknock")
  load(hap.file)
  inp.file <- writeXtoInp(H, phased=T)
  # Stored INP file
  inp.file.stored <- system.file("extdata", "haplotypes.inp", package = "SNPknock")
  testthat::expect_equal(readLines(inp.file), readLines(inp.file.stored))
})

test_that("Genotypes are correctly converted into INP format", {
  # Load genotypes from RData and convert into INP
  gen.file <- system.file("extdata", "genotypes.RData", package = "SNPknock")
  load(gen.file)
  inp.file <- writeXtoInp(X, phased=F)
  # Stored INP file
  inp.file.stored <- system.file("extdata", "genotypes.inp", package = "SNPknock")
  testthat::expect_equal(readLines(inp.file), readLines(inp.file.stored))
})
