test_that("ffdEvidenceFSTS", {
  
  skip_on_cran()
  
  # browser()
  
  fMRI.data  <- readRDS(system.file("test_data", 'data_test.Rds', package= "BayesDLMfMRI"))
  covariates <- readRDS(system.file("test_data", 'covariates.Rds', package= "BayesDLMfMRI"))
  result     <- readRDS(system.file("test_data", 'res_FSTS.Rds', package= "BayesDLMfMRI")) 
  
  res <- ffdEvidenceFSTS(ffdc = fMRI.data, covariates = covariates, m0 = 0,
                         Cova = 100, delta = 0.95, S0 = 1,n0 = 1, Nsimu1 = 100, Cutpos1 = 20,
                         r1 = 1, Ncores = 8, seed=444) 
  
  expect_equal(res[[1]] |> as.numeric(), result[[1]] |> as.numeric())
  expect_equal(res[[2]] |> as.numeric(), result[[2]] |> as.numeric())
})
