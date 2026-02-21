test_that("multiplication works", {
  path = tempfile()
  vascr_save(growth.df, path = path)
  
  expect_snapshot(load(path, envir = .GlobalEnv, verbose = TRUE))
  
  
})
