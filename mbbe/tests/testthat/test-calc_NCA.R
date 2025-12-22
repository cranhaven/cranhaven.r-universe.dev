test_that("calc_NCA works", {
  run_dir <- tempdir()
  if (dir.exists(run_dir)) {
    unlink(run_dir,TRUE,TRUE)
  }
  dir.create(run_dir)

  source_dir <- testthat::test_path("..","test_files", "calc_nca")
  dir.create(file.path(run_dir,"MBBEsim1"))
  dir.create(file.path(run_dir,"MBBEsim2"))
  file.copy(file.path(source_dir, "MBBEsim1","OUT.DAT"),
            file.path(run_dir,"MBBEsim1"))
  file.copy(file.path(source_dir, "MBBEsim2","OUT.DAT"),
            file.path(run_dir,"MBBEsim2"))
  nca_ref1 <- read.csv(file.path(source_dir,"MBBEsim1","NCAresults1.csv"))
  nca_ref2 <- read.csv(file.path(source_dir,"MBBEsim2","NCAresults2.csv"))
  calc_NCA(run_dir, 4, c(1,2), c(3,4), 72, 2)
  nca_test1  <- read.csv(file.path(run_dir,"MBBEsim1","NCAresults1.csv"))
  nca_test2  <- read.csv(file.path(run_dir,"MBBEsim2","NCAresults2.csv"))
  testthat::expect_equal(nca_test1, nca_ref1)
  testthat::expect_equal(nca_test2, nca_ref2)

  unlink(run_dir, recursive = TRUE)
})
