test_that("calc_power works", {

   run_dir <-  tempdir()
  if (dir.exists(run_dir)) {
    unlink(run_dir,TRUE,TRUE)
  }
  dir.create(run_dir)
  # copy everything to run_dir


  source_dir <- testthat::test_path("../test_files", "calc_power")
  dir.create(file.path(run_dir,"MBBEsim1"))
  dir.create(file.path(run_dir,"MBBEsim2"))

  file.copy(file.path(source_dir, "MBBEsim1","NCAresults1.csv"),
            file.path(run_dir,"MBBEsim1"))
  file.copy(file.path(source_dir, "MBBEsim2","NCAresults2.csv"),
            file.path(run_dir,"MBBEsim2"))

  referencepower <- read.csv(file.path(source_dir, "All_results.csv"))[,-1] # only 2 samples, remove first column

  testpower <-  calc_power(run_dir, 2, alpha = 0.05,  model_averaging_by = "study", NTID = FALSE)
  # need to convert sample_num to integer
  testpower <- testpower %>% dplyr::mutate(SampleNum = as.integer(SampleNum ))
  testthat::expect_equal(referencepower, testpower)

  unlink(run_dir, recursive = TRUE)
})
