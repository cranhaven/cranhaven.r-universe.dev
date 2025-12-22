test_that("sample_data works", {
   run_dir <-  tempdir()
  if (dir.exists(run_dir)) {
    unlink(run_dir,TRUE,TRUE)
  }
  dir.create(run_dir)
  #when package is installed files in inst go to r library/package

  model1_dir <- testthat::test_path("../test_files", "model_files", "model1")
  model2_dir <- testthat::test_path("../test_files", "model_files", "model2")

  file.copy(from = model1_dir, to = run_dir, recursive = TRUE)
  file.copy(from = model2_dir, to = run_dir, recursive = TRUE)

  #Replace datafile path in model files

  data_file <- test_path("../test_files", "data_seq.csv")

  file.copy(from = data_file, to = run_dir, overwrite = TRUE)
  data_file <- gsub("\\\\", "/", file.path(run_dir, "data_seq.csv"))
  test_path("../test_files", "model_files", "model1")
  #data_file <- gsub("\\\\", "/",  file.path("u:","fda","mbbe","tests","test_files", "data_seq.csv"))
  new_data_file <-   gsub("\\\\", "/", file.path(run_dir, "data_seq.csv"))
  #file.path(run_dir, "data_seq.csv")
  # need "\\\\: instead of "\\:
  #new_data_file = sub("\\","\\\\",new_data_file)
  model1_file <- file.path(run_dir, "model1", "bs1.mod")
  model1_lines <- suppressWarnings(readLines(model1_file))
  model2_file <- file.path(run_dir, "model2", "bs2.mod")
  model2_lines <-  suppressWarnings(readLines(model2_file))
  model1_lines[4] <- sub("\\$DATA[[:space:]]*(.*?)[[:space:]]*IGNORE=@",
               paste0("$DATA ", new_data_file, " IGNORE=@"),
               model1_lines[4])
  model2_lines[5] <- sub("\\$DATA[[:space:]]*(.*?)[[:space:]]*IGNORE=@",
                         paste0("$DATA ", new_data_file, " IGNORE=@"),
                         model2_lines[5])
  writeLines(model1_lines, model1_file)
  writeLines(model2_lines, model2_file)
  set.seed(1)
  mbbe:::sample_data(run_dir, nmodels = 2, samp_size = 4)

  #read in reference files
  data_samp <- list()
  for (i in 1:4) {
    data_samp[[paste0("data_samp",i)]] <-
      read.csv(
       # file.path("u:","fda","mbbe","tests",
        file.path(test_path(".."),
          "test_files",
          "reference_data",
          "sample_data",
          paste0("data_samp", i, ".csv")
        )
      )
  }

  testthat::expect_equal(data_samp$data_samp1, read.csv(file.path(run_dir, "data_samp1.csv")))
  testthat::expect_equal(data_samp$data_samp2, read.csv(file.path(run_dir, "data_samp2.csv")))
  testthat::expect_equal(data_samp$data_samp3, read.csv(file.path(run_dir, "data_samp3.csv")))
  testthat::expect_equal(data_samp$data_samp4, read.csv(file.path(run_dir, "data_samp4.csv")))

  unlink(run_dir, recursive = TRUE)
})
