test_that("Extra local tests", {

  path <- "~/Dropbox (Personal)/projects/ravetools/adhoc/testextra"

  skip_if_not(dir.exists(path))

  fs <- list.files(path, full.names = TRUE, pattern = "\\.R$", ignore.case = TRUE)
  for(f in fs) {
    source(f, local = TRUE, chdir = TRUE)
  }

})
