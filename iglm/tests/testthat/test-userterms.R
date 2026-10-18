# library(iglm)
# library(testthat)
# 
# test_that("Test custom term registration", {
#   pkg_path <- file.path(tempdir(), "test_pkg")
#   if (dir.exists(pkg_path)) unlink(pkg_path, recursive = TRUE)
#   dir.create(pkg_path, recursive = TRUE)
#   
#   on.exit({
#     if (dir.exists(pkg_path)) unlink(pkg_path, recursive = TRUE)
#   }, add = TRUE)
# 
#   create_userterms_skeleton(path = pkg_path, pkg_name = "iglm.custom")
#   old_wd <- getwd()
#   on.exit(setwd(old_wd), add = TRUE)
#   setwd(file.path(pkg_path, "iglm.custom"))
#   system("R CMD INSTALL .")
#   
#   library(iglm.custom)
#   data(state_twitter)
#   
#   message("Computing statistics with custom term...")
#   res <- statistics(state_twitter[[1]] ~ mutual + my_mutual)
#   print(res)
#   
#   expect_equal(as.numeric(res[1]), as.numeric(res[2]))
# })
