testthat::test_that("Execute Cohort Pathways", {
  testthat::expect_error(CohortPathways:::createIfNotExist(type = "file"))
  testthat::expect_error(CohortPathways:::createIfNotExist(type = "file", name = NULL))
  testthat::expect_no_error(CohortPathways:::createIfNotExist(type = "folder", name = file.path(
    tempdir(),
    tempfile()
  )))
})
