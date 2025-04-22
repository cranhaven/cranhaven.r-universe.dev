context("Miscellaneous")

test_that("Function List", {
  # read namespace and extract exported functions
  funnamespace = data.table(read.table(system.file("NAMESPACE", package = "CornerstoneR"), stringsAsFactors = FALSE))
  funnamespace[, c("status", "fun") := tstrsplit(V1, "\\(")]
  funnamespace[, fun := tstrsplit(fun, "\\)")]
  # read function list
  funlist = read.csv2(system.file("csdata", "functionList.txt", package = "CornerstoneR"), stringsAsFactors = FALSE)
  # check function list at all
  expect_data_frame(funlist, ncols = 3)
  expect_data_frame(funlist[, 1:2], any.missing = FALSE)
  # compare functions to namespace
  expect_set_equal(funlist[, 1], c(funnamespace[status == "export", fun], "---"))
})

test_that("Internal Version", {
  CSRnews = news(package = "CornerstoneR")
  expect_equal(head(CSRnews$Version, 1), as.character(packageVersion("CornerstoneR")))
})

test_that("Versions Dataset", {
  expect_true(showVersions())
  res = showVersions(return.results = TRUE)
  expect_data_table(res$versions, nrows = 3, ncols = 2)
})