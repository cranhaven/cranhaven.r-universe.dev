y <- system.file("das_sample.das", package = "swfscDAS")

y.read <- das_read(y, skip = 20, tz = "GMT")
y.proc <- das_process(y.read, reset.effort = FALSE)

event.sight <- c("S", "K", "M", "G", "s", "k", "m", "g", "t", "p", "F",
                 "A", "?", 1:8)



test_that("The column classes of the das_sight 'default' output are the same whether or not there is sighting data", {
  y.sight <- dplyr::filter(das_sight(y.proc, return.format = "default"),
                               Event == "ZZ")

  y.sight0 <- y.proc %>%
    filter(!(Event %in% event.sight)) %>%
    das_sight(return.format = "default")

  expect_identical(y.sight, y.sight0)
})


test_that("The column classes of the das_sight 'wide' output, are the same whether or not there is sighting data", {
  y.sight <- dplyr::filter(das_sight(y.proc, return.format = "wide"),
                               Event == "ZZ")

  y.sight0 <- y.proc %>%
    filter(!(Event %in% event.sight)) %>%
    das_sight(return.format = "wide")

  expect_identical(y.sight, y.sight0)
})


test_that("The column classes of the das_sight 'complete' output are the same whether or not there is sighting data", {
  y.sight <- dplyr::filter(das_sight(y.proc, return.format = "complete"),
                               Event == "ZZ")

  y.sight0 <- y.proc %>%
    filter(!(Event %in% event.sight)) %>%
    das_sight(return.format = "complete")

  expect_identical(y.sight, y.sight0)
})
