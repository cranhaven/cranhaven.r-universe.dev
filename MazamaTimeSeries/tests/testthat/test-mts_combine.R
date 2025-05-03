
test_that("input is validated", {

  expect_error(
    mts_combine()
  )

  expect_error(
    suppressWarningMessages({
      mts_combine(example_mts, list("not", "an", "mts"))
    })
  )

})

test_that("simple combination works", {

  # combining an mts with itself should be the original mts
  expect_identical(
    mts_combine(example_mts, example_mts) %>% mts_extractData(),
    example_mts %>% mts_extractData()
  )

  # combining 2
  mts_1 <- mts_filterDate(example_mts, 20190701, 20190703)
  mts_2 <- mts_filterDate(example_mts, 20190703, 20190708)

  expect_identical(
    mts_combine(mts_1, mts_2) %>% mts_extractData(),
    example_mts %>% mts_extractData()
  )

  # combining 3
  mts_1 <- mts_filterDate(example_mts, 20190701, 20190703)
  mts_2 <- mts_filterDate(example_mts, 20190703, 20190706)
  mts_3 <- mts_filterDate(example_mts, 20190706, 20190708)

  expect_identical(
    mts_combine(mts_1, mts_2, mts_3) %>% mts_extractData(),
    example_mts %>% mts_extractData()
  )

  # combining a list
  mts_1 <- mts_filterDate(example_mts, 20190701, 20190703)
  mts_2 <- mts_filterDate(example_mts, 20190703, 20190706)
  mts_3 <- mts_filterDate(example_mts, 20190706, 20190708)
  mtsList <- list(mts_1, mts_2, mts_3)

  expect_identical(
    mts_combine(mtsList) %>% mts_extractData(),
    example_mts %>% mts_extractData()
  )

})

test_that("gaps and overlaps are supported", {

  # gap
  mts_1 <- mts_filterDate(example_mts, 20190701, 20190703)
  mts_2 <- mts_filterDate(example_mts, 20190706, 20190708)

  expect_identical(
    mts_combine(mts_1, mts_2) %>% mts_extractData() %>% dplyr::pull(.data$datetime),
    example_mts %>% mts_extractData() %>% dplyr::pull(.data$datetime)
  )

  # overlap
  mts_1 <- mts_filterDate(example_mts, 20190701, 20190706)
  mts_2 <- mts_filterDate(example_mts, 20190703, 20190708)

  expect_identical(
    mts_combine(mts_1, mts_2) %>% mts_extractData() %>% dplyr::pull(.data$datetime),
    example_mts %>% mts_extractData() %>% dplyr::pull(.data$datetime)
  )

})

test_that("later-is-better is supported", {

  # overlap
  mts_1 <- mts_filterDate(example_mts, 20190701, 20190706)
  mts_2 <- mts_filterDate(example_mts, 20190703, 20190708)

  # new values in mts_2
  id <- names(mts_2$data[6])
  mts_2$data[[id]] <- 9999

  # first 48 hours from mts_1, all other data from mts_2
  mts <- mts_combine(mts_1, mts_2)
  expect_identical(
    mts$data[,id],
    c(mts_1$data[1:48,id], mts_2$data[,id])
  )

  # order of incoming mts objects shouldn't matter
  mts <- mts_combine(mts_2, mts_1)
  expect_identical(
    mts$data[,id],
    c(mts_1$data[1:48,id], mts_2$data[,id])
  )

})

test_that("new columns can be added", {

  ids <- example_mts$meta$deviceDeploymentID

  # overlap
  mts_1 <- mts_select(example_mts, ids[1:10])
  mts_2 <- mts_select(example_mts, ids[11:length(ids)])
  mts <- mts_combine(mts_1, mts_2)

  expect_identical(
    mts %>% mts_extractMeta(),
    example_mts %>% mts_extractMeta()
  )

  expect_identical(
    mts %>% mts_extractData(),
    example_mts %>% mts_extractData()
  )

})

test_that("overlapStragey = 'replace na' works", {

  id <- "f1eb28176ffa3c00_3487"

  # different values
  mts_1 <- mts_select(example_mts, id)
  mts_2 <- mts_select(example_mts, id)

  replaceIndices <- c(50:60, 70, 80, 150:168)
  keepIndices <- setdiff(1:168, replaceIndices)

  mts_1$data[replaceIndices, 2] <- NA
  mts_2$data[, 2] <- mts_2$data[, 2] * 100

  mts <- mts_combine(mts_1, mts_2, overlapStrategy = "replace na")

  # We retained non-missing original values
  expect_identical(
    mts$data[keepIndices, 2],
    mts_1$data[keepIndices, 2]
  )

  # We replaced missing original values
  expect_identical(
    mts$data[replaceIndices, 2],
    mts_2$data[replaceIndices, 2]
  )

})


