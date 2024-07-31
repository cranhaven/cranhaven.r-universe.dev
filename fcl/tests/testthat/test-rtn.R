test_that("make_rtn works", {
  out <- make_rtn(c(210101, 210105, 210110), c(100, 103, 110), c(0, 3, 7))
  cr <- out$twrr_cr(210102, 210110)
  dr <- out$twrr_dr(210102, 210110)
  expect_equal(length(cr), 9L)
  expect_equal(length(dr), 9L)
  expect_equal(cr, cumprod(dr + 1) - 1, ignore_attr = TRUE)
  expect_equal(as.double(cr)[length(cr)], 0.1)

  dietz <- out$dietz(210102, 210110)
  avc <- out$dietz_avc(210102, 210110)
  expect_equal(as.double(dietz)[length(dietz)], 0.1)
  expect_equal(as.double(avc)[length(avc)], 100)
  expect_equal(length(dietz), 9L)
  expect_equal(length(avc), 9L)
})

test_that("make_rtn will check input len", {
  expect_error(
    make_rtn(c(210101, 210105, 210110), c(100, 103, 110), c(0, 3, 7), 1:2),
    "length 1 or 3",
    fixed = TRUE
  )
  out <- make_rtn(c(210101, 210105, 210110), c(100, 103, 110), c(0, 3, 7), 1)
  expect_error(
    out$twrr_cr(210102, 210110, 1:2),
    "must be length 1",
    fixed = TRUE
  )
})

test_that("make_rtn method's id default works", {
  obj <- make_rtn(c(210101, 210105, 210110), c(100, 103, 110), c(0, 3, 7), c(1, 1, 2))
  expect_error(obj$twrr_cr(210102, 210105), "id", fixed = TRUE)
  obj <- make_rtn(c(210101, 210105, 210110), c(100, 103, 110), c(0, 3, 7), 2)
  expect_equal(as.double(obj$twrr_cr(210102, 210105)[4, 1]), 0.03)
})
