# ------------------------------------------------------------------------------
# Tests for devRateInfo
# ------------------------------------------------------------------------------
test_that("devRateInfo returns NULL",{
  res <- devRateInfo(eq = taylor_81)
  expect_equal(
    object = res,
    expected = NULL
  )
})

# ------------------------------------------------------------------------------
# Tests for devRatePlotInfo
# ------------------------------------------------------------------------------

test_that("devRatePlotInfo returns NULL",{
  eqOpt <- devRateEqList
  trash <- lapply(eqOpt, function(j){
    sortOpt <- c("ordersp", "familysp", "genussp", "species", "genSp")
    res <- lapply(sortOpt, function(i){
      devRatePlotInfo(
        eq = j,
        sortBy = i,
        xlim = c(0, 40),
        ylim = c(0, 0.05)
      )
    })
    sapply(res, function(i){
      expect_equal(
        object = i,
        expected = NULL
      )
    })
  })
})
