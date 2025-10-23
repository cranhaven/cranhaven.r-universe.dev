
test_that("ORDER",{
  res <- devRateFind(orderSP = "Lepidoptera")
  expect_is(res, "data.frame")
})

test_that("FAMILY",{
  res <- devRateFind(familySP = "Gelechiidae")
  expect_is(res, "data.frame")
})

test_that("SPECIES",{
  res <- devRateFind(species = "Tuta absoluta")
  expect_is(res, "data.frame")
})

test_that("ERROR",{
  res <- devRateFind(
    familySP = c("Gelechiidae", "Notuidae"))
  expect_match(
    res,
    "Error in arguments provided: only one argument of type character is allowed")
})
