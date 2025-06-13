# test for issue #2

test_that("D25071 is reduced to dm from dmcx because D4439 already codes perivasc", {
  cases <- data.frame(id=c(1,1,1,1,2,2,2,2,2,2),
      icd9cm=c("D1970","D20206","D25071","D4439","D25001","D45621","D570","D25071","D4439","D25070"),
      stringsAsFactors=TRUE)
  gen <- generate_comorbidity_df(cases)

  expect_true(gen[1,]$dm)
  expect_false(gen[1,]$dmcx)
  expect_true(gen[2,]$dm)
  expect_false(gen[2,]$dmcx)
})
