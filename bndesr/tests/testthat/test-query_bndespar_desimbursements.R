
df <- query_bndespar_desimbursements()

test_that("Check row number", {
  expect_gt(nrow(df), 551)
})
