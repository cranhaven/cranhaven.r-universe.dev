test_that("Voteview cast codes", {
  codes <- get_voteview_cast_codes()

  expect_s3_class(codes, "tbl_df")
  expect_length(codes, 2)
  expect_equal(nrow(codes), 10)
  expect_equal(codes$cast_code, 0:9)
  expect_s3_class(codes$vote_cast, "factor")
})
