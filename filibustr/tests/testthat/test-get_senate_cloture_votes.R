test_that("cloture votes", {
  skip_if_offline()

  cloture_votes <- get_senate_cloture_votes()
  expect_s3_class(cloture_votes, "tbl_df")
  expect_length(cloture_votes, 5)
  # number of rows
  expect_equal(nrow(cloture_votes), current_congress() - 64)
  expect_equal(unique(cloture_votes$congress), current_congress():65)

  # increasing cloture action over time
  expect_gt(cor(get_senate_cloture_votes()$congress,
                get_senate_cloture_votes()$motions_filed),
            0.5)
  expect_gt(cor(get_senate_cloture_votes()$congress,
                get_senate_cloture_votes()$votes_on_cloture),
            0.5)
  expect_gt(cor(get_senate_cloture_votes()$congress,
                get_senate_cloture_votes()$cloture_invoked),
            0.5)
})
