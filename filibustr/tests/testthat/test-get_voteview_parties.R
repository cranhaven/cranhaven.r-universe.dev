test_that("all parties data", {
  all_parties <- get_voteview_parties()
  expect_s3_class(all_parties, "tbl_df")
  expect_length(all_parties, 9)
  expect_equal(levels(all_parties$chamber),
               c("President", "House", "Senate"))
  expect_equal(unique(all_parties$congress), 1:current_congress())
})

test_that("filter parties by congress", {
  parties_99_101 <- get_voteview_parties(congress = 99:101)
  expect_s3_class(parties_99_101, "tbl_df")
  expect_length(parties_99_101, 9)
  expect_equal(nrow(parties_99_101), 15)
  expect_equal(unique(parties_99_101$congress), 99:101)

  curr_parties <- get_voteview_parties(congress = current_congress())
})
