# test_that("download from Voteview", {
#   online_votes <- get_voteview_member_votes(local = FALSE)
#   expect_s3_class(online_votes, "tbl_df")
#   expect_length(online_votes, 6)
#   expect_equal(levels(online_votes$chamber), c("House", "Senate"))
#   expect_equal(unique(online_votes$congress), 1:current_congress())
#   expect_gte(min(online_votes$prob, na.rm = TRUE), 0)
#   expect_lte(max(online_votes$prob, na.rm = TRUE), 100)
# })

# test_that("filter votes by chamber", {
#   s_votes <- get_voteview_member_votes(chamber = "s")
#   expect_s3_class(s_votes, "tbl_df")
#   expect_length(s_votes, 6)
#   expect_equal(levels(s_votes$chamber), "Senate")
#   expect_equal(unique(s_votes$congress), 1:current_congress())
#
#   hr_votes <- get_voteview_member_votes(chamber = "hr")
#   expect_s3_class(hr_votes, "tbl_df")
#   expect_length(hr_votes, 6)
#   expect_equal(levels(hr_votes$chamber), "House")
#   expect_equal(unique(hr_votes$congress), 1:current_congress())
#
#   # 4x more members per House vote, so this will hold
#   expect_lt(nrow(s_votes), nrow(hr_votes))
# })

test_that("filter votes by congress", {
  votes_1_5 <- get_voteview_member_votes(congress = 1:5)
  expect_s3_class(votes_1_5, "tbl_df")
  expect_length(votes_1_5, 6)
  expect_equal(levels(votes_1_5$chamber), c("House", "Senate"))
  expect_equal(unique(votes_1_5$congress), 1:5)
  expect_equal(nrow(votes_1_5), 62037)

  votes_108_110 <- get_voteview_member_votes(congress = 108:110)
  expect_s3_class(votes_108_110, "tbl_df")
  expect_length(votes_108_110, 6)
  expect_equal(levels(votes_108_110$chamber), c("House", "Senate"))
  expect_equal(unique(votes_108_110$congress), 108:110)
  expect_equal(nrow(votes_108_110), 2064859)

  # combo of chamber and congress
  s_votes_117 <- get_voteview_member_votes(chamber = "s", congress = 117)
  expect_s3_class(s_votes_117, "tbl_df")
  expect_length(s_votes_117, 6)
  expect_equal(levels(s_votes_117$chamber), "Senate")
  expect_equal(unique(s_votes_117$congress), 117)
  expect_equal(nrow(s_votes_117), 95152)
})
