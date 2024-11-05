# specified dv and IDs ----
test_that("specified dv and IDs", {
  s <- sim_mixed_df(fr4, 10, 10, "rating", "rater_id", "face_id")
  default_names <- c("sub_id", "item_id", "y", "grand_i", "sub_i", "item_i", "err")
  
  expect_equal(nrow(s), 100)
  expect_equal(ncol(s), 7)
  expect_equal(names(s), default_names)
})

test_that("numeric spec", {
  s <- sim_mixed_df(fr4, dv = 1, sub_id = 2, item_id = 3)
  default_names <- c("sub_id", "item_id", "y", "grand_i", "sub_i", "item_i", "err")
  
  expect_equal(nrow(s), nrow(fr4))
  expect_equal(ncol(s), 7)
  expect_equal(names(s), default_names)
})

# seed ----
# test_that("seed", {
#   df1 <- sim_mixed_df(fr4, 10, 10, "rating", "rater_id", "face_id", seed = 1)
#   df2 <- sim_mixed_df(fr4, 10, 10, "rating", "rater_id", "face_id", seed = 1)
#   
#   expect_equal(df1, df2)
#   
#   df3 <- sim_mixed_df(fr4, 10, 10, "rating", "rater_id", "face_id", seed = 90210)
#   
#   expect_true(!identical(df1, df3))
# })

# exact items ----
test_that("exact items", {
  check <- check_mixed_design(fr4, "rating", "rater_id", "face_id")
  
  # 20 new subjects with same 10 items
  df_items <- sim_mixed_df(fr4, sub_n = 20, item_n = NULL, "rating", "rater_id", "face_id")
  items <- dplyr::count(df_items, item_id, item_i)
  check_items <- check$random_effects$face_id
  
  expect_equal(items$item_i, check_items[,1])
  
  # 20 new items with same 10 subjects
  df_subs <- sim_mixed_df(fr4, sub_n = NULL, item_n = 20, "rating", "rater_id", "face_id")
  subs <- dplyr::count(df_subs, sub_id, sub_i)
  check_subs <- check$random_effects$rater_id
  
  expect_equal(subs$sub_i, check_subs[,1])
})

