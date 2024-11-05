test_that("errors", {
  err <- "data must be a data frame or matrix"
  expect_error(check_mixed_design(1), err)
  expect_error(check_mixed_design("A"), err)
  expect_error(check_mixed_design(FALSE), err)
  expect_error(check_mixed_design(1:10), err)
})

test_that("matrix", {
  des <- check_mixed_design(fr4[,1:3], "rating", "rater_id", "face_id")
  fr4mat <- fr4[,1:3] %>% dplyr::mutate(
    rater_id = gsub("S", "", rater_id) %>% as.numeric(),
    face_id = gsub("I", "", face_id) %>% as.numeric()
  ) %>% as.matrix()
  des2 <- check_mixed_design(fr4mat, 1, 2, 3)
  
  expect_equal(des$grand_i, des2$grand_i)
  expect_equal(des$sub_sd, des2$sub_sd)
  expect_equal(des$item_sd, des2$item_sd)
  expect_equal(des$error_sd, des2$error_sd)
})

test_that("faceratings", {
  des <- check_mixed_design(fr4, "rating", "rater_id", "face_id")
  
  expect_equal(des$grand_i, mean(fr4$rating), tolerance = 1e-06)
  expect_equal(des$sub_sd, 0.8959721, tolerance = 1e-06)
  expect_equal(des$item_sd, 0.6545743, tolerance = 1e-06)
  expect_equal(des$error_sd, 1.04295, tolerance = 1e-06)
  expect_equal(des$random_effects$rater_id$`(Intercept)` %>% length(), 24)
  expect_equal(des$random_effects$face_id$`(Intercept)` %>% length(), 32)
})


test_that("formula", {
  fm <- "rating ~ face_sex + (1 | face_id) + (1 | rater_id)"
  des <- check_mixed_design(fr4, "rating", "rater_id", "face_id", fm)
  
  expect_equal(des$grand_i, 2.854167, tolerance = 1e-06)
  expect_equal(des$sub_sd, 0.8959708, tolerance = 1e-06)
  expect_equal(des$item_sd, 0.640946, tolerance = 1e-06)
  expect_equal(des$error_sd, 1.04295, tolerance = 1e-06)
  expect_equal(des$random_effects$rater_id %>% names(), "(Intercept)")
  expect_equal(des$random_effects$face_id %>% names(), "(Intercept)")
})
