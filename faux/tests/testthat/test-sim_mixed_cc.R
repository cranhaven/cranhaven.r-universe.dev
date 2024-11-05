# default ----
test_that("default", {
  data <- sim_mixed_cc()
  
  expect_equal(nrow(data), 2000)
  expect_equal(ncol(data), 7)
  expect_equal(names(data), c("sub_id", "item_id", "y", "grand_i", "sub_i", "item_i", "err"))
  
  res <- lme4::lmer(y ~ 1 + (1 | sub_id) + (1 | item_id), data = data) %>%
    broom.mixed::tidy(effects = "ran_pars")
  
  expect_equal(res$estimate, c(1,1,1), tolerance = 0.4)
})

# n ----
test_that("n", {
  data <- sim_mixed_cc(100, 100)
  
  expect_equal(nrow(data), 10000)
  expect_equal(ncol(data), 7)
  expect_equal(names(data), c("sub_id", "item_id", "y", "grand_i", "sub_i", "item_i", "err"))
  
  res <- lme4::lmer(y ~ 1 + (1 | sub_id) + (1 | item_id), data = data) %>%
    broom.mixed::tidy(effects = "ran_pars")
  
  expect_equal(res$estimate, c(1,1,1), tolerance = 0.1)
})

# seed ----
test_that("seed", {
  # # setting seed returns same DF, but is reset
  # set.seed(1)
  # rnd0 <- rnorm(1)
  # df1 <- sim_mixed_cc(seed = 910210)
  # rnd1 <- rnorm(1)
  # df2 <- sim_mixed_cc(seed = 910210)
  # rnd2 <- rnorm(1)
  # set.seed(1)
  # rnd0b <- rnorm(1)
  # rnd1b <- rnorm(1)
  # rnd2b <- rnorm(1)
  # df3 <- sim_mixed_cc(seed = 8675309)
  # 
  # expect_equal(df1, df2)
  # expect_false(rnd1 == rnd2)
  # expect_equal(rnd0, rnd0b)
  # expect_equal(rnd1, rnd1b)
  # expect_equal(rnd2, rnd2b)
  # expect_true(!identical(df1, df3))
  
  # user sets seed externally
  set.seed(1)
  df4 <- sim_mixed_cc()
  set.seed(1)
  df5 <- sim_mixed_cc()
  expect_equal(df4, df5)
})

# fixed intercepts ----
test_that("fixed intercepts", {
  # unnamed vectors
  data <- sim_mixed_cc(3, 3, 0, 1:3, 4:6)
  
  sub_ids <- unique(data$sub_id) %>% as.vector()
  item_ids <- unique(data$item_id) %>% as.vector()
  sub_is <- unique(data$sub_i) %>% as.vector()
  item_is <- unique(data$item_i) %>% as.vector()
  
  expect_equal(sub_ids, c("S1", "S2", "S3"))
  expect_equal(item_ids, c("I1", "I2", "I3"))
  expect_equal(sub_is, 1:3)
  expect_equal(item_is, 4:6)
  
  # named vectors
  sub_sd <- c("A" = 1, "B" = 2, "C" = 3)
  item_sd <- c("D" = 4, "E" = 5, "F" = 6)
  
  data <- sim_mixed_cc(3, 3, 0, sub_sd, item_sd)
  
  sub_ids <- unique(data$sub_id) %>% as.vector()
  item_ids <- unique(data$item_id) %>% as.vector()
  sub_is <- unique(data$sub_i) %>% as.vector()
  item_is <- unique(data$item_i) %>% as.vector()
  
  expect_equal(sub_ids, LETTERS[1:3])
  expect_equal(item_ids, LETTERS[4:6])
  expect_equal(sub_is, 1:3)
  expect_equal(item_is, 4:6)
})
