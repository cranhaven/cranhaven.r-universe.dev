# test multi_dscore() function ####

test_that("multi_dscore recognizes the correct class of the object",{
  iat_cleandata <- clean_iat(raw_data, sbj_id = "Participant",
                             block_id = "blockcode",
                             mapA_practice = "practice.iat.Milkbad",
                             mapA_test = "test.iat.Milkbad",
                             mapB_practice = "practice.iat.Milkgood",
                             mapB_test = "test.iat.Milkgood",
                             latency_id = "latency",
                             accuracy_id = "correct",
                             trial_id = "trialcode",
                             trial_eliminate = c("reminder", "reminder1"),
                             demo_id = "blockcode",
                             trial_demo = "demo")
  dscore <- compute_iat(iat_cleandata[[1]], Dscore = "d1")
  expect_error(multi_dscore(dscore))
})

test_that("multi_dscore results in a List of 2 (data.frame and list)",{
  iat_cleandata <- clean_iat(raw_data, sbj_id = "Participant",
                             block_id = "blockcode",
                             mapA_practice = "practice.iat.Milkbad",
                             mapA_test = "test.iat.Milkbad",
                             mapB_practice = "practice.iat.Milkgood",
                             mapB_test = "test.iat.Milkgood",
                             latency_id = "latency",
                             accuracy_id = "correct",
                             trial_id = "trialcode",
                             trial_eliminate = c("reminder", "reminder1"),
                             demo_id = "blockcode",
                             trial_demo = "demo")
  iat_data <- iat_cleandata[[1]]
  expect_output(str(multi_dscore(iat_data)), "List of 2")
  expect_true(class(multi_dscore(iat_data)[[1]]) == "data.frame")
  expect_true(class(multi_dscore(iat_data)[[2]])[2] == "ggplot")
})

test_that("multi_dscore results in dataframe with 3 col when built-in (default) is chosen",{
  iat_cleandata <- clean_iat(raw_data, sbj_id = "Participant",
                             block_id = "blockcode",
                             mapA_practice = "practice.iat.Milkbad",
                             mapA_test = "test.iat.Milkbad",
                             mapB_practice = "practice.iat.Milkgood",
                             mapB_test = "test.iat.Milkgood",
                             latency_id = "latency",
                             accuracy_id = "correct",
                             trial_id = "trialcode",
                             trial_eliminate = c("reminder", "reminder1"),
                             demo_id = "blockcode",
                             trial_demo = "demo")
  iat_data <- iat_cleandata[[1]]
  expect_true(ncol(multi_dscore(iat_data)[[1]]) == 3)
  expect_true(ncol(multi_dscore(iat_data)[[1]]) == ncol(multi_dscore(iat_data, ds = "built-in")[[1]]))
})

test_that("multi_dscore results in dataframe with 5 col when error-inflation is chosen",{
  iat_cleandata <- clean_iat(raw_data, sbj_id = "Participant",
                             block_id = "blockcode",
                             mapA_practice = "practice.iat.Milkbad",
                             mapA_test = "test.iat.Milkbad",
                             mapB_practice = "practice.iat.Milkgood",
                             mapB_test = "test.iat.Milkgood",
                             latency_id = "latency",
                             accuracy_id = "correct",
                             trial_id = "trialcode",
                             trial_eliminate = c("reminder", "reminder1"),
                             demo_id = "blockcode",
                             trial_demo = "demo")
  iat_data <- iat_cleandata[[1]]
  expect_true(ncol(multi_dscore(iat_data, ds = "error-inflation")[[1]]) == 5)
})


# Test multi_dsciat function #####
test_that("multi_dsciat recognizes the class of both the SC-IATs", {
  data("raw_data") # load data
  sciat_data <- clean_sciat(raw_data, sbj_id = "Participant",
                            block_id = "blockcode",
                            latency_id = "latency",
                            accuracy_id = "correct",
                            block_sciat_1 = c("test.sc_dark.Darkbad",
                                              "test.sc_dark.Darkgood"),
                            block_sciat_2 = c("test.sc_milk.Milkbad",
                                              "test.sc_milk.Milkgood"),
                            trial_id  = "trialcode",
                            trial_eliminate = c("reminder",
                                                "reminder1"))
  sciat1 <- sciat_data[[1]]
  sciat2 <- sciat_data[[2]]
  d_sciat1 <- compute_sciat(sciat1,
                     mappingA = "test.sc_dark.Darkbad",
                     mappingB = "test.sc_dark.Darkgood",
                     non_response = "alert")
  d_sciat2 <- compute_sciat(sciat2,
                     mappingA = "test.sc_milk.Milkbad",
                     mappingB = "test.sc_milk.Milkgood",
                     non_response = "alert")
  iat_cleandata <- clean_iat(raw_data, sbj_id = "Participant",
                             block_id = "blockcode",
                             mapA_practice = "practice.iat.Milkbad",
                             mapA_test = "test.iat.Milkbad",
                             mapB_practice = "practice.iat.Milkgood",
                             mapB_test = "test.iat.Milkgood",
                             latency_id = "latency",
                             accuracy_id = "correct",
                             trial_id = "trialcode",
                             trial_eliminate = c("reminder", "reminder1"))
  iat_score <- compute_iat(iat_cleandata[[1]], Dscore = "d3")
  expect_error(multi_dsciat(sciat_data[[1]], sciat_data[[2]]))
  expect_error(multi_dsciat(d_sciat1, iat_score))
  expect_error(multi_dsciat(iat_score, d_sciat1))
  expect_error(multi_dsciat(raw_data, d_sciat1))
  expect_output(str(multi_dsciat(d_sciat1, d_sciat2)))
})


