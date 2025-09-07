# test d_point function ####
test_that("d_point recognises the right class",{
  data("raw_data") # import data
  expect_error(d_point(raw_data))
})

test_that("d_point produces a ggplot fot the IAT",{
  data("raw_data") # import data
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
  # calculate D-score
  iat_dscore <- compute_iat(iat_data,
                         Dscore =  "d6")
  expect_true(class(d_point(iat_dscore))[[1]] == "gg")
  expect_true(class(d_point(iat_dscore))[[2]] == "ggplot")
})

test_that("d_point produces a ggplot fot the SC-IAT",{
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

  sciat1 <- sciat_data[[1]] # compute D for the first SC-IAT
  d_sciat1 <- compute_sciat(sciat1,
                     mappingA = "test.sc_dark.Darkbad",
                     mappingB = "test.sc_dark.Darkgood",
                     non_response = "alert")
  expect_true(class(d_point(d_sciat1))[[1]] == "gg")
  expect_true(class(d_point(d_sciat1))[[2]] == "ggplot")
})



# test d_density function ####
test_that("d_density recognises the right class",{
  data("raw_data") # import data
  expect_error(d_density(raw_data))
})

test_that("d_density produces a ggplot fot the IAT",{
  data("raw_data") # import data
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
  # calculate D-score
  iat_dscore <- compute_iat(iat_data,
                         Dscore =  "d6")
  expect_true(class(d_density(iat_dscore))[[1]] == "gg")
  expect_true(class(d_density(iat_dscore))[[2]] == "ggplot")
})

test_that("d_density produces a ggplot fot the SC-IAT",{
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

  sciat1 <- sciat_data[[1]] # compute D for the first SC-IAT
  d_sciat1 <- compute_sciat(sciat1,
                     mappingA = "test.sc_dark.Darkbad",
                     mappingB = "test.sc_dark.Darkgood",
                     non_response = "alert")
  expect_true(class(d_density(d_sciat1))[[1]] == "gg")
  expect_true(class(d_density(d_sciat1))[[2]] == "ggplot")
})
