test_that("compute_iat recognize the rigth class", {
  data("raw_data")
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
                                                "reminder1"),
                            demo_id = "blockcode",
                            trial_demo = "demo")
  expect_error(compute_iat(raw_data), Dscore = "d1")
  expect_error(compute_iat(sciat_data[[1]]), Dscore = "d2")
})

test_that("compute_iat stops if Dscore is not specified", {
  data("raw_data")
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
  expect_error(compute_iat(iat_cleandata[[1]]))
})

test_that("compute_iat produce a dataframe of class dscore (all scores)", {
  skip_on_cran()
  data("raw_data")
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
  ds <- paste0(rep("d",6), 1:6 )
  check_compD <- list()
  scores <- list()

  for(i in 1:length(ds)){
    scores[[i]] <- compute_iat(iat_cleandata[[1]], Dscore = ds[i])
    expect_true(class(scores[[i]])[2] == "dscore")
  }
})


test_that("compute_iat produce a dataframe with the same number of sbjs for all", {
  skip_on_cran()
  data("raw_data")
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
  ds <- paste0(rep("d",6), 1:6 )
  check_compD <- list()
  scores <- list()

  for(i in 1:length(ds)){
    scores[[i]] <- compute_iat(iat_cleandata[[1]], Dscore = ds[i])
    expect_true(nrow(scores[[i]]) == length(unique(iat_cleandata[[1]]$participant)) )
  }
})
