# clean_sciat test  #######
test_that("clean_sciat produces a List of one object of class sciat_clean" , {
  data("raw_data")
  sciat_data <- clean_sciat(raw_data, sbj_id = "Participant",
                            block_id = "blockcode",
                            latency_id = "latency",
                            accuracy_id = "correct",
                            block_sciat_1 = c("test.sc_dark.Darkbad",
                                              "test.sc_dark.Darkgood"),
                            trial_id  = "trialcode",
                            trial_eliminate = c("reminder",
                                                "reminder1"))
  expect_output(str(sciat_data), "List of 1")
  expect_equal(class(sciat_data[[1]])[2], "sciat_clean")
})

test_that("clean_sciat produces a list with two objects of class sciat_clean" , {
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
                                                "reminder1"))
  expect_output(str(sciat_data), "List of 2")
  expect_equal(class(sciat_data[[1]])[2], "sciat_clean")
  expect_equal(class(sciat_data[[2]])[2], "sciat_clean")
})

test_that("clean_sciat produces a list with two objects, one with class sciat_clean" , {
  data("raw_data")
  sciat_data <- clean_sciat(raw_data, sbj_id = "Participant",
                            block_id = "blockcode",
                            latency_id = "latency",
                            accuracy_id = "correct",
                            block_sciat_1 = c("test.sc_dark.Darkbad",
                                              "test.sc_dark.Darkgood"),

                            trial_id  = "trialcode",
                            trial_eliminate = c("reminder",
                                                "reminder1"),
                            demo_id = "blockcode",
                            trial_demo = "demo")
  expect_output(str(sciat_data), "List of 2")
  expect_equal(class(sciat_data[[1]])[2], "sciat_clean")
  expect_equal(class(sciat_data[[2]]), "data.frame")
})

test_that("clean_sciat produces a list with three objects, two with class sciat_clean" , {
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
  expect_output(str(sciat_data), "List of 3")
  expect_equal(class(sciat_data[[1]])[2], "sciat_clean")
  expect_equal(class(sciat_data[[2]])[2], "sciat_clean")
  expect_equal(class(sciat_data[[3]]), "data.frame")
})

test_that("clean_sciat stops when wrong column names are passed", {
  data("raw_data")
  expect_error(clean_sciat(raw_data, sbj_id = "Participant",
                           block_id = "bckcode",
                           latency_id = "latency",
                           accuracy_id = "correct",
                           block_sciat_1 = c("test.sc_dark.Darkbad",
                                             "test.sc_dark.Darkgood"),
  ))
})

test_that("clean_sciat stops when trial_id is specified but no trials are passed", {
  data("raw_data")
  expect_error( clean_sciat(raw_data, sbj_id = "Participant",
                            block_id = "blockcode",
                            latency_id = "latency",
                            accuracy_id = "correct",
                            block_sciat_1 = c("test.sc_dark.Darkbad",
                                              "test.sc_dark.Darkgood"),
                            block_sciat_2 = c("test.sc_milk.Milkbad",
                                              "test.sc_milk.Milkgood"),
                            trial_id  = "trialcode",
                            demo_id = "blockcode",
                            trial_demo = "demo"))
})

test_that("clean_sciat stops when trial_elimanate are passed but no trial_id is specified", {
  data("raw_data")
  expect_error( clean_sciat(raw_data, sbj_id = "Participant",
                            block_id = "blockcode",
                            latency_id = "latency",
                            accuracy_id = "correct",
                            block_sciat_1 = c("test.sc_dark.Darkbad",
                                              "test.sc_dark.Darkgood"),
                            block_sciat_2 = c("test.sc_milk.Milkbad",
                                              "test.sc_milk.Milkgood"),

                            trial_eliminate = c("reminder",
                                                "reminder1"),
                            demo_id = "blockcode",
                            trial_demo = "demo"))
})

test_that("clean_sciat stops when demo_id is specified but no trials are passed", {
  data("raw_data")
  expect_error( clean_sciat(raw_data, sbj_id = "Participant",
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
                            demo_id = "blockcode"))
})

test_that("clean_sciat stops when trial_demo are passed but no demo_id is specified", {
  data("raw_data")
  expect_error( clean_sciat(raw_data, sbj_id = "Participant",
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
                            trial_demo = "demo"))
})

test_that("clean_sciat stops when block labels for sciat1 are not in the data", {
  data("raw_data")
  expect_error( clean_sciat(raw_data, sbj_id = "Participant",
                            block_id = "blockcode",
                            latency_id = "latency",
                            accuracy_id = "correct",
                            block_sciat_1 = c("tsc_dark.Darkbad",
                                              "test.sc_dark.Darkgood"),
                            trial_id  = "trialcode",
                            trial_eliminate = c("reminder",
                                                "reminder1"),
                            trial_demo = "demo"))
})

test_that("clean_sciat stops when block labels for sciat2 are not in the data", {
  data("raw_data")
  expect_error( clean_sciat(raw_data, sbj_id = "Participant",
                            block_id = "blockcode",
                            latency_id = "latency",
                            accuracy_id = "correct",
                            block_sciat_1 = c("test.sc_dark.Darkbad",
                                              "test.sc_dark.Darkgood"),
                            block_sciat_2 = c("est.sc_milk.Milkbad",
                                              "test.sc_milk.Milkgood"),
                            trial_id  = "trialcode",
                            trial_eliminate = c("reminder",
                                                "reminder1"),
                            trial_demo = "demo"))
})

test_that("clean_sciat stops when block labels for both sciats are not in the data", {
  data("raw_data")
  expect_error( clean_sciat(raw_data, sbj_id = "Participant",
                            block_id = "blockcode",
                            latency_id = "latency",
                            accuracy_id = "correct",
                            block_sciat_1 = c("test.sc_dark.Darkbad",
                                              "test.sc_k.Darkgood"),
                            block_sciat_2 = c("est.sc_milk.Milkbad",
                                              "test.sc_milk.Milkgood"),
                            trial_id  = "trialcode",
                            trial_eliminate = c("reminder",
                                                "reminder1"),
                            trial_demo = "demo"))
})

test_that("clean_sciat produces dataframes for sciat1 and demographic with the same number of sbjs", {
  data("raw_data")
  sciat_data <- clean_sciat(raw_data, sbj_id = "Participant",
                            block_id = "blockcode",
                            latency_id = "latency",
                            accuracy_id = "correct",
                            block_sciat_1 = c("test.sc_dark.Darkbad",
                                              "test.sc_dark.Darkgood"),
                            trial_id  = "trialcode",
                            trial_eliminate = c("reminder",
                                                "reminder1"),
                            demo_id = "blockcode",
                            trial_demo = "demo"
  )
  expect_equal(length(unique(sciat_data[[1]]$participant)),
               length(unique(sciat_data[[2]]$participant)))
})
