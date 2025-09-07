###### clean_iat test ###########
test_that("clean_iat produces one dataframe with iat_clean class", {
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
                             trial_eliminate = c("reminder", "reminder1"),
                             demo_id = "blockcode",
                             trial_demo = "demo")
  expect_true(class(iat_cleandata[[1]])[2] == "iat_clean")
})

test_that("clea_iat produces the right list when demo is specified", {
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
                             trial_eliminate = c("reminder", "reminder1"),
                             demo_id = "blockcode",
                             trial_demo = "demo")
  expect_output(str(iat_cleandata), "List of 3")
})

test_that("clean_iat produces the right list when demo is not specified", {
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
  expect_output(str(iat_cleandata), "List of 2")
})

test_that("clean_iat stops when trial_id is specified but no trials are passed", {
  data("raw_data")
  expect_error( clean_iat(raw_data, sbj_id = "Participant",
                          block_id = "blockcode",
                          mapA_practice = "practice.iat.Milkbad",
                          mapA_test = "test.iat.Milkbad",
                          mapB_practice = "practice.iat.Milkgood",
                          mapB_test = "test.iat.Milkgood",
                          latency_id = "latency",
                          accuracy_id = "correct",
                          trial_id = "trialcode"))
})

test_that("clean_iat stops when trial_elimiante is specified but not trial_id", {
  data("raw_data")
  expect_error( clean_iat(raw_data, sbj_id = "Participant",
                          block_id = "blockcode",
                          mapA_practice = "practice.iat.Milkbad",
                          mapA_test = "test.iat.Milkbad",
                          mapB_practice = "practice.iat.Milkgood",
                          mapB_test = "test.iat.Milkgood",
                          latency_id = "latency",
                          accuracy_id = "correct",
                          trial_eliminate = c("reminder", "reminder1")))
})


test_that("clean_iat stops when demo_id is specified but no trials are passed", {
  data("raw_data")
  expect_error( clean_iat(raw_data, sbj_id = "Participant",
                          block_id = "blockcode",
                          mapA_practice = "practice.iat.Milkbad",
                          mapA_test = "test.iat.Milkbad",
                          mapB_practice = "practice.iat.Milkgood",
                          mapB_test = "test.iat.Milkgood",
                          latency_id = "latency",
                          accuracy_id = "correct",
                          trial_id = "trialcode",
                          trial_eliminate = c("reminder", "reminder1"),
                          demo_id = "blockcode"))
})

test_that("clean_iat stops when trial_elimiante is specified but not trial_id", {
  data("raw_data")
  expect_error( clean_iat(raw_data, sbj_id = "Participant",
                          block_id = "blockcode",
                          mapA_practice = "practice.iat.Milkbad",
                          mapA_test = "test.iat.Milkbad",
                          mapB_practice = "practice.iat.Milkgood",
                          mapB_test = "test.iat.Milkgood",
                          latency_id = "latency",
                          accuracy_id = "correct",
                          trial_id = "trialcode",
                          trial_eliminate = c("reminder", "reminder1"),
                          trial_demo = "demo"))
})

test_that("clean_iat stops when wrong column names are passed", {
  data("raw_data")
  expect_error(clean_iat(raw_data, sbj_id = "Participant",
                         block_id = "block",
                         mapA_practice = "practice.iat.Milkbad",
                         mapA_test = "test.iat.Milkbad",
                         mapB_practice = "practice.iat.Milkgood",
                         mapB_test = "test.iat.Milkgood",
                         latency_id = "latency",
                         accuracy_id = "correct"))
})

test_that("clean_iat stops when wrong block labels are passed", {
  data("raw_data")
  expect_error(clean_iat(raw_data, sbj_id = "Participant",
                         block_id = "blockcode",
                         mapA_practice = "practice.Milkbad",
                         mapA_test = "test.iat.Milkbad",
                         mapB_practice = "practice.iat.Milkgood",
                         mapB_test = "test.iat.Milkgood",
                         latency_id = "latency",
                         accuracy_id = "correct"))
})

test_that("clean_iat stops when the same block label is passed more than once", {
  data("raw_data")
  expect_error(clean_iat(raw_data, sbj_id = "Participant",
                         block_id = "blockcode",
                         mapA_practice = "practice.iat.Milkbad",
                         mapA_test = "practice.iat.Milkbad",
                         mapB_practice = "practice.iat.Milkgood",
                         mapB_test = "test.iat.Milkgood",
                         latency_id = "latency",
                         accuracy_id = "correct"))
})

test_that("clean_iat produces dataframes for data and demographic with the same number of sbjs", {
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
                             trial_eliminate = c("reminder", "reminder1"),
                             demo_id = "blockcode",
                             trial_demo = "demo")
  expect_true(length(unique(iat_cleandata[[1]]$participant)) ==
               length(unique(iat_cleandata[[3]]$participant)))
})
