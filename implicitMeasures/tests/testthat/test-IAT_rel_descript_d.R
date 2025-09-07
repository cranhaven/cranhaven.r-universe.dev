# Test IAT_rel function #####

test_that("IAT_rel throws an error when the wrong class is passed",{
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
  expect_error(IAT_rel(iat_cleandata[3]))
})

test_that("IAT_rel results in a list of 2 elements with class IAT_rel", {
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
                         Dscore =  "d2")
  expect_output(str(IAT_rel(iat_dscore)), "List of 2")
  expect_true(class(IAT_rel(iat_dscore)) == "IAT_rel")
})


# TEST descript_d Function ####

test_that("descript_d recognizies the class of the object for the SC-IAT",
          {
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

            sciat1 <- sciat_data[[1]] # compute D for the first SC-IAT
            d_sciat1 <- compute_sciat(sciat1,
                               mappingA = "test.sc_dark.Darkbad",
                               mappingB = "test.sc_dark.Darkgood",
                               non_response = "alert")
            expect_error(descript_d(sciat1))})

test_that("descript_d recognizies the class of the object for the IAT",
          {
            data("raw_data") # load data
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
            expect_error(descript_d(iat_data))})
