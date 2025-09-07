test_that("SC-IAT D-score replicate", {
  # import data set for SC-IAT dscore 1 computed on 07/09/2020
  data(dsciat1)
  # import data set for SC-IAT dscore 2 computed on 07/09/2020
  data(dsciat2)
  # import global data set
  data(raw_data)
  # clean data for SC-IAT 1 and SC-IAT 2
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
  # compute D for the first SC-IAT
  sciat1 <- sciat_data[[1]]
  Rsciat1 <- compute_sciat(sciat1,
                    mappingA = "test.sc_dark.Darkbad",
                    mappingB = "test.sc_dark.Darkgood",
                    non_response = "alert")
  Rsciat1$participant <- as.character(Rsciat1$participant)
  # compute D for the first SC-IAT
  sciat2 <- sciat_data[[2]]
  Rsciat2 <- compute_sciat(sciat2,
                    mappingA = "test.sc_milk.Milkbad",
                    mappingB = "test.sc_milk.Milkgood",
                    non_response = "alert")
  Rsciat2$participant <- as.character(Rsciat2$participant)
  expect_true(all(dsciat1[, "d_sciat"] == Rsciat1[, "d_sciat"]) == TRUE)
  expect_true(all(dsciat1[, "d_sciat"] == Rsciat1[, "d_sciat"]) == TRUE)
})
