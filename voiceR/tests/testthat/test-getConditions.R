test_that("Get Conditions from a list of audios", {
  conditions <- getConditions(audioList = testAudioList,
                fileNamePattern = "ID_Condition_Dimension", sep = "_")
  expect_true(all(conditions %in% c("Happy", "Sad")))

})
