test_that("Get IDs from a list of audios", {
  ids <- getIds(audioList = testAudioList,
                fileNamePattern = "ID_Condition_Dimension", sep = "_")

  expect_true(length(ids) == unique(length(ids)))

})
