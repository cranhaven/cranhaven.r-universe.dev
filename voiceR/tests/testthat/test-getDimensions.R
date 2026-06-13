test_that("Get Components from a list of audios", {
  dimensions <- getDimensions(audioList = testAudioList,
                              fileNamePattern = "ID_Condition_Dimension", sep = "_")
  expect_true(all(dimensions %in% c("Bar")))

})
