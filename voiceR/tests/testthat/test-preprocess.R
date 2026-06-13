test_that("Basic preprocessing of a list of audio files", {
  preprocessedAudioList <- preprocess(testAudioList)
  expect_true(length(preprocessedAudioList) == length(testAudioList))
})
