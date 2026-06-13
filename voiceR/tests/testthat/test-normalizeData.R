test_that("Normalize list of audios", {
  normalizedData <- normalizeData(testAudioData)
  expect_true(all(dim(normalizedData$audioData) == dim(testAudioData)))
})
