test_that("Feature Extraction from a list of audios adding a filter", {
  audioData <- autoExtract(audioList = testAudioList,
                           fileNamePattern = "ID_Condition_Dimension",
                           filter = c("5b438f516066ad470d3be72c52005251"))
  expect_s3_class(audioData, "data.frame")
  expect_equal(nrow(audioData),
               sum(apply(sapply("5b438f516066ad470d3be72c52005251", grepl,
                            names(testAudioList)), 1, all)))
})
