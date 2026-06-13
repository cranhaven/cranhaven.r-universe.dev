test_that("Get components from a list of audio files with name pattern ID_Condition", {
  components <- getComponents(names(testAudioList), fileNamePattern = "ID_Condition", sep = "_")
  expect_true(components$Positions$Dimension == 99)
  expect_true(components$Positions$ID == 1)
  expect_true(components$Positions$Condition == 2)

})

test_that("Get components from a list of audio files with name pattern ID_Condition_Dimension", {
  components <- getComponents(names(testAudioList), fileNamePattern = "ID_Condition_Dimension", sep = "_")
  expect_true(components$Positions$Dimension == 3)
  expect_true(components$Positions$ID == 1)
  expect_true(components$Positions$Condition == 2)

})
