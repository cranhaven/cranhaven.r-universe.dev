test_that("Read audio Files", {
  audioList <- readAudio(system.file("Audios", package = "voiceR"),
             fileType = "wav", recursive = TRUE)
  expect_true(length(audioList) == length(list.files(system.file("Audios", package = "voiceR"))))
  expect_true(all(sapply(audioList, class) == "Wave"))
})
