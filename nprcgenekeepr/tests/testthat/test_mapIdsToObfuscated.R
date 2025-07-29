#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
test_that("mapIdsToObfuscated maps IDS as expected", {
  set_seed(1L)
  ped <- qcStudbook(nprcgenekeepr::pedSix)
  obfuscated <- obfuscatePed(ped, map = TRUE)
  someIds <- c("s1", "s2", "d1", "d1")
  someUndefinedIds <- c("nope", "s1", "still_nope", "d1")
  expect_identical(
    mapIdsToObfuscated(someIds, obfuscated$map),
    c("JNAN5L", "0ZR5QI", "2D0P3X", "2D0P3X")
  )
  expect_error(
    mapIdsToObfuscated(someUndefinedIds, obfuscated$map),
    "Some IDs are not in map."
  )
})
