test_that("normalize_minmax() works", {
  path <- system.file("external/DSCN4500.JPG", package = "rcaiman")
  expect_equal(min(normalize_minmax(read_caim(path), 0, 255)[]),
               0)
  expect_equal(max(normalize_minmax(read_caim(path), 0, 255)[]),
               1)
})
