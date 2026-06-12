
test_that("Is colnames in `.meta` actual?", {
  expect_equal(
    tolower(colnames(ps1_metadata("mean", "dr2"))),
    tolower(colnames(.meta[["mean_dr2"]])))
  expect_equal(
    tolower(colnames(ps1_metadata("mean", "dr1"))),
    tolower(colnames(.meta[["mean_dr1"]])))
  expect_equal(
    tolower(colnames(ps1_metadata("stack", "dr2"))),
    tolower(colnames(.meta[["stack_dr2"]])))
  expect_equal(
    tolower(colnames(ps1_metadata("stack", "dr1"))),
    tolower(colnames(.meta[["stack_dr1"]])))
  expect_equal(
    tolower(colnames(ps1_metadata("detection", "dr2"))),
    tolower(colnames(.meta[["detection_dr2"]])))
  expect_equal(
    tolower(colnames(ps1_metadata("forced_mean", "dr2"))),
    tolower(colnames(.meta[["forced_mean_dr2"]])))
  }
)



