targets::tar_test("tar_make_archive() works", {
  expect_no_error(
    tar_make_archive(
      package = "tarchives",
      pipeline = "example-model"
    )
  )
  expect_error(
    tar_make_archive(
      package = "tarchives-wrong",
      pipeline = "example-model"
    )
  )
  expect_error(
    tar_make_archive(
      package = "tarchives",
      pipeline = "example-model-wrong"
    )
  )
})
