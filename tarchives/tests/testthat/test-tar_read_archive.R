targets::tar_test("tar_read_archive() works", {
  tar_make_archive(
    package = "tarchives",
    pipeline = "example-model"
  )

  expect_s3_class(
    tar_read_archive(
      model,
      package = "tarchives",
      pipeline = "example-model"
    ),
    "lm"
  )
})
