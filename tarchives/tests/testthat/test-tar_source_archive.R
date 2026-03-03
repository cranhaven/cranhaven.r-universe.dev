targets::tar_test("tar_source_archive() works", {
  expect_no_error(
    tar_source_archive(
      "tarchives"
    )
  )
  expect_error(
    tar_source_archive(
      "tarchives-wrong"
    )
  )
})
