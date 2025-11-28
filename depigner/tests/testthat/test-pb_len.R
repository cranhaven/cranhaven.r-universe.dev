test_that("return error if not integer", {
  expect_error(pb_len(1.1), "must be an integer",
    class = "usethis_error"
  )
})


test_that("tick return a progressbar", {
  pb <- pb_len(1L)
  expect_s3_class(tick(pb), "progress_bar")
})
