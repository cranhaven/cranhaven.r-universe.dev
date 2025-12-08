test_that("static tgve works", {
  p = tempdir()
  ip = file.path(p, "tgve", "index.html")
  r = tgve(browse = FALSE)
  expect_true(identical(ip, r))
  # try again
  unlink(file.path(p, "tgve"), recursive = TRUE)
  skip_on_cran()
  expect_message(tgve())
  expect_error(tgve(remote = TRUE,
                    url = "notvalidurl"))
  expect_message(tgve(remote = TRUE,
                      # even just tgve(remote = TRUE)
                      url = "https://tgve.github.io/app/"))
})
