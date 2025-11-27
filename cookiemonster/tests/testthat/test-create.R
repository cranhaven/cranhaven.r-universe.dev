test_that("default cookie location as described in docs", {
  expect_equal(
    default_jar(),
    rappdirs::user_cache_dir("r_cookies")
  )
})

test_that("getting cookies works", {
  jar <- options(cookie_dir = tempdir())

  expect_false(file.exists(file.path(tempdir(), paste0("cookies.rds"))))
  add_cookies(cookiestring = "test=true", domain = "https://create.com", confirm = TRUE)
  expect_true(file.exists(file.path(tempdir(), paste0("cookies.rds"))))
})
