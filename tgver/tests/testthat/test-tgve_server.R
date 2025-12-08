test_that("tgve returns a plumber instance", {
  server = tgve_server(run = FALSE)
  expect_true(inherits(server, "Plumber"))
  expect_equal(length(server$mounts), 1)
  ps = tgve_server(background = TRUE)
  expect_true(inherits(ps, "r_process"))
  ps$kill()
})
