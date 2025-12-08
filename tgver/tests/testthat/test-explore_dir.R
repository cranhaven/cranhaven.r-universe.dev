test_that("explore_dir works", {
  p = file.path(tempdir(), "data")
  dir.create(p)

  expect_error(explore_dir())
  expect_error(explore_dir(p))

  source("../skip-download.R")
  skip_download()
  skip_on_cran()

  # download saferactive files
  gURL = paste0("https://raw.githubusercontent.com/saferactive/",
                "tgve/main/pf-only-name.geojson")
  dURL = "https://raw.githubusercontent.com/saferactive/tgve/main/ksi-pf.csv"
  download.file(gURL, destfile = file.path(p, "pf.geojson"))
  download.file(dURL, destfile = file.path(p, "data.csv"))

  ps = explore_dir(p, background = TRUE)
  expect_true(inherits(ps, "r_process"))
  ps$kill()
  unlink(p, recursive = TRUE)
})
