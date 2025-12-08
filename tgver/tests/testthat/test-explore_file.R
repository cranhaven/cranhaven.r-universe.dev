test_that("explore_file works", {
  expect_error(explore_file())
  t = tempdir()
  expect_error(explore_file(t))

  # empty file
  fp = file.path(t, "empty.geojson")
  write("", fp)
  expect_error(explore_file(fp))
  file.remove(fp)
  js <- c(
    '[
      {"type":"Point","coordinates":[0,0]},
      {"type":"LineString","coordinates":[[-1,-1],[1,1]]},
        {
      "type": "FeatureCollection",
      "features": [
      {
        "type": "Feature",
        "properties": {"id":1},
        "geometry": {"type": "Point", "coordinates": [100.0, 0.0]}
      }
    ]
  }
    ]'
  )
  fp = file.path(t, "test.geojson")
  write(js, fp)
  ps = tgver::explore_file(fp, background = TRUE)
  expect_true(inherits(ps, "r_process"))
  ps$kill()
  file.remove(fp)
})
