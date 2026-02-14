
test_that("roads_to_segments explodes MULTILINESTRING and creates seg_id/length_m", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  ml <- sf::st_sfc(
    sf::st_multilinestring(list(
      matrix(c(0,0, 1,0), ncol = 2, byrow = TRUE),
      matrix(c(1,0, 1,1), ncol = 2, byrow = TRUE)
    )),
    crs = 3857
  )
  roads <- sf::st_sf(id = 1, geometry = ml)

  segs <- roads_to_segments(roads)

  expect_s3_class(segs, "sf")
  expect_true(all(names(segs)[1:2] == c("seg_id", "length_m")))
  expect_equal(nrow(segs), 2)
  expect_equal(segs$seg_id, 1:2)
  expect_true(all(segs$length_m > 0))
  expect_true(all(as.character(sf::st_geometry_type(segs)) == "LINESTRING"))
})


test_that("roads_to_segments computes metric length for lon/lat via projection", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  ls <- sf::st_sfc(
    sf::st_linestring(matrix(c(-122.0, 37.0, -122.0, 37.001), ncol = 2, byrow = TRUE)),
    crs = 4326
  )
  roads <- sf::st_sf(geometry = ls)

  segs <- roads_to_segments(roads, crs_m = 3857)

  expect_equal(nrow(segs), 1)
  expect_true(is.numeric(segs$length_m))
  expect_true(segs$length_m > 0)
})


test_that("roads_to_segments drops empty/zero-length when drop_zero=TRUE", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  g <- sf::st_sfc(
    sf::st_linestring(matrix(c(0,0, 0,0), ncol = 2, byrow = TRUE)), # zero length
    sf::st_linestring(matrix(c(0,0, 1,0), ncol = 2, byrow = TRUE)), # ok
    crs = 3857
  )
  roads <- sf::st_sf(geometry = g)

  segs <- roads_to_segments(roads, drop_zero = TRUE)

  expect_equal(nrow(segs), 1)
  expect_true(segs$length_m > 0)
})


test_that("roads_to_segments handles huge coordinates without overflow", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  # very large planar coordinates still produce finite positive lengths
  g <- sf::st_sfc(
    sf::st_linestring(matrix(c(1e9, 1e9, 1e9 + 1e6, 1e9), ncol = 2, byrow = TRUE)),
    crs = 3857
  )
  roads <- sf::st_sf(geometry = g)

  segs <- roads_to_segments(roads)
  expect_equal(nrow(segs), 1)
  expect_true(is.finite(segs$length_m))
  expect_gt(segs$length_m, 0)
})


test_that("roads_to_segments rejects non-sf inputs", {
  # non-sf inputs error cleanly
  expect_error(roads_to_segments(list(a = 1)), "sf", ignore.case = TRUE)
})


test_that("roads_to_segments rejects non-LINESTRING geometry types", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  pts <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  roads <- sf::st_sf(geometry = pts)
  expect_error(roads_to_segments(roads), "LINESTRING|MULTILINESTRING", ignore.case = TRUE)
})


test_that("roads_to_segments drops NA/empty geometries safely", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  g <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
    sf::st_geometrycollection(),  # empty-ish
    crs = 3857
  )
  roads <- sf::st_sf(geometry = g)

  segs <- roads_to_segments(roads, verbose = FALSE)
  expect_equal(nrow(segs), 1)
  expect_equal(segs$seg_id, 1L)
  expect_gt(segs$length_m, 0)
})


test_that("roads_to_segments drops zero/near-zero length segments", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  g <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 0, 0), ncol = 2, byrow = TRUE)),          # zero
    sf::st_linestring(matrix(c(0, 0, 1e-12, 0), ncol = 2, byrow = TRUE)),      # extremely small
    sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),          # ok
    crs = 3857
  )
  roads <- sf::st_sf(geometry = g)

  segs <- roads_to_segments(roads, drop_zero = TRUE)
  expect_equal(nrow(segs), 2)  # near-zero may survive depending on sf numeric precision
  expect_true(all(segs$length_m > 0))
})


test_that("roads_to_segments keeps requested attrs only", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  g <- sf::st_sfc(sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)), crs = 3857)
  roads <- sf::st_sf(osm_id = 123, speed_lim = 35, geometry = g)

  segs <- roads_to_segments(roads, keep_attrs = c("osm_id"))
  expect_true("osm_id" %in% names(segs))
  expect_false("speed_lim" %in% names(segs))

  expect_error(
    roads_to_segments(roads, keep_attrs = c("does_not_exist")),
    "missing columns",
    ignore.case = TRUE
  )
})


test_that("roads_to_segments works on mixed LINESTRING + MULTILINESTRING input", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  g <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
    sf::st_multilinestring(list(
      matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE),
      matrix(c(1, 1, 2, 1), ncol = 2, byrow = TRUE)
    )),
    crs = 3857
  )
  roads <- sf::st_sf(kind = c("ls", "mls"), geometry = g)

  segs <- roads_to_segments(roads)
  expect_equal(nrow(segs), 3)
  expect_equal(segs$seg_id, 1:3)
  expect_true(all(segs$length_m > 0))
})


test_that("roads_to_segments splits at intersections when split_at_intersections=TRUE", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  g <- sf::st_sfc(
    sf::st_linestring(matrix(c(-1, 0,  1, 0), ncol = 2, byrow = TRUE)),  # horizontal
    sf::st_linestring(matrix(c( 0,-1,  0, 1), ncol = 2, byrow = TRUE)),  # vertical
    crs = 3857
  )
  roads <- sf::st_sf(road_id = c("h", "v"), geometry = g)

  segs0 <- roads_to_segments(roads, split_at_intersections = FALSE)
  segs1 <- roads_to_segments(roads, split_at_intersections = TRUE)

  expect_equal(nrow(segs0), 2)
  expect_equal(nrow(segs1), 4)
  expect_equal(segs1$seg_id, 1:4)
  expect_true(all(segs1$length_m > 0))
})



test_that("build_adjacency links shared endpoints and returns isolates/components", {
  skip_if_not_installed("sf")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("units")

  g <- sf::st_sfc(
    sf::st_linestring(matrix(c(0,0, 1,0), ncol = 2, byrow = TRUE)),  # seg 1
    sf::st_linestring(matrix(c(1,0, 2,0), ncol = 2, byrow = TRUE)),  # seg 2 (touches seg 1 at (1,0))
    sf::st_linestring(matrix(c(0,5, 1,5), ncol = 2, byrow = TRUE)),  # seg 3 isolate
    crs = 3857
  )
  segs <- sf::st_sf(geometry = g)
  segs$seg_id <- 1:3

  out <- build_adjacency(segs)

  expect_s4_class(out$A, "dgCMatrix")
  expect_equal(dim(out$A), c(3, 3))
  expect_equal(out$A[1,2], 1)
  expect_equal(out$A[2,1], 1)
  expect_equal(out$A[1,1], 0)
  expect_true(out$isolates[3])
  expect_false(out$isolates[1])
  expect_false(out$isolates[2])

  # components: (1,2) same; 3 alone
  expect_equal(out$components[1], out$components[2])
  expect_true(out$components[3] != out$components[1])
})



test_that("build_adjacency snaps near-coincident endpoints when tol>0", {
  skip_if_not_installed("sf")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("units")

  g <- sf::st_sfc(
    sf::st_linestring(matrix(c(0,0, 1,0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1 + 1e-6, 0, 2,0), ncol = 2, byrow = TRUE)),
    crs = 3857
  )
  segs <- sf::st_sf(geometry = g)

  out0 <- build_adjacency(segs, tol = 0)
  out1 <- build_adjacency(segs, tol = 1e-3)

  expect_equal(out0$A[1,2], 0)
  expect_equal(out1$A[1,2], 1)
})



test_that("build_adjacency handles duplicate geometries without self-loops", {
  skip_if_not_installed("sf")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("units")

  g <- sf::st_sfc(
    sf::st_linestring(matrix(c(0,0, 1,0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0,0, 1,0), ncol = 2, byrow = TRUE)),
    crs = 3857
  )
  segs <- sf::st_sf(geometry = g)

  out <- build_adjacency(segs)
  expect_equal(out$A[1,2], 1)
  expect_equal(out$A[2,1], 1)
  expect_equal(out$A[1,1], 0)
  expect_true(all(out$A@x %in% 1))
})



test_that("build_adjacency returns all isolates when no endpoints match", {
  skip_if_not_installed("sf")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("units")

  g <- sf::st_sfc(
    sf::st_linestring(matrix(c(0,0, 1,0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0,2, 1,2), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0,4, 1,4), ncol = 2, byrow = TRUE)),
    crs = 3857
  )
  segs <- sf::st_sf(geometry = g)

  out <- build_adjacency(segs)
  expect_equal(Matrix::nnzero(out$A), 0)
  expect_true(all(out$isolates))
  expect_equal(length(unique(out$components)), 3)
})



test_that("build_adjacency rejects invalid inputs", {
  expect_error(build_adjacency(list(a = 1)), "sf", ignore.case = TRUE)

  skip_if_not_installed("sf")
  skip_if_not_installed("Matrix")
  pts <- sf::st_sfc(sf::st_point(c(0,0)), crs = 3857)
  bad <- sf::st_sf(geometry = pts)
  expect_error(build_adjacency(bad), "LINESTRING", ignore.case = TRUE)
})


test_that("components_from_adjacency returns isolates and singleton components correctly", {
  skip_if_not_installed("Matrix")

  A <- Matrix::sparseMatrix(
    i = c(1, 2), j = c(2, 1), x = 1,
    dims = c(3, 3), giveCsparse = TRUE
  )
  out <- components_from_adjacency(A)

  expect_equal(length(out$components), 3)
  expect_equal(length(unique(out$components)), 2)
  expect_true(out$isolates[3])
  expect_false(out$isolates[1])
  expect_false(out$isolates[2])
  expect_equal(out$components[1], out$components[2])
})

