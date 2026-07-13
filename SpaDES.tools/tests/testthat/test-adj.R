test_that("adj.R results not identical to adjacent", {
  testInit("terra")
  rastDF <- needTerraAndRaster()

  for (ii in seq_len(NROW(rastDF))) {
    pkg <- rastDF$pkg[ii]
    a <- switch(pkg,
                raster = raster::raster(raster::extent(0, 1e1, 0, 1e1), res = 1),
                terra = terra::rast(terra::ext(0, 1e1, 0, 1e1), res = 1))
    a[] <- NA

    sam <- sample(seq_along(as.vector(a[])), 4)
    set.seed(123)
    for (incl in c(TRUE, FALSE)) {
      for (ids in list(NULL, seq_len(length(sam)))) {
        for (targs in list(NULL, sam + 1)) {
          for (sortTF in c(TRUE, FALSE)) {
            for (ma in c(TRUE, FALSE)) {
              for (dirs in list(4, 8, "bishop")) {
                aOrig <- reproducible::Copy(a)
                for (prs in c(TRUE, FALSE)) {
                  for (tor in c(TRUE, FALSE)) {
                    adjDT <- adj(a, sam, directions = dirs, sort = sortTF,
                                 match.adjacent = ma, include = incl, target = targs,
                                 cutoff.for.data.table = 2, id = ids, pairs = prs, torus = tor)
                    adjMat <- adj(a, sam, directions = dirs, sort = sortTF,
                                  match.adjacent = ma, include = incl,
                                  target = targs, id = ids, pairs = prs, torus = tor)
                    expect_equal(adjMat, adjDT, ignore_attr = TRUE)
                    if (!tor) {
                      useRasterPkg <- !is.null(targs) || isTRUE(sortTF) || !is.null(ids)
                      if (useRasterPkg) {
                        adj2 <- tryCatch({
                          if (!requireNamespace("raster")) stop()
                          terra::adjacent(a, sam, directions = dirs, sorted = sortTF,
                                          include = incl, id = !is.null(ids),
                                          pairs = prs, target = targs
                          )},
                          error = function(x) FALSE
                        )
                      } else {
                        adj2 <- tryCatch(
                          terra::adjacent(a, sam, directions = dirs, # sorted = sortTF,
                                          include = incl, #id = !is.null(ids),
                                          pairs = prs#, target = targs
                          ),
                          error = function(x) FALSE
                        )
                      }
                      if (!is.matrix(adj2))
                        adj2 <- matrix(adj2, nrow = 1)

                      if (!isFALSE(adj2)) {
                        if (!prs) {
                          if (ma) {
                            if (useRasterPkg) {
                              expect_equal(adjDT, adj2,
                                           info = paste0("ma=", ma,
                                                         ", dirs=", dirs,
                                                         ", sortTF=", sortTF,
                                                         ", incl=", incl,
                                                         ", is.null(ids)=", is.null(ids),
                                                         ", prs=", prs),
                                           ignore_attr = TRUE)
                            } else {
                              expect_equal(sort(adjDT), unique(sort(na.omit(as.numeric(adj2)))),
                                           info = paste0("ma=", ma,
                                                         ", dirs=", dirs,
                                                         ", sortTF=", sortTF,
                                                         ", incl=", incl,
                                                         ", is.null(ids)=", is.null(ids),
                                                         ", prs=", prs),
                                           ignore_attr = TRUE)
                            }

                          } else {
                            expect_equal(unique(sort(adjDT[, "to"])),
                                         unique(sort(na.omit(as.numeric(sort(adj2))))),
                                         ignore_attr = TRUE)
                          }
                        } else {
                          colOrd <- if (is.null(ids)) 1:2 else c(2, 3, 1)
                          if (ma) {
                            if (!sortTF) {
                              # if (!isTRUE(all.equal(adjDT, adj2[, colOrd]))) browser()
                              if (useRasterPkg) {
                                expect_equal(adjDT, adj2[, colOrd], ignore_attr = TRUE)
                              } else {
                                expect_equal(adj2[order(adj2[, "from"], adj2[, "to"]), ][, colOrd],
                                             adjDT[order(adjDT[, "from"], adjDT[, "to"]), ],
                                             ignore_attr = TRUE)
                              }
                            } else {
                              expect_equal(adjDT, adj2[order(adj2[, "from"], adj2[, "to"]), colOrd],
                                           ignore_attr = TRUE)
                            }
                          } else {
                            if (!sortTF) {
                              # if match.adjacent is FALSE, and sort is FALSE,
                              # then they mostly don't match

                              if (sum((adjDT - adj2[, colOrd]) ^ 2) == 0) {
                                expect_equal(adjDT, adj2[, colOrd], ignore_attr = TRUE)
                              } else {
                                # sum of squared difference should be positive
                                expect_gt(sum((adjDT - adj2[, colOrd]) ^ 2), 0)
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
})

test_that("errors in adj are not correct", {
  testInit("terra")
  rastDF <- needTerraAndRaster()

  for (ii in seq_len(NROW(rastDF))) {
    pkg <- rastDF$pkg[ii]

    a <- switch(pkg,
                raster = raster::raster(raster::extent(0, 1e1, 0, 1e1), res = 1),
                terra = terra::rast(terra::ext(0, 1e1, 0, 1e1), res = 1))
    a[] <- NA

    sam <- sample(seq_along(as.vector(a[])), 4)
    expect_error(adj(a, sam, directions = 5), "directions must be 4 or 8 or \'bishop\'")
  }
})

test_that("adj.R: torus does not work as expected", {
  testInit("terra")
  rastDF <- needTerraAndRaster()

  for (ii in seq_len(NROW(rastDF))) {
    pkg <- rastDF$pkg[ii]
    a <- switch(pkg,
                raster = raster::raster(raster::extent(0, 4, 0, 4), res = 1),
                terra = terra::rast(terra::ext(0, 4, 0, 4), res = 1))
    a[] <- NA
    # test data.table and matrix
    for (i in c(100, 1)) {
      # a corner
      s <- 4
      newCells <- adj(a, s, directions = 4, sort = TRUE, cutoff.for.data.table = i,
                      match.adjacent = TRUE, pairs = FALSE, torus = TRUE)
      expect_identical(sort(as.numeric(newCells)), c(1, 3, 8, 16))
      Mat <- terra::adjacent(a, s, directions = "bishop", pairs = TRUE)
      if (is(a, "SpatRaster")) {
        Mat <- t(as.matrix(Mat))
      }
      expect_equal(adj(a, s, directions = "bishop"), Mat)

      # a corner
      s <- 1
      newCells <- adj(a, s, directions = 4, sort = TRUE, cutoff.for.data.table = i,
                      match.adjacent = TRUE, pairs = FALSE, torus = TRUE)
      expect_identical(sort(as.numeric(newCells)), c(2, 4, 5, 13))
      Mat <- terra::adjacent(a, s, directions = "bishop", pairs = TRUE)
      if (is(a, "SpatRaster")) {
        Mat <- t(as.matrix(Mat))
      }
      expect_equal(adj(a, s, directions = "bishop"), Mat)

      # a side
      s <- 12
      newCells <- adj(a, s, directions = 4, sort = TRUE, cutoff.for.data.table = i,
                      match.adjacent = TRUE, pairs = FALSE, torus = TRUE)
      expect_identical(sort(as.numeric(newCells)), c(8, 9, 11, 16))
      expect_equal(adj(a, s, directions = "bishop"),
                   terra::adjacent(a, s, directions = "bishop", pairs = TRUE))

      # a corner
      s <- 16
      newCells <- adj(a, s, directions = 4, sort = TRUE, cutoff.for.data.table = i,
                      match.adjacent = TRUE, pairs = FALSE, torus = TRUE)
      expect_identical(sort(as.numeric(newCells)), c(4, 12, 13, 15))
      Mat <- terra::adjacent(a, s, directions = "bishop", pairs = TRUE)
      if (is(a, "SpatRaster")) {
        Mat <- t(as.matrix(Mat))
      }
      expect_equal(adj(a, s, directions = "bishop"), Mat)

      # a corner with 8 neighbours
      s <- 16
      newCells <- adj(a, s, directions = 8, sort = TRUE, cutoff.for.data.table = i,
                      match.adjacent = TRUE, pairs = FALSE, torus = TRUE)
      expect_identical(sort(as.numeric(newCells)), c(1, 3, 4, 9, 11, 12, 13, 15))
      Mat <- terra::adjacent(a, s, directions = "bishop", pairs = TRUE)
      if (is(a, "SpatRaster")) {
        Mat <- t(as.matrix(Mat))
      }
      expect_equal(adj(a, s, directions = "bishop"), Mat)

      # a corner with 8 neighbours
      s <- 1
      newCells <- adj(a, s, directions = 8, sort = TRUE, cutoff.for.data.table = i,
                      match.adjacent = TRUE, pairs = FALSE, torus = TRUE)
      expect_identical(sort(as.numeric(newCells)), c(2, 4, 5, 6, 8, 13, 14, 16))
      Mat <- terra::adjacent(a, s, directions = "bishop", pairs = TRUE)
      if (is(a, "SpatRaster")) {
        Mat <- t(as.matrix(Mat))
      }
      expect_equal(adj(a, s, directions = "bishop"), Mat)
    }
  }
})
