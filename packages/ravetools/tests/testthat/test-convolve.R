test_that("convolve_image", {
  testthat::skip_on_cran()
  testthat::skip_if(system.file(package = "imager") == '')

  # Do NOT test on CRAN, `imager` does not install well, I don't want to
  # add it to `Suggests`
  imager <- asNamespace("imager")
  boats <- imager$as.cimg(imager$.__NAMESPACE__.$lazydata$boats[,,1,1])
  filter <- imager$as.cimg(function(x,y) sign(x-5),11,11)

  x <- imager$convolve(boats, filter = filter)
  y <- imager$as.cimg(ravetools:::convolve_image(boats, filter = filter))
  testthat::expect_equal(y, x, tolerance = 1e-10)

  boats <- imager$as.cimg(t(imager$.__NAMESPACE__.$lazydata$boats[,,1,1]))
  filter <- imager$as.cimg(function(x,y) sign(x-5),12,12)

  x <- imager$convolve(boats, filter = filter)
  y <- imager$as.cimg(ravetools:::convolve_image(boats, filter = filter))
  testthat::expect_equal(y, x, tolerance = 1e-10)

})



test_that("convolve_volume", {
  testthat::skip_on_cran()
  testthat::skip_if(system.file(package = "imager") == '')

  # Do NOT test on CRAN, `imager` does not install well, I don't want to
  # add it to `Suggests`
  imager <- asNamespace("imager")
  boats <- as.array(imager$.__NAMESPACE__.$lazydata$boats)
  dim(boats) <- dim(boats)[c(1,2,4,3)]
  boats <- imager$as.cimg(boats)
  filter <- imager$as.cimg(function(x,y,z) sign(x-5),11,11, 11)

  x <- imager$convolve(boats, filter = filter)
  y <- imager$as.cimg(structure(ravetools:::convolve_volume(boats, filter = filter), dim = dim(boats)))
  testthat::expect_equal(y, x, tolerance = 1e-10)

  boats <- as.array(imager$.__NAMESPACE__.$lazydata$boats)
  dim(boats) <- dim(boats)[c(4,2,1,3)]
  boats <- imager$as.cimg(boats)
  filter <- imager$as.cimg(function(x,y,z) sign(x-5),12,12, 12)

  x <- imager$convolve(boats, filter = filter)
  y <- imager$as.cimg(structure(ravetools:::convolve_volume(boats, filter = filter), dim = dim(boats)))
  testthat::expect_equal(y, x, tolerance = 1e-10)

  # imager <- asNamespace("imager")
  # boats <- imager$as.cimg(array(rnorm(256*256*256), c(rep(256,3), 1)))
  # filter <- imager$as.cimg(function(x,y,z) sign(x-5),11,11, 11)
  # microbenchmark::microbenchmark(
  #   {imager$convolve(boats, filter = filter)},
  #   {imager$as.cimg(structure(ravetools:::convolve_volume(boats, filter = filter), dim = dim(boats)))}, times = 1
  # )

})
