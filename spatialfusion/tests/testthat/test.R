Sys.setenv("R_TESTS" = "")
library(spatialfusion)
library(testthat)


# check simulation --------------------------------------------------------

  dat <- fusionSimulate(n.point = 50, n.area = 10, n.grid = 10, n.pred = 100,
                        psill = 1, phi = 1, nugget = 0, tau.sq = 1,
                        dimension = 10, domain = NULL, point.beta = list(rbind(1,5)),
                        area.beta = list(rbind(1, 1.5)), nvar.pp = 1,
                        distributions = c("normal","poisson"),
                        design.mat = matrix(c(2,1,1), ncol = 1), pp.offset = 1)

  expect_type(dat, "list")
  expect_type(dat$data, "list")


  # design mat
  expect_error(fusionSimulate(n.point = 50, n.area = 10, n.grid = 10, n.pred = 100,
                              psill = 1, phi = 1, nugget = 0, tau.sq = 1,
                              n.sampling = 5, sampling.type = "random", n.neighbor = 5,
                              dimension = 10, domain = NULL, point.beta = list(rbind(1,5)),
                              area.beta = list(rbind(1, 1.5)), nvar.pp = 1,
                              distributions = c("normal","poisson"),
                              design.mat = matrix(c(2,1,1,2), ncol = 2), pp.offset = 0.1))
  # distribution
  expect_error(fusionSimulate(n.point = 50, n.area = 10, n.grid = 10, n.pred = 100,
                              psill = 1, phi = 1, nugget = 0, tau.sq = 1,
                              n.sampling = 5, sampling.type = "random", n.neighbor = 5,
                              dimension = 10, domain = NULL, point.beta = list(rbind(1,5)),
                              area.beta = list(rbind(1, 1.5)), nvar.pp = 1,
                              distributions = c("normal","binomial"),
                              design.mat = matrix(c(2,1,1), ncol = 1), pp.offset = 0.1))

  # point.beta
  expect_error(fusionSimulate(n.point = 50, n.area = 10, n.grid = 10, n.pred = 100,
                              psill = 1, phi = 1, nugget = 0, tau.sq = 1,
                              n.sampling = 5, sampling.type = "random", n.neighbor = 5,
                              dimension = 10, domain = NULL, point.beta = list(rbind(1,5),rbind(2,3)),
                              area.beta = list(rbind(1, 1.5)), nvar.pp = 1,
                              distributions = c("normal","poisson"),
                              design.mat = matrix(c(2,1,1), ncol = 1), pp.offset = 0.1))

  # tau.sq with distribution
  expect_error(fusionSimulate(n.point = 50, n.area = 10, n.grid = 10, n.pred = 100,
                              psill = 1, phi = 1, nugget = 0, tau.sq = 1,
                              n.sampling = 5, sampling.type = "random", n.neighbor = 5,
                              dimension = 10, domain = NULL, point.beta = list(rbind(1,5)),
                              area.beta = list(rbind(1, 1.5)), nvar.pp = 1,
                              distributions = c("normal","normal"),
                              design.mat = matrix(c(2,1,1), ncol = 1), pp.offset = 0.1))

  # dimension
  expect_error(fusionSimulate(n.point = 50, n.area = 10, n.grid = 10, n.pred = 100,
                              psill = 1, phi = 1, nugget = 0, tau.sq = 1,
                              n.sampling = 5, sampling.type = "random", n.neighbor = 5,
                              dimension = 0, domain = NULL, point.beta = list(rbind(1,5)),
                              area.beta = list(rbind(1, 1.5)), nvar.pp = 1,
                              distributions = c("normal","poisson"),
                              design.mat = matrix(c(2,1,1), ncol = 1), pp.offset = 0.1))




# check data preparation --------------------------------------------------------

geo.data <- data.frame(x = dat$mrf[dat$sample.ind, "x"], y = dat$mrf[dat$sample.ind, "y"],
                       cov.point = dat$data$X_point[,2], outcome = dat$data$Y_point[[1]])

lattice.data <-cbind(dat$poly,
                       data.frame(outcome = dat$data$Y_area[[1]],
                                  cov.area = dat$data$X_area[,2]))
pp.data <-dat$data$lgcp.coords[[1]]
expect_s3_class(fusionData(geo.data = geo.data, geo.formula = outcome ~ cov.point, lattice.data = lattice.data, lattice.formula = outcome ~ cov.area,
                   pp.data = pp.data, distributions = c("normal", "poisson"), method = "INLA"), "dinla")

expect_s3_class(fusionData(geo.data = geo.data, geo.formula = outcome ~ cov.point, lattice.data = lattice.data, lattice.formula = outcome ~ cov.area,
                           pp.data = pp.data, distributions = c("normal", "poisson"), method = "Stan"), "dstan")

expect_s3_class(fusionData(geo.data = geo.data, geo.formula = outcome ~ cov.point, lattice.data = lattice.data, lattice.formula = outcome ~ cov.area,
                           distributions = c("normal", "poisson"), method = "Stan"), "dstan")

expect_s3_class(fusionData(geo.data = geo.data, geo.formula = outcome ~ cov.point, lattice.data = lattice.data, lattice.formula = outcome ~ cov.area,
                           distributions = c("normal", "poisson"), method = "INLA"), "dinla")


# domain no lattice
expect_error(fusionData(geo.data = geo.data, geo.formula = outcome ~ cov.point, method = "Stan"))
expect_error(fusionData(geo.data = geo.data, geo.formula = outcome ~ cov.point, method = "INLA"))


# geo.formula
expect_error(fusionData(geo.data = geo.data, geo.formula = outcome2 ~ cov.point, lattice.data = lattice.data, lattice.formula = outcome ~ cov.area,
                        pp.data = pp.data, distributions = c("normal", "poisson"), method = "INLA"))

expect_error(fusionData(geo.data = geo.data, geo.formula = outcome2 ~ cov.point, lattice.data = lattice.data, lattice.formula = outcome ~ cov.area,
                        pp.data = pp.data, distributions = c("normal", "poisson"), method = "Stan"))

# lattice.data
expect_error(fusionData(geo.data = geo.data, geo.formula = outcome ~ cov.point, lattice.data = lattice.data@data, lattice.formula = outcome ~ cov.area,
                        pp.data = pp.data, distributions = c("normal", "poisson"), method = "INLA"))

expect_error(fusionData(geo.data = geo.data, geo.formula = outcome ~ cov.point, lattice.data = lattice.data@data, lattice.formula = outcome ~ cov.area,
                        pp.data = pp.data, distributions = c("normal", "poisson"), method = "Stan"))

# pp.data
expect_error(fusionData(geo.data = geo.data, geo.formula = outcome ~ cov.point, lattice.data = lattice.data, lattice.formula = outcome ~ cov.area,
                        pp.data = expand.grid(1:10,1:10), distributions = c("normal", "poisson"), method = "INLA"))

expect_error(fusionData(geo.data = geo.data, geo.formula = outcome ~ cov.point, lattice.data = lattice.data, lattice.formula = outcome ~ cov.area,
                        pp.data = expand.grid(1:10,1:10), distributions = c("normal", "poisson"), method = "Stan"))

