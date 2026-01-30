
test_that("gam_mod returns correct structure", {


  # loading processed data
  dt.samples_trt <- get_test_samples_trt()
  # data processing
  dt.samples_long <- prepare_samples_clim(dt.samples_trt)
  dt.samples_long$uid_site.fac <- as.factor(as.character(dt.samples_long$uid_site))
  dt.m <- dt.samples_long
  m.gam <- gam_mod(data = dt.m, resp_scale = "resp_gaussian", m.candidates = "rw_mm ~ s(year, by = uid_site.fac)")
  check_cfs_model(m.gam)

})
test_that("gamm_radius returns correct structure", {


  # loading processed data
  dt.samples_trt <- get_test_samples_trt()
  # data processing
  dt.samples_long <- prepare_samples_clim(dt.samples_trt)
dt.m <- dt.samples_long[uid_radius == 1]
m.radius <- gamm_radius(data = dt.m, resp_scale = "resp_gaussian", m.candidates = "rw_mm ~ s(year)")
check_cfs_model(m.radius)

  })


test_that("gamm_site returns correct structure", {


  # loading processed data
  dt.samples_trt <- get_test_samples_trt()
  # climate
  dt.clim <- fread(system.file("extdata", "dt.clim.csv", package = "growthTrendR"))
  # pre-data for model
  dt.samples_clim <- prepare_samples_clim(dt.samples_trt, dt.clim)

  dt.m <- dt.samples_clim[uid_site == 1][ageC >1]

  # gamm_site model
  m.site <-growthTrendR::gamm_site(data = dt.m, resp_scale = "resp_log",
                                m.candidates = c( "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + s(FFD)",
                                                  "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + FFD"))
  # check model output
  check_cfs_model(m.site)

})

test_that("gamm_spatial returns correct structure", {


  # loading processed data
  dt.samples_trt <- get_test_samples_trt()
  # climate
  dt.clim <- fread(system.file("extdata", "dt.clim.csv", package = "growthTrendR"))
  # pre-data for model
  dt.samples_clim <- prepare_samples_clim(dt.samples_trt, dt.clim)

  dt.m <- dt.samples_clim[ageC >1]

  # gamm_site model
  m.spatial <-growthTrendR::gamm_spatial(data = dt.m, resp_scale = "resp_gamma",
                                   m.candidates = c( "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + s(FFD)",
                                                     "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + FFD"))
  # check model output
  check_cfs_model(m.spatial)

})

test_that("bam_spatial returns correct structure", {


  # loading processed data
  dt.samples_trt <- get_test_samples_trt()
  # climate
  dt.clim <- fread(system.file("extdata", "dt.clim.csv", package = "growthTrendR"))
  # pre-data for model
  dt.samples_clim <- prepare_samples_clim(dt.samples_trt, dt.clim)

  dt.m <- dt.samples_clim[ageC >1]

  # gamm_site model
  m.spatial_bam <-growthTrendR::bam_spatial(data = dt.m, resp_scale = "resp_log",
                                         m.candidates = c( "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + s(FFD)",
                                                           "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + FFD"))
  # check model output
  check_cfs_model(m.spatial_bam)

})



test_that("ci_resp returns correct structure", {
  # loading processed data
  dt.samples_trt <- get_test_samples_trt()
  # climate
  dt.clim <- fread(system.file("extdata", "dt.clim.csv", package = "growthTrendR"))
  # pre-data for model
  dt.samples_clim <- prepare_samples_clim(dt.samples_trt, dt.clim)

  dt.m <- dt.samples_clim[ageC >1]

  # gamm_site model
  m.spatial <-growthTrendR::gamm_spatial(
    data = dt.m, resp_scale = "resp_log",
    m.candidates = c( "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + s(FFD)"))

 if ( inherits(m.spatial$model, "gamm")) gam_model <- m.spatial$model$gam else gam_model <- m.spatial$model
nd <- copy(dt.m)
nd[, uid_site.fac:= as.factor(as.character(uid_site))]

dt.ci <- ci_resp(gam_model, newdata = nd)

testthat::expect_s3_class(dt.ci, "data.table")
# Check for required columns
testthat::expect_contains(names(dt.ci), c("fit", "lwr", "upr","ci_method"))
testthat::expect_false(anyNA(dt.ci[, .(fit, lwr, upr, ci_method)]))

})

test_that("sterm_imp returns correct structure", {
  # loading processed data
  dt.samples_trt <- get_test_samples_trt()
  # climate
  dt.clim <- fread(system.file("extdata", "dt.clim.csv", package = "growthTrendR"))
  # pre-data for model
  dt.samples_clim <- prepare_samples_clim(dt.samples_trt, dt.clim)

  dt.m <- dt.samples_clim[ageC >1]

  # gamm_site model
  m.spatial <-growthTrendR::gamm_spatial(
    data = dt.m, resp_scale = "resp_log",
    m.candidates = c( "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + s(FFD)"))
  if ( inherits(m.spatial$model, "gamm")) gam_model <- m.spatial$model$gam else gam_model <- m.spatial$model

dt.imp <- sterm_imp(gam_model)

testthat::expect_s3_class(dt.imp, "data.table")
# Check for required columns
testthat::expect_contains(names(dt.imp), c("term", "importance_pct", "method"))
testthat::expect_false(anyNA(dt.imp[, .(term, importance_pct,method)]))
})
