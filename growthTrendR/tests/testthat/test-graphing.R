test_that("plot_scale returns correct structure", {


  dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))

  all.sites <- dt.samples_trt$tr_all_wide[,.N, by = c("species", "uid_site", "site_id")][, N:=NULL]
  dupes <- all.sites[, .N, by = .(species, site_id)][N > 1]
  expect_equal(nrow(dupes), 0)


  # e.g. taking the target sites
  target_site <- all.sites[c(1,2), -"uid_site"]

  ref_sites <- merge(dt.samples_trt$tr_all_wide[,c("species", "uid_site", "site_id", "latitude","longitude", "uid_radius")], dt.samples_trt$tr_all_long$tr_7_ring_widths, by = c("uid_radius"))

  dt.scale <- growthTrendR::CFS_scale( target_site = target_site, ref_sites = ref_sites, scale.label_data_ref = "demo-samples", scale.max_dist_km = 200, scale.N_nbs = 5)
  plots.lst <- plot_scale(dt.scale[[1]])

  # structure tests
  expect_named(plots.lst, c("plot.year", "plot.ll" ))
  lapply(plots.lst, function(p) expect_s3_class(p, "ggplot"))
})


test_that("plot_qa returns correct structure", {



  # loading processed data
  dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))
  # data processing
  dt.samples_long <- prepare_samples_clim(dt.samples_trt = dt.samples_trt, calbai = FALSE )

  # rename to the reserved column name
  setnames(dt.samples_long, c("sample_id", "year", "rw_mm"), c("SampleID", "Year" ,"RawRing"))

  # assign treated series
  # users can decide their own treated series
  # dt.samples_long[, RW_trt:= RawRing - shift(RawRing), by = SampleID]

  # for rhub::rhub_check() on macos VECTOR_ELT issues
  data.table::setorder(dt.samples_long, SampleID, Year)
  dt.samples_long$RW_trt <-
   ave(
      as.numeric(dt.samples_long$RawRing),
      dt.samples_long$SampleID,
      FUN = function(x)
      if (length(x) > 1L) c(NA_real_, diff(x)) else NA_real_
     )

  # quality check on radius alignment based on the treated series
  dt.qa <-CFS_qa(dt.input = dt.samples_long, qa.label_data = "demo-samples", qa.label_trt = "difference", qa.min_nseries = 5)
  plots.lst <- plot_qa(dt.qa, qa.out_series = "X003_101_005")

  # structure tests
  expect_named(plots.lst, c("plot.raw.series", "plot.trt.series", "plot.raw.ccf" ,   "plot.trt.ccf" ))
  lapply(plots.lst, function(p) expect_s3_class(p[[1]], "ggplot"))
})

test_that("plot_freq returns correct structure", {


  # loading processed data
  dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))

  dt.freq <- CFS_freq(dt.samples_trt$tr_all_wide, freq.label_data = "demo-samples", freq.uid_level = "uid_radius")



  plots.lst <- plot_freq(dt.freq)

  # structure tests

  expect_true(is.list(plots.lst))
  expect_s3_class(plots.lst[[1]], "ggplot")
})

test_that("plot_resp returns correct structure", {
  # loading processed data
  dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))
  # climate
  dt.clim <- fread(system.file("extdata", "dt.clim.csv", package = "growthTrendR"))
  # pre-data for model
  dt.samples_clim <- prepare_samples_clim(dt.samples_trt, dt.clim)

  dt.m <- dt.samples_clim[ageC >1]

  # gamm_site model
  m.spatial <-growthTrendR::gamm_spatial(
    data = dt.m, resp_scale = "resp_log",
    m.candidates = c( "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + s(FFD)"))

  plots.lst <- plot_resp(m.spatial)

  testthat::expect_true(is.list(plots.lst))
  if (length(plots.lst) > 0) expect_s3_class(plots.lst[[1]], "ggplot")

})
