test_that("CFS_scale returns correct structure", {


  dt.samples_trt <- get_test_samples_trt()

  all.sites <- dt.samples_trt$tr_all_wide[,.N, by = c("species", "uid_site", "site_id")][, N:=NULL]
  dupes <- all.sites[, .N, by = .(species, site_id)][N > 1]
  expect_equal(nrow(dupes), 0)


  # e.g. taking the target sites
  target_site <- all.sites[c(1,2), -"uid_site"]

  ref_sites <- merge(dt.samples_trt$tr_all_wide[,c("species", "uid_site", "site_id", "latitude","longitude", "uid_radius")], dt.samples_trt$tr_all_long$tr_7_ring_widths, by = c("uid_radius"))

  dt.scale <- growthTrendR::CFS_scale( target_site = target_site, ref_sites = ref_sites, scale.label_data_ref = "demo-samples", scale.max_dist_km = 200, scale.N_nbs = 5)

  expect_gt(length(names(dt.scale)), 0)
  expect_s3_class(dt.scale, "cfs_scale_list")


  # class tests
  expect_s3_class(dt.scale[[1]], "cfs_scale")

  # structure tests
  expect_named(dt.scale[[1]], c("dt.plots",     "ratio.median", "scale.parms" ))
  # element classes
  expect_true(is.list(dt.freq$freq.parms))
  expect_s3_class(dt.freq$dist_uids, "data.table")

})










test_that("CFS_qa returns correct structure", {


# loading processed data
dt.samples_trt <- get_test_samples_trt()
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


expect_gt(length(names(dt.qa)), 0)
expect_s3_class(dt.qa, "cfs_qa")

# structure tests

expect_named(dt.qa, c( "dt.ccf",   "dt.chron", "dt.stats", "dt.plots", "qa.parms" ))

# element classes

expect_s3_class(dt.qa$dt.ccf, "data.table")
expect_s3_class(dt.qa$dt.chron, "data.table")
expect_s3_class(dt.qa$dt.stats, "data.table")
expect_true(is.list(dt.qa$dt.plots))
expect_true(is.list(dt.qa$qa.parms))

})
