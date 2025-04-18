### Test on high variance simulated data

test_that("DEMIC produces correct PTRs on generated inputs 001", {
  O <- est_ptr(max_bin_001)

  # Too few contigs to effectively estimate higher PTRs, tolerance has to be much higher
  expect_equal(O$all_ptr$est_ptr, c(2, 3, 4), tolerance = 2)
  expect_equal(O$contigs_ptr$est_ptr, c(2, 3, 4), tolerance = 2)
  expect_null(O$samples_ptr)
  print(O)
})

test_that("DEMIC produces correct PTRs on generated inputs 002 on all", {
  O <- est_ptr_on_all(max_bin_002)

  expect_equal(O$est_ptr, c(2, 3, 4), tolerance = 0.3)
  print(O)
})

test_that("DEMIC produces correct PTRs on generated inputs 003 on contigs", {
  O <- est_ptr_on(max_bin_003, "contig")

  expect_equal(O$est_ptr, c(2, 3, 4), tolerance = 0.3)
  print(O)
})

### Test on low variance simulated data (from original paper)

EO1 <- structure(list(est_ptr = c(1.58820988778219, 2.29699398418901, 1.84056370898365), coefficient = c(0.409285576940177, 0.73580397477001, 0.539312123134913), pValue = c(2.61567654416603e-29, 2.05157018929424e-66, 2.43569174933219e-35), cor = c(0.767467786321584, 0.93543307519703, 0.819665386998375), correctY = c(1.4891508185809, 1.59052462645664, 1.42535597156592)), row.names = c("Sample1", "Sample2", "Sample3"), class = "data.frame")
EO2 <- structure(list(est_ptr = c(2.21303380999812, 1.75222762168833, 2.25436646747005), coefficient = c(0.626249889762292, 0.442604625244878, 0.64168504060637), pValue = c(2.57415826813133e-48, 1.28302075677619e-31, 6.40172677352955e-52), cor = c(0.976659750095046, 0.928251509604572, 0.980684877999788), correctY = c(0.309937783089312, 0.377917440682641, 0.304441148907541)), row.names = c("Sample1", "Sample2", "Sample3"), class = "data.frame")

test_that("DEMIC main function produces correct output on sourceforge data cluster 1", {
  O <- est_ptr(ContigCluster1)
  O$all_ptr <- abs(O$all_ptr)
  O$contigs_ptr <- abs(O$contigs_ptr)

  expect_equal(O$all_ptr, EO1, tolerance = 0.2)
  expect_equal(O$contigs_ptr, EO1, tolerance = 0.2)
  print(O)
})

test_that("DEMIC main function produces correct output on sourceforge data cluster 2", {
  O <- est_ptr(ContigCluster2)
  O$all_ptr <- abs(O$all_ptr)
  O$contigs_ptr <- abs(O$contigs_ptr)

  expect_equal(O$all_ptr, EO2, tolerance = 0.2)
  expect_equal(O$contigs_ptr, EO2, tolerance = 0.2)
  print(O)
})

### Test expected failures

test_that("DEMIC main function fails on empty input", {
  expect_error(est_ptr(data.frame()), "Input must have columns 'log_cov', 'GC_content', 'sample', 'contig', and 'length'")
})

test_that("DEMIC main function fails with too few contigs and samples", {
  expect_error(est_ptr(droplevels(ContigCluster1[1:10, ])), "Input must have at least 2 samples")
})
