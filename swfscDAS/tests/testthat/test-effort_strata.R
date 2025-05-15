test_that("effort distance and sightings are the same with or without strata splitting", {
  y <- system.file("das_sample.das", package = "swfscDAS")
  y.proc <- das_process(y)
  stratum.file <- system.file("das_sample_stratum.csv", package = "swfscDAS")
  # y.rand <- system.file("das_sample_randpicks.csv", package = "swfscDAS")

  # Using "condition" method
  eff.cond.nostrata <- das_effort(
    y.proc, method = "condition", conditions = c("Bft", "SwellHght", "Vis"),
    seg.min.km = 0.05, num.cores = 1
  )

  eff.cond.strata <- das_effort(
    y.proc, method = "condition", conditions = c("Bft", "SwellHght", "Vis"),
    strata.files = list(stratum.file),
    seg.min.km = 0.05, num.cores = 1
  )

  # Using "section" method
  eff.sect.nostrata <- das_effort(y.proc, method = "section", num.cores = 1)

  eff.sect.strata <- das_effort(
    y.proc, method = "section", strata.files = list(Poly1 = stratum.file),
    num.cores = 1
  )

  # Using "equallength" method
  eff.eqln.nostrata <- das_effort(
    y.proc, method = "equallength", seg.km = 5,
    num.cores = 1
  )

  eff.eqln.strata <- das_effort(
    y.proc, method = "equallength", seg.km = 5,
    strata.files = list(Poly1 = stratum.file),
    num.cores = 1
  )

  # Check that distances are equal
  expect_equal(sum(eff.cond.nostrata$segdata$dist), sum(eff.cond.strata$segdata$dist))
  expect_equal(sum(eff.sect.nostrata$segdata$dist), sum(eff.sect.strata$segdata$dist))
  expect_equal(sum(eff.eqln.nostrata$segdata$dist), sum(eff.eqln.strata$segdata$dist))

  # Check that sightings are the same other than segnum
  col.torm <- c("segnum", "mlat", "mlon")
  expect_identical(subset(eff.cond.nostrata$sightinfo, select = -c(segnum, mlat, mlon)),
                   subset(eff.cond.strata$sightinfo, select = -c(segnum, mlat, mlon)))
  expect_identical(subset(eff.sect.nostrata$sightinfo, select = -c(segnum, mlat, mlon)),
                   subset(eff.sect.strata$sightinfo, select = -c(segnum, mlat, mlon)))
  expect_identical(subset(eff.eqln.nostrata$sightinfo, select = -c(segnum, mlat, mlon)),
                   subset(eff.eqln.strata$sightinfo, select = -c(segnum, mlat, mlon)))
})
