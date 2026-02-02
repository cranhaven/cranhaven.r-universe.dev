context("distanceSP wrapper")

test_that("simSP calculated as anticipated", {
  ## resampled, uneven, best match: i.e., alignment object contains $sim:
  ant <- dtwSP(SPpairs$C_day3, SPpairs$C_day1, resamplingRate = 0.5, rescale2refHS = FALSE,
               checkGlobalAlignment = TRUE, bottom.up = TRUE, top.down = TRUE, keep.internals = TRUE,
               dims = c("hardness", "gtype"), weights = c(0.2, 0.8),
               window.size.abs = 30, nonMatchedSim = 0)
  dst <- distanceSP(SPpairs$C_day3, SPpairs$C_day1, symmetric = FALSE,
                    resamplingRate = 0.5, rescale2refHS = FALSE,
                    checkGlobalAlignment = TRUE, bottom.up = TRUE, top.down = TRUE, keep.internals = TRUE,
                    dims = c("hardness", "gtype"), weights = c(0.2, 0.8),
                    window.size.abs = 30, nonMatchedSim = 0)
  expect_equal(as.numeric(dst), 1 - ant$sim)

  ## only top.down, alignment object contains NO $sim:
  nMS <- 0
  ant2 <- dtwSP(SPpairs$C_day3, SPpairs$C_day1, resamplingRate = 0.5, rescale2refHS = FALSE,
             checkGlobalAlignment = FALSE, bottom.up = FALSE, top.down = TRUE, keep.internals = TRUE,
             dims = c("hardness", "gtype"), weights = c(0.2, 0.8),
             window.size.abs = 30, nonMatchedSim = nMS)
  dst2 <- distanceSP(SPpairs$C_day3, SPpairs$C_day1, symmetric = FALSE,
                     resamplingRate = 0.5, rescale2refHS = FALSE,
                     checkGlobalAlignment = FALSE, bottom.up = FALSE, top.down = TRUE, keep.internals = TRUE,
                     dims = c("hardness", "gtype"), weights = c(0.2, 0.8),
                     window.size.abs = 30, nonMatchedSim = nMS)
  expect_equal(as.numeric(dst2), (1 - simSP(ant2$reference, ant2$queryWarped, nonMatchedSim = nMS)))

})

