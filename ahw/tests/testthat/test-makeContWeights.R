library(timereg)
library(data.table)

set.seed(8642844)
# fr1 is a longitudinal data set with subjects that are diagnosed at time 0, and may
# be treated as time evolves. Subjects can die before receiving treatment:

# The method assumes there are no tied event times in the observed data. Although there are no
# tied event times in fr1, we use the function addNoiseAtEventTimes() for illustration here
fr1 <- as.data.table(fr1)
fr1 <- addNoiseAtEventTimes(fr1)

# Time to treatment and death are confounded by the baseline variable L. We want to
# mimic a scenario where time to treatment is randomized (and does not depend on L):
fFit <- aalen(Surv(from,to,to.state =="treat")~1 + L,data=fr1[fr1$from.state == "diag",])
cfFit <- aalen(Surv(from,to,to.state =="treat")~1,data=fr1[fr1$from.state == "diag",])

# We calculate and plot the weights
frame1 <- makeContWeights(fFit, cfFit, fr1, "diag", "treat", "from", "to",
                         "from.state", "to.state", "id", b = 0.4,
                         weightRange = c(0,5), willPlotWeights = FALSE)

# We fit a weighted model for the outcome. A is a treatment indicator (A=1 means treated).
a1 <- aalen(Surv(from,to,to.state =="death") ~ 1 + A,data=frame1,weights = frame1$weights)

# Next we consider an example with dependent censoring.
# Subjects are censored depending on a baseline variable u. We wish to mimic the
# cumulative hazard for death we would have seen if the censoring were independent.

faFit <- aalen(Surv(from,to,to.state=="Censored") ~ 1 + u, data = fFrame)
cfaFit <- aalen(Surv(from,to,to.state=="Censored") ~ 1, data = fFrame)

frame <- makeContWeights(faFit,cfaFit,fFrame,"Alive","Censored","from","to","from.state",
"to.state","id",100, willPlotWeights = FALSE)

fMod <- aalen(Surv(from,to,to.state=="Dead")~1,data=fFrame)
wMod <- aalen(Surv(from,to,to.state=="Dead")~1,data=frame,weights = frame$weights)

test_that("makeContWeights example produces expected output", {
  expect_length(a1, 20)
  expect_length(cfaFit, 20)
  expect_length(cfFit, 20)
  expect_length(faFit, 20)
  expect_length(fFit, 20)
  expect_length(fMod, 20)
  expect_length(wMod, 20)
  expect_length(fr1, 7)
  expect_length(frame, 7)
  expect_length(frame1, 8)
  expect_equal(a1$conf.band, c(3.0, 3.0), ignore_attr = TRUE, tolerance = 1e-1)
  # expect_equal(
  #   head(cfaFit$sim.testBeq0),
  #   c(1.449063, 1.937333, 2.008575, 1.409008, 2.257087, 1.804509),
  #   ignore_attr = TRUE, tolerance = 1e-5
  # )
  expect_equal(
    head(frame1$from),
    c(0.000, 0.020, 0.020, 0.022, 0.025, 0.025),
    ignore_attr = TRUE, tolerance = 1e-1
  )
  expect_equal(
    head(frame1$to),
    c(0.020, 0.020, 0.022, 0.025, 0.025, 0.027),
    ignore_attr = TRUE, tolerance = 1e-1
  )
  expect_equal(
    head(frame$from),
    c(0.000, 0.002, 0.006, 0.012, 0.016, 0.030),
    ignore_attr = TRUE, tolerance = 1e-1
  )
  expect_equal(
    head(frame$to),
    c(0.002, 0.006, 0.012, 0.016, 0.030, 0.040),
    ignore_attr = TRUE, tolerance = 1e-1
  )
  expect_equal(mean(frame1$weights), .99, ignore_attr = TRUE, tolerance = 1e-2)
  expect_equal(mean(frame$weights), .94, ignore_attr = TRUE, tolerance = 1e-2)
})
