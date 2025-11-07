library(data.table)
library(timereg)

# fr1 is a longitudinal data set with subjects that are diagnosed at time 0, and may
# be treated as time evolves. Subjects can die before receiving treatment

# The method assumes there are no tied event times in the observed data. Although there are no
# tied event times in fr1, we use the function addNoiseAtEventTimes() for illustration here
fr1 <- as.data.table(fr1)
fr1 <- addNoiseAtEventTimes(fr1)

# Time to treatment and death are confounded by the baseline variable L. We want to
# mimic a scenario where time to treatment is randomized (and does not depend on L):
fr1_diag <- fr1[fr1$from.state == "diag", ]
fFit <- aalen(
  Surv(from, to, to.state == "treat") ~ 1 + L, data = fr1_diag, n.sim = 50L,
  robust = 0
)
cfFit <- aalen(
  Surv(from, to, to.state == "treat") ~ 1, data = fr1_diag, n.sim = 50L,
  robust = 0
)

# We calculate and plot the weights
frame1 <- makeContWeights(fFit, cfFit, fr1, "diag", "treat", "from", "to",
  "from.state", "to.state", "id",
  b = 0.4,
  weightRange = c(0, 5)
)

# We fit a weighted model for the outcome. A is a treatment indicator (A=1 means treated).
a1 <- aalen(
  Surv(from, to, to.state == "death") ~ 1 + A, data = frame1,
  weights = frame1$weights, n.sim = 50L, robust = 0
)

# We plot the A coefficient from the weighted regression,
# and compare with the true hypothetical coefficient
plot(a1$cum[, c(1, 3)],
  type = "s", ylim = c(-1.2, 0.5), xlim = c(0, 5),
  main = "Weighted additive hazard treatment coefficient"
)
lines(Tmat, col = 2)
legend("bottomleft", c("weighted estimate", "hypothetical treatment coef"),
  lty = 1, col = c(1, 2), bty = "n"
)

# Next we consider an example with dependent censoring.
# Subjects are censored depending on a baseline variable u. We wish to mimic the
# cumulative hazard for death we would have seen if the censoring were independent.

faFit <- aalen(
  Surv(from, to, to.state == "Censored") ~ 1 + u, data = fFrame, n.sim = 50L,
  robust = 0
)
cfaFit <- aalen(
  Surv(from, to, to.state == "Censored") ~ 1, data = fFrame, n.sim = 50L,
  robust = 0
)

frame <- makeContWeights(
  faFit, cfaFit, fFrame, "Alive", "Censored", "from", "to", "from.state",
  "to.state", "id", 100
)

fMod <- aalen(
  Surv(from, to, to.state == "Dead") ~ 1, data = fFrame, n.sim = 50L,
  robust = 0
)
wMod <- aalen(
  Surv(from, to, to.state == "Dead") ~ 1, data = frame, weights = frame$weights,
  n.sim = 50L, robust = 0
)

plot(fMod$cum, type = "s", main = "Nelson-Aalen for death", ylab = "")
lines(wMod$cum, type = "s", col = "red")
legend("topleft", c("factual", "weighted factual"), lty = 1, col = c(1, "red"), bty = "n")
