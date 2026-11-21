survival_fixture_proportional <- function(n = 240) {
  set.seed(2601)
  trt <- rep(c("Control", "Treatment"), each = n / 2)
  age <- rep(seq(42, 78, length.out = n / 2), times = 2)
  rate <- 0.09 * ifelse(trt == "Treatment", 0.55, 1) * exp(0.018 * (age - 60))
  event_time <- stats::rexp(n, rate = rate)
  censor_time <- 20

  data.frame(
    time = pmin(event_time, censor_time),
    event = as.integer(event_time <= censor_time),
    trt = factor(trt, levels = c("Control", "Treatment")),
    age = age
  )
}

survival_fixture_median_unreached <- function() {
  data.frame(
    time = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
    event = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
    arm = factor(rep(c("A", "B"), each = 5))
  )
}
