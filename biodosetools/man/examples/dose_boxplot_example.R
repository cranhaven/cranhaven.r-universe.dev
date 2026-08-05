dat <- data.frame(
  Lab = c("A1", "A2", "A1", "A2", "A1", "A2"),
  Module = c("dicentrics", "dicentrics", "dicentrics", "dicentrics", "dicentrics", "dicentrics"),
  Type = c("manual", "manual", "manual", "manual", "manual", "manual"),
  Sample = c(1, 1, 2, 2, 3, 3),
  N = c(200, 200, 200, 200, 200, 200),
  X = c(140, 111, 204, 147, 368, 253),
  estimate = c(2.589230, 2.610970, 3.146055, 3.018682, 4.259400, 3.989222),
  lower = c(2.083403, 2.231150, 2.614931, 2.622748, 3.882349, 3.552963),
  upper = c(3.208857, 3.045616, 3.785879, 3.471269, 4.688910, 4.487336),
  y = c(0.700, 0.555, 1.020, 0.735, 1.840, 1.265),
  y.err = c(0.0610, 0.0495, 0.0733, 0.0556, 0.0923, 0.0785),
  DI = c(1.0625, 0.8818, 1.0539, 0.8406, 0.9255, 0.9731),
  u = c(0.6252, -1.1840, 0.5389, -1.5951, -0.7442, -0.2692),
  stringsAsFactors = FALSE
)

dose_boxplot(
  dat = dat,
  place = "UI"
)
