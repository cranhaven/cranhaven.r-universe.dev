rm(list = ls())
devtools::load_all()
library(rlang)


designpath <- system.file("extdata", "SE_DRIVE", package = "simulateDCE")

resps <- 120 # number of respondents
nosim <- 2 # number of simulations to run (about 500 is minimum)

decisiongroups <- c(0, 0.7, 1)

# pass beta coefficients as a list
bcoeff <- list(
  b.preis = -0.01,
  b.lade = -0.07,
  b.warte = 0.02
)

manipulations <- list(
  alt1.x2 = expr(alt1.x2 / 10),
  alt1.x3 = expr(alt1.x3 / 10),
  alt2.x2 = expr(alt2.x2 / 10),
  alt2.x3 = expr(alt2.x3 / 10)
)


# place your utility functions here
ul <- list(
  u1 =

    list(
      v1 = V.1 ~ b.preis * alt1.x1 + b.lade * alt1.x2 + b.warte * alt1.x3,
      v2 = V.2 ~ b.preis * alt2.x1 + b.lade * alt2.x2 + b.warte * alt2.x3
    ),
  u2 = list(
    v1 = V.1 ~ b.preis * alt1.x1,
    v2 = V.2 ~ b.preis * alt2.x1
  )
)


sedrive <- sim_all(
  nosim = nosim, resps = resps,
  designpath = designpath, u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups,
  manipulations = manipulations, utility_transform_type = "exact", mode = "sequential"
)
