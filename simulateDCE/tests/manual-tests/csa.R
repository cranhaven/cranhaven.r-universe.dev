rm(list = ls())
devtools::load_all()





set.seed(3393)

designpath <- system.file("extdata", "CSA", "linear", package = "simulateDCE")

library(rlang)
library(formula.tools)







notes <- "No Heuristics"

resps <- 240 # number of respondents
nosim <- 2 # number of simulations to run (about 500 is minimum)

destype <- "spdesign"

bcoeff <- list(
  bx1 = -0.02,
  bx2 = -0.03,
  bx3 = -0.02,
  bx4 = -0.005,
  bnobuy = -1
)

# place your utility functions here
ul <- list(u1 = list(
  v1 = V.1 ~ bx1 * alt1.x1 + bx2 * alt1.x2 + bx3 * alt1.x3 + bx4 * alt1.x4,
  v2 = V.2 ~ bx1 * alt2.x1 + bx2 * alt2.x2 + bx3 * alt2.x3 + bx4 * alt2.x4,
  v3 = V.3 ~ bnobuy
))

savefile <- "testdir/file"

csa <- simulateDCE::sim_all(
  nosim = nosim, resps = resps, designtype = destype,
  designpath = designpath, u = ul, bcoeff = bcoeff, utility_transform_type = "exact", savefile =
    NULL, mode = "sequential"
)
