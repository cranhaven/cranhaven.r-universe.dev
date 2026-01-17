rm(list = ls())
devtools::load_all()
### currently used to test the chunks option

set.seed(3393)

designpath <- system.file("extdata", "Rbook", package = "simulateDCE")
# notes <- "This design consists of different heuristics. One group did not attend the methan attribute, another group only decided based on the payment"

notes <- "No Heuristics"

resps <- 240 # number of respondents
nosim <- 19 # number of simulations to run (about 500 is minimum)

# betacoefficients should not include "-"
bcoeff <- list(
  bsq = 0.00,
  bredkite = -0.05,
  bdistance = 0.50,
  bcost = -0.05,
  bfarm2 = 0.25,
  bfarm3 = 0.50,
  bheight2 = 0.25,
  bheight3 = 0.50
)


destype <- "spdesign"


# place your utility functions here
ul <- list(u1 = list(
  v1 = V.1 ~ bsq * alt1.sq,
  v2 = V.2 ~ bfarm2 * alt2.farm2 + bfarm3 * alt2.farm3 + bheight2 * alt2.height2 + bheight3 * alt2.height3 + bredkite * alt2.redkite + bdistance * alt2.distance + bcost * alt2.cost,
  v3 = V.3 ~ bfarm2 * alt3.farm2 + bfarm3 * alt3.farm3 + bheight2 * alt3.height2 + bheight3 * alt3.height3 + bredkite * alt3.redkite + bdistance * alt3.distance + bcost * alt3.cost
))



rbook <- simulateDCE::sim_all(
  nosim = nosim, resps = resps, destype = destype,
  designpath = designpath, u = ul, bcoeff = bcoeff, chunks = 4, utility_transform_type = "exact"
)
