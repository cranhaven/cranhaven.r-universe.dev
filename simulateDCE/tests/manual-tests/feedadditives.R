rm(list = ls())
devtools::load_all()

designpath <- system.file("extdata", "feedadditives", package = "simulateDCE")

# notes <- "This design consists of different heuristics. One group did not attend the methan attribute, another group only decided based on the payment"

notes <- "Three heuristics"

resps <- 396 # number of respondents
nosim <- 2 # number of simulations to run (about 500 is minimum)
destype <- "ngene"

# betacoefficients should not include "-"
bcoeff <- list(
  basc = 0.2,
  bcow = 0.3,
  badv = 0.3,
  bvet = 0.3,
  bfar = 0.3,
  bmet = 0.3,
  bbon = 0.3,
  bbon2 = 1.9,
  basc2 = 2
)

decisiongroups <- c(0, 0.3, 0.6, 0.8, 1)

# place your utility functions here
ul <- list(
  u1 = list(
    v1 = V.1 ~ bcow * alt1.cow + badv * alt1.adv + bvet * alt1.vet + bfar * alt1.far + bmet * alt1.met + bbon * alt1.bon,
    v2 = V.2 ~ bcow * alt2.cow + badv * alt2.adv + bvet * alt2.vet + bfar * alt2.far + bmet * alt2.met + bbon * alt2.bon,
    v3 = V.3 ~ basc
  ),
  u2 = list(
    v1 = V.1 ~ bcow * alt1.cow + badv * alt1.adv + bvet * alt1.vet + bfar * alt1.far + bbon * alt1.bon,
    v2 = V.2 ~ bcow * alt2.cow + badv * alt2.adv + bvet * alt2.vet + bfar * alt2.far + bbon * alt2.bon,
    v3 = V.3 ~ basc
  ),
  u3 = list(
    v1 = V.1 ~ bbon2 * alt1.bon,
    v2 = V.2 ~ bbon2 * alt2.bon,
    v3 = V.3 ~ basc
  ),
  u4 = list(
    v1 = V.1 ~ basc2 + bcow * alt1.cow + badv * alt1.adv + bvet * alt1.vet + bfar * alt1.far + bmet * alt1.met + bbon * alt1.bon,
    v2 = V.2 ~ bcow * alt2.cow + badv * alt2.adv + bvet * alt2.vet + bfar * alt2.far + bmet * alt2.met + bbon * alt2.bon,
    v3 = V.3 ~ basc
  )
)

feedadditives <- sim_all(
  nosim = nosim, resps = resps, designtype = destype,
  designpath = designpath, u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups
)
