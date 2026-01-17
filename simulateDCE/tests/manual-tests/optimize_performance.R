rm(list = ls())
devtools::load_all()


design <- system.file("extdata", "spdesigns", "designs", "twoattr.RDS", package = "simulateDCE")


resps <- 4000 # number of respondents
nosim <- 2 # number of simulations to run (about 500 is minimum)

# betacoefficients should not include "-"
bcoeff <- list(
  bx1A = 0.1, ## very high asc
  bx1B = -0.1,
  bx2A = 0.2,
  bx2B = -0.2
)


desisiongroups <- c(0, 0.3, 1)

ul <- list(
  uA =
    list(
      v1 = V.1 ~ bx1A * alt1.x1 + bx2A * alt1.x2,
      v2 = V.2 ~ bx1A * alt2.x1 + bx2A * alt2.x2
    ),
  uB = list(
    v1 = V.1 ~ bx1B * alt1.x1 + bx2B * alt1.x2,
    v2 = V.2 ~ bx1B * alt2.x1 + bx2B * alt2.x2
  )
)

simplesim <- sim_all(nosim = nosim, resps = resps, designpath = designpath, bcoeff = bcoeff, u = ul, designtype = "spdesign", decisiongroups = desisiongroups)

formattedes <- readdesign(design = design)

simulate_choices()
