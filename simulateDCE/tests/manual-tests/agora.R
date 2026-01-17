rm(list = ls())
# devtools::load_all()


designpath <- system.file("extdata", "agora", package = "simulateDCE")

resps <- 360 # number of respondents
nosim <- 10 # number of simulations to run (about 500 is minimum)

# betacoefficients should not include "-"

# design priors parameters
bcoeff <- list(
  basc = -1.2,
  basc2 = -1.4,
  baction = 0.1,
  badvisory = 0.4,
  bpartnertest = 0.3,
  bcomp = 0.02
)


# from survey

# basc = -2.2
# basc2 = -2.7
# baction = -0.15
# badvisory = 0.26
# bpartner = -0.09
# bcomp = 0.004



# place your utility functions here
ul <- list(
  u1 =
    list(
      v1 = V.1 ~ basc + baction * alt1.b + badvisory * alt1.c + bpartnertest * alt1.d + bcomp * alt1.p, # Utility of alternative 1
      v2 = V.2 ~ basc2 + baction * alt2.b + badvisory * alt2.c + bpartnertest * alt2.d + bcomp * alt2.p, # Utility of alternative 2
      v3 = V.3 ~ 0
    )
)

destype <- "ngene"

tictoc::tic("Total length Agora")

agora <- simulateDCE::sim_all(
  nosim = nosim, resps = resps, designtype = destype,
  designpath = designpath, u = ul, bcoeff = bcoeff, utility_transform_type = "exact", mode = "parallel"
)

tictoc::toc()
