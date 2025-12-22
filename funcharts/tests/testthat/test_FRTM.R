skip_on_cran()
set.seed(1)
cores <- 1
data <- simulate_data_FRTM(n_obs = 20)

data_oc <-
  simulate_data_FRTM(
    n_obs = 2,
    scenario = "1",
    shift = "OC_h",
    severity = 0.5
  )

lambda <- 10 ^ -5
max_x <- max(unlist(data$grid_i))
seq_t_tot <- seq(0, 1, length.out = 30)[-1]
seq_x <- seq(0.1, max_x, length.out = 10)


mod_phaseI_FRTM <- FRTM_PhaseI(
  data_tra =  data,
  control.FDTW = list(
    M = 30,
    N = 30,
    lambda = lambda,
    seq_t = seq_t_tot,
    iter_tem = 1,
    iter = 1
  ),
  control.rtr = list(seq_x = seq_x),
  ncores = 1
)
mod_phaseII_FRTM <-
  FRTM_PhaseII(data_oc = data_oc , mod_phaseI = mod_phaseI_FRTM)

plot(mod_phaseI_FRTM)
plot(mod_phaseII_FRTM)
