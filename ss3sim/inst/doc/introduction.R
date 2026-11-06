## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ---- install-and-load, eval=FALSE---------------------------------------
#  # CRAN version
#  install.packages("ss3sim")
#  # Github version, which depends on "devtools"
#  # install.packages("devtools")
#  devtools::install_github("ss3sim/ss3sim",
#    ref = "development", dependencies = TRUE, build_vignettes = TRUE)
#  # Load the package
#  library("ss3sim")

## ---- help, eval=FALSE---------------------------------------------------
#  ?ss3sim
#  help(package = "ss3sim")
#  browseVignettes("ss3sim")

## ---- locate-folders-----------------------------------------------------
library(ss3sim)
d <- system.file("extdata", package = "ss3sim")
case_folder <- file.path(d, "eg-cases")
om <- file.path(d, "models", "cod-om")
em <- file.path(d, "models", "cod-em")

## ---- case-file-checks, eval=FALSE---------------------------------------
#  run_ss3sim(iterations = 1, scenarios =
#    c("D0-E0-F0-M0-cod",
#      "D1-E0-F0-M0-cod",
#      "D0-E1-F0-M0-cod",
#      "D1-E1-F0-M0-cod"),
#    case_files = list(F = "F", D = c("index", "lcomp", "agecomp"),
#      E = "E", M = "M"),
#    case_folder = case_folder, om_dir = om,
#    em_dir = em)

## ------------------------------------------------------------------------
recdevs_det <- matrix(0, nrow = 100, ncol = 20)

## ---- deterministic-runs, eval=FALSE-------------------------------------
#  run_ss3sim(iterations = 1:20,
#    scenarios = c("D100-E100-F0-M0-cod", "D100-E101-F0-M0-cod"),
#    case_files = list(F = "F", D = c("index", "lcomp", "agecomp"),
#      E = "E", M = "M"),
#    case_folder = case_folder, om_dir = om, em_dir = em,
#    bias_adjust = TRUE, user_recdevs = recdevs_det)

## ---- deterministic-runs-expand, eval=FALSE------------------------------
#  x <- expand_scenarios(list(D = 100, E = 100:101, F = 0, M = 0),
#    species = "cod")
#  run_ss3sim(iterations = 1:20, scenarios = x,
#    case_folder = case_folder, om_dir = om, em_dir = em,
#    bias_adjust = TRUE, user_recdevs = recdevs_det,
#    case_files = list(F = "F", D = c("index", "lcomp", "agecomp"),
#      E = "E", M = "M"))

## ---- stochastic-runs, eval=FALSE----------------------------------------
#  run_ss3sim(iterations = 1:100, scenarios =
#    c("D0-E0-F0-M0-cod",
#      "D1-E0-F0-M0-cod",
#      "D0-E1-F0-M0-cod",
#      "D1-E1-F0-M0-cod"),
#    case_files = list(F = "F", D = c("index", "lcomp", "agecomp"),
#      E = "E", M = "M"),
#    case_folder = case_folder, om_dir = om,
#    em_dir = em, bias_adjust = TRUE)

## ---- get-results, eval=FALSE--------------------------------------------
#  get_results_all(user_scenarios =
#    c("D100-E100-F0-M0-cod",
#      "D100-E101-F0-M0-cod",
#      "D0-E0-F0-M0-cod",
#      "D1-E0-F0-M0-cod",
#      "D0-E1-F0-M0-cod",
#      "D1-E1-F0-M0-cod"))
#  # Read in the data frames stored in the csv files
#  scalar_dat <- read.csv("ss3sim_scalar.csv")
#  ts_dat <- read.csv("ss3sim_ts.csv")

## ---- load-output--------------------------------------------------------
data("scalar_dat", package = "ss3sim")
data("ts_dat", package = "ss3sim")

## ---- transform-output---------------------------------------------------
scalar_dat <- calculate_re(scalar_dat, add = TRUE)
ts_dat <- calculate_re(ts_dat, add = TRUE)
ts_dat <- merge(ts_dat, scalar_dat[,c("scenario", "iteration",
    "max_grad")])

scalar_dat_det <- scalar_dat[scalar_dat$E %in% c("E100", "E101"), ]
scalar_dat_sto <- scalar_dat[scalar_dat$E %in% c("E0", "E1"), ]
ts_dat_det <- ts_dat[ts_dat$E %in% c("E100", "E101"), ]
ts_dat_sto <- ts_dat[ts_dat$E %in% c("E0", "E1"), ]

scalar_dat_long <- scalar_dat
colnames(scalar_dat_long) <- gsub("(.+)_re", "RE _\\1", colnames(scalar_dat_long))
scalar_dat_long <- reshape(scalar_dat_long, sep = " _", 
  direction = "long",
  varying = grep(" _", colnames(scalar_dat_long)),
  idvar = c("scenario", "iteration"),
  timevar = "parameter")

## ---- relative-error-boxplots-det, fig.height=7, fig.width=5, fig.cap="Box plots of the relative error (RE) for deterministic runs. *M* is fixed at the true value from the OM (E100) or estimated (E101). The standard deviation on the survey index observation error is 0.001 (D100)."----
p <- plot_scalar_boxplot(scalar_dat_long[
  scalar_dat_long$parameter %in% c("depletion", "SR_BH_steep", "SSB_MSY") &
  scalar_dat_long$E %in% c("E100", "E101"), ], 
  x = "D", y = "RE", re = FALSE,
  vert = "E", horiz = "parameter", print = FALSE)
print(p)
# see plot_scalar_points() for another plotting function

## ---- plot-sto-ts, fig.height=5, fig.width=7, fig.cap="Time series of relative error in spawning stock biomass."----
p <- plot_ts_lines(ts_dat_sto, y='SpawnBio_re', 
  vert = "E", horiz="D", print = FALSE, col = "max_grad")
print(p)

## ---- ssb-ts-plots, fig.height=5, fig.width=7, cache=TRUE, fig.cap="Spawning stock biomass time series."----
p <- plot_ts_lines(ts_dat_sto, y='SpawnBio_em', 
  vert = "E", horiz="D", print = FALSE, col = "max_grad")
print(p)

## ---- relative-error-boxplots-sto, fig.height=7, fig.width=5, cache=TRUE, fig.cap="Box plots of relative error (RE) for stochastic runs. *M* is fixed at the true value from the OM (E0) or estimated (E1). The standard deviation on the survey index observation error is 0.4 (D1) or 0.1 (D0), representing an increase in survey sampling effort."----
p <- plot_scalar_boxplot(scalar_dat_long[
  scalar_dat_long$parameter %in% c("depletion", "SR_BH_steep", "SSB_MSY") &
  scalar_dat_long$E %in% c("E0", "E1"), ], 
  x = "D", y = "RE", re = FALSE, 
  vert = "E", horiz = "parameter", print = FALSE)
print(p)

## ---- ss3sim-base-eg, eval=FALSE-----------------------------------------
#  d <- system.file("extdata", package = "ss3sim")
#  om <- paste0(d, "/models/cod-om")
#  em <- paste0(d, "/models/cod-em")
#  
#  F0 <- list(years = 1:100, years_alter = 1:100, fvals =
#    c(rep(0, 25), rep(0.114, 75)))
#  
#  index1 <- list(fleets = 2, years = list(seq(60, 100, by = 2)),
#    sds_obs = list(0.1))
#  
#  lcomp1 <- list(fleets = c(1, 2), Nsamp = list(100, 100), years =
#    list(25:100, seq(60, 100, by = 2)), lengthbin_vector = NULL,
#    cpar = c(1, 1))
#  
#  agecomp1 <- list(fleets = c(1, 2), Nsamp = list(100, 100), years =
#    list(25:100, seq(60, 100, by = 2)), agebin_vector = NULL,
#    cpar = c(1, 1))
#  
#  E0 <- list(natM_type = "1Parm", natM_n_breakpoints = NULL,
#    natM_lorenzen = NULL, natM_val = c(NA,-1), par_name =
#    "LnQ_base_3_CPUE", par_int = NA, par_phase = -1, forecast_num = 0)
#  
#  M0 <- list(NatM_p_1_Fem_GP_1 = rep(0, 100))
#  
#  # todo: add data_params so that this function runs.
#  ss3sim_base(iterations = 1:20, scenarios = "D1-E0-F0-M0-cod",
#    f_params = F0, index_params = index1, lcomp_params = lcomp1,
#    agecomp_params = agecomp1, estim_params = E0, tv_params = M0,
#    om_dir = om, em_dir = em)

## ---- alternative-case-lists, eval=FALSE---------------------------------
#  case_folder <- system.file("extdata", "eg-cases", package = "ss3sim")
#  om <- system.file("extdata", "models/cod-om", package = "ss3sim")
#  em <- system.file("extdata", "models/cod-em", package = "ss3sim")
#  files <- list.files(case_folder, pattern = "0-cod")
#  
#  temp_case_folder <- "ss3sim-example-cases"
#  dir.create(temp_case_folder)
#  file.copy(paste0(case_folder, "/", files), temp_case_folder)
#  
#  # now make X, Y, and Z case files:
#  setwd(temp_case_folder)
#  file.copy("index0-cod.txt", "X0-cod.txt")
#  file.copy("lcomp0-cod.txt", "Y0-cod.txt")
#  file.copy("agecomp0-cod.txt", "Z0-cod.txt")
#  setwd("..")
#  
#  # our custom specification:
#  case_files <- list(F = "F", X = "index", Y = "lcomp", Z = "agecomp")
#  
#  # and use run_ss3sim() with our new case_file list:
#  run_ss3sim(iterations = 1,
#    scenarios = "X0-Y0-Z0-F0-cod",
#    case_folder = temp_case_folder,
#    om_dir = om, em_dir = em,
#    case_files = case_files)

## ---- custom-case-eg, eval=FALSE-----------------------------------------
#  case_files = list(D = c("index", "lcomp", "agecomp"), F = "F", S = "S")

## ---- parallel-one, eval=FALSE-------------------------------------------
#  require(doParallel)
#  
#  registerDoParallel(cores = ifelse(Sys.getenv("NUMBER_OF_PROCESSORS")<6, 2, 4))

## ---- parallel-two, eval=FALSE-------------------------------------------
#  require(foreach)
#  getDoParWorkers()
#  
#  #> [1] 4

## ---- parallel-three, eval=FALSE-----------------------------------------
#  run_ss3sim(iterations = 1, scenarios =
#    c("D1-E0-F0-cod",
#      "D1-E1-F0-cod"),
#    case_files =
#      list(F = "F", D = c("index", "lcomp", "agecomp"), E = "E"),
#    case_folder = case_folder, om_dir = om, em_dir = em,
#    bias_adjust = TRUE, parallel = TRUE)

## ---- parallel-iterations1, eval=FALSE-----------------------------------
#  run_ss3sim(iterations = 1:2, scenarios = "D0-F0-cod",
#    case_folder = case_folder, om_dir = om, em_dir = em,
#    parallel = TRUE, parallel_iterations = TRUE)

## ---- parallel-iterations2, eval=FALSE-----------------------------------
#  run_ss3sim(iterations = 1:2, scenarios = "D0-F0-cod",
#    case_folder = case_folder, om_dir = om, em_dir = em,
#    parallel = TRUE, parallel_iterations = TRUE, bias_nsim = 2,
#    bias_adjust = TRUE)

