## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE)
Sys.setlocale("LC_ALL","en_GB")

## ----libraries, message = FALSE-----------------------------------------------
library(tidyverse)
library(sf)
library(telemac)

path_proj <- tempdir()

## ----mesh, fig.width = 6, fig.height = 6--------------------------------------
tin_obj <- tin(list(boundary = data.frame(x = c(0, 100, 100,    0),
                                          y = c(0,   0, 100,  100))),
               s = 2, a = 4, q = 30)
plot(tin_obj, pch = ".")

## ----geo, fig.width = 6, fig.height = 6---------------------------------------
geo_obj <- geo(tin_obj,
               dem = list(elevation = data.frame(x = tin_obj$points[,1],
                                                 y = tin_obj$points[,2],
                                                 z = rep(0, nrow(tin_obj$points))),
                          cn = list(unit = "-",
                                    values = data.frame(
                                      x = tin_obj$points[,1],
                                      y = tin_obj$points[,2]) %>%
                                      mutate(z = case_when(
                                        between(x, 0, 49.9) & between(y, 0, 49.9) ~ 80,
                                        between(x, 0, 49.9) & between(y, 50, 100) ~ 85,
                                        between(x, 50, 100) & between(y, 0, 49.9) ~ 90,
                                        between(x, 50, 100) & between(y, 50, 100) ~ 95,
                                      ))
                                    )),
               fname = "geo.slf", title = "Constant bathymetry (zero elevation) with CN values")
plot(geo_obj, s = 1)
plot(geo_obj, s = 1, v = "cn")

## ----cli----------------------------------------------------------------------
cli_obj <- cli(geo_obj, "bnd.cli")

## ----cas_rainfall-------------------------------------------------------------
cas_pars <- list(
  # files
  "BOUNDARY CONDITIONS FILE" = "bnd.cli",
  "GEOMETRY FILE" = "geo.slf",
  "RESULTS FILE" = "results.slf",
  # General
  # H: water depth (m);
  # U, V: velocity along in x, y direction (m/s);
  # S, B: free surface, bottom elevation (m)
  # N, O, R, Z: additional 'private' variables defined in user code
  "VARIABLES FOR GRAPHIC PRINTOUTS" = "H",
  "DURATION" = 3600*7, # seconds
  "TIME STEP" = 60, # seconds
  "GRAPHIC PRINTOUT PERIOD " = 60, # number of time steps
  "LISTING PRINTOUT PERIOD" = 60, # number of time steps
  # rainfall
  "RAIN OR EVAPORATION" = "YES",
  "RAIN OR EVAPORATION IN MM PER DAY" = 100,
  "DURATION OF RAIN OR EVAPORATION IN HOURS" = 6,
  # Numerics
  # 1: conjugate gradient, recommended when using wave equation,
  # time step should be adapted until convergence is reached after 20 to 30 iterations
  "SOLVER" = 1,
  "MAXIMUM NUMBER OF ITERATIONS FOR SOLVER" = 200,
  "MASS-BALANCE" = "YES",
  "TIDAL FLATS" = "YES",
  # 1: surface gradient corrected (recommended);
  # 2: areas masked from computation;
  # 3: like 1 with porosity term added to half-dry elements to change water quantity
  "OPTION FOR THE TREATMENT OF TIDAL FLATS" = 1,
  # 0: no treatment; 1: smoothing negative depths;
  # 2: flux limitation by segment ensuring positive depths;
  # 3: flux limitation by element
  "TREATMENT OF NEGATIVE DEPTHS" = 2,
  # required if TREATMENT OF NEGATIVE DEPTHS = 2 (not documented, see forum)
  "MASS-LUMPING ON H" = 1,
  # required if TREATMENT OF NEGATIVE DEPTHS = 2
  "CONTINUITY CORRECTION" = "YES",
  # SUPG OPTION=...;0;... required if TREATMENT OF NEGATIVE DEPTHS = 2
  "SUPG OPTION" = "0;0",
  # 1: coupled; 2: wave equation (more stable)
  "TREATMENT OF THE LINEAR SYSTEM" = 2,
  # recommended for steep topography and tidal flats
  "FREE SURFACE GRADIENT COMPATIBILITY" = 0.9,
  # Initial conditions
  "INITIAL CONDITIONS" = 'ZERO DEPTH',
  # Friction
  # 0: no friction, 2: Chezy, 3: Strickler, 4: Manning
  "LAW OF BOTTOM FRICTION" = 3,
  # concrete: 100; straight stream: 30-40; natural stream with wood: <10
  "FRICTION COEFFICIENT" = 50
)
cas_obj <- cas(cas_pars, fname = "rainfall.cas")

## ----t2d_rainfall-------------------------------------------------------------
t2d_obj <- t2d("Example: static rainfall without runoff",
               wdir = paste(path_proj, "rainfall", sep = "/"),
               cas = cas_obj, geo = geo_obj, cli = cli_obj)

## ----run_rainfall, eval = FALSE-----------------------------------------------
#  write_t2d(t2d_obj)
#  t2d_sim <- simulate_t2d(t2d_obj, exec = "telemac2d.py")

## ----add_result_rainfall, echo = FALSE----------------------------------------
t2d_sim <- t2d_obj
t2d_sim$res <- results(system.file("telemac/rainfall_runoff/res_rainfall.slf", package = "telemac"),
                       times = 7*3600)

## ----plot_rainfall------------------------------------------------------------
res_df <- tin2grid(t2d_sim$res, s = 2, output = "data.frame")
ggplot(res_df, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  coord_equal() +
  scale_y_continuous(expand = expansion(0,0)) +
  scale_x_continuous(expand = expansion(0,0)) +
  theme_bw()

## ----cn_file------------------------------------------------------------------
cn_dat <- rbind(
  matrix(c(   0, 49.9, 49.9,    0,    0,
              0,    0, 49.9, 49.9,    0,
            rep(80, 5)), ncol = 3),
  matrix(c(   0, 49.9, 49.9,    0,    0,
           50.1, 50.1,  100,  100, 50.1,
           rep(85, 5)), ncol = 3),
  matrix(c(50.1,  100,  100, 50.1, 50.1,
              0,    0, 49.9, 49.9,    0,
           rep(90, 5)), ncol = 3),
  matrix(c(50.1,  100,  100, 50.1, 50.1,
           50.1, 50.1,  100,  100, 50.1,
           rep(95, 5)), ncol = 3)
)
dir.create(paste(path_proj, "rainfall_runoff", sep = "/"), recursive = T)
write("#", paste(path_proj, "rainfall_runoff/cn.txt", sep = "/"), sep = "\n")
write("# x y cn", paste(path_proj, "rainfall_runoff/cn.txt", sep = "/"), append = T, sep = "\n")
write.table(cn_dat, paste(path_proj, "rainfall_runoff/cn.txt", sep = "/"),
            row.names = F, col.names = F, quote = F, sep = "\t", append = T)

## ----cas_rainfall_runoff------------------------------------------------------
# path might be too long, see note above
cas_obj[["FORTRAN FILE"]] <- system.file("telemac/rainfall_runoff/code_rainstat_cnfile",
                                         package = "telemac")
cas_obj[["FORMATTED DATA FILE 2"]] <- "cn.txt"
# 'acc. runoff' as defined in user code
cas_obj[["VARIABLES FOR GRAPHIC PRINTOUTS"]] <- "R"
# 1: on; 0: off
cas_obj[["RAINFALL-RUNOFF MODEL"]] <- 1
# 1: 0.2 (standard); 2: revised method, 0.05 with conversion of CN values
cas_obj[["OPTION FOR INITIAL ABSTRACTION RATIO"]] <- 1
# 1: dry, 2: normal, 3: wet
cas_obj[["ANTECEDENT MOISTURE CONDITIONS"]] <- 2

## ----t2d_rainfall_runoff------------------------------------------------------
t2d_obj <- t2d("Example: static rainfall with runoff, CN from file",
               wdir = paste(path_proj, "rainfall_runoff", sep = "/"),
               cas = cas_obj, geo = geo_obj, cli = cli_obj)

## ----run_rainfall_runoff, eval = FALSE----------------------------------------
#  write_t2d(t2d_obj)
#  t2d_sim <- simulate_t2d(t2d_obj, exec = "telemac2d.py")

## ----add_result_rainfall_runoff, echo = FALSE---------------------------------
t2d_sim <- t2d_obj
t2d_sim$res <- results(system.file("telemac/rainfall_runoff/res_rainfall_runoff.slf", package = "telemac"),
                       times = 3600 * c(0,1,4,7))

## ----plot_rainfall_runoff, fig.width = 12, fig.height = 10--------------------
res_df <- tin2grid(t2d_sim$res, s = 2, output = "data.frame")
ggplot(res_df, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  coord_equal() +
  scale_y_continuous(expand = expansion(0,0)) +
  scale_x_continuous(expand = expansion(0,0)) +
  facet_wrap(~ timestep) +
  theme_bw()

## ----dynamic_rainfall_input---------------------------------------------------
dir.create(paste(path_proj, "rainfall_runoff_dyn", sep = "/"), recursive = T)
write("#", paste(path_proj, "rainfall_runoff_dyn/rain.txt", sep = "/"), sep = "\n")
write("# time (s) rainfall (mm)", paste(path_proj, "rainfall_runoff_dyn/rain.txt", sep = "/"), append = T, sep = "\n")
rain <- data.frame(t = c(0, 3600, 3*3600, 4*3600, 6*3600, 8*3600),
                   r = c(0,   10,      5,      7,      3,      0))
write.table(rain, paste(path_proj, "rainfall_runoff_dyn/rain.txt", sep = "/"),
            row.names = F, col.names = F, quote = F, sep = "\t", append = T)

## ----cas_rainfall_runoff_dyn--------------------------------------------------
# path might be too long, see note above
cas_obj[["FORTRAN FILE"]] <- system.file("telemac/rainfall_runoff/code_raindyn_cngeo",
                                         package = "telemac")
cas_obj[["FORMATTED DATA FILE 1"]] <- "rain.txt"
cas_obj[["FORMATTED DATA FILE 2"]] <- NULL # not needed here
# as given in geometry file and used in user code
cas_obj[["NAMES OF PRIVATE VARIABLES"]] <- "CN"

## ----t2d_rainfall_runoff_dyn--------------------------------------------------
t2d_obj <- t2d("Example: dynamic rainfall with runoff, CN from geo",
               wdir = paste(path_proj, "rainfall_runoff_dyn", sep = "/"),
               cas = cas_obj, geo = geo_obj, cli = cli_obj)

## ----run_rainfall_runoff_dyn, eval = FALSE------------------------------------
#  write_t2d(t2d_obj)
#  t2d_sim <- simulate_t2d(t2d_obj, exec = "telemac2d.py")

## ----add_result_rainfall_runoff_dyn, echo = FALSE-----------------------------
t2d_sim <- t2d_obj
t2d_sim$res <- results(system.file("telemac/rainfall_runoff/res_rainfall_runoff_dyn.slf", package = "telemac"),
                       times = 3600 * c(0,1,4,7))

## ----plot_rainfall_runoff_dyn, fig.width = 12, fig.height = 10----------------
res_df <- tin2grid(t2d_sim$res, s = 2, output = "data.frame")
ggplot(res_df, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  coord_equal() +
  scale_y_continuous(expand = expansion(0,0)) +
  scale_x_continuous(expand = expansion(0,0)) +
  facet_wrap(~ timestep) +
  theme_bw()

