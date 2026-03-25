## ----install package, eval = F------------------------------------------------
#  install.packages('devtools')
#  library(devtools)
#  install_github('MoBiodiv/mobr')

## ----load pkg and data, message=FALSE-----------------------------------------
library(mobr)
library(dplyr)
library(ggplot2)
data(inv_comm)      # Community matrix
data(inv_plot_attr) # Plot attributes data.frame

## ----examine data-------------------------------------------------------------
str(inv_comm)
head(inv_plot_attr)

## ----make mob_in--------------------------------------------------------------
inv_mob_in <- make_mob_in(inv_comm, inv_plot_attr, coord_names = c('x', 'y'))
inv_mob_in

## ----fig.width = 5, fig.height = 5--------------------------------------------
plot_rarefaction(inv_mob_in, 'group', ref_level = 'uninvaded', 'sSBR', lwd = 4)

## ----fig.width = 7, fig.height=4----------------------------------------------
oldpar <- par(no.readonly = TRUE)       
par(mfrow = c(1, 2))
plot_rarefaction(inv_mob_in, 'group', 'uninvaded', 'IBR', 
                 leg_loc = 'bottomright')
par(oldpar)

## ----fig.width = 7, fig.height=4----------------------------------------------
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))
plot_abu(inv_mob_in, 'group', type = 'rad', scale = 'alpha', log = 'x')
plot_abu(inv_mob_in, 'group', type = 'rad', scale = 'gamma' , log = 'x')
par(oldpar)

## ----compute diversity indices, eval = TRUE-----------------------------------
indices <- c('N', 'S', 'S_n', 'S_PIE')
inv_div <- tibble(inv_comm) %>% 
  group_by(group = inv_plot_attr$group) %>% 
  group_modify(~ calc_comm_div(.x, index = indices, effort = 5,
                               extrapolate = TRUE))

## ----echo = FALSE-------------------------------------------------------------
#load('../vignettes/inv_stats.Rdata')

## -----------------------------------------------------------------------------
head(inv_div)

## ----fig.width = 7, fig.height = 3.5------------------------------------------
plot_comm_div(inv_div, 'S')

## ----beta demo----------------------------------------------------------------
calc_beta_div(inv_comm, c('S', 'S_n', 'S_PIE'), effort = 5)

## ----fig.width = 7, fig.height = 3.5------------------------------------------
plot_comm_div(inv_div, 'N')

## ----fig.width = 7, fig.height = 3.5------------------------------------------
plot_comm_div(inv_div, 'S_n')

## ----fig.width = 7, fig.height = 3.5------------------------------------------
plot_comm_div(inv_div, 'S_PIE')

## ----eval=FALSE---------------------------------------------------------------
#  plot_comm_div(inv_div)

## ----multi-scale analysis, eval=FALSE-----------------------------------------
#  inv_mob_in <- make_mob_in(inv_comm, inv_plot_attr,
#                            coord_names = c('x', 'y'))
#  inv_deltaS <- get_delta_stats(inv_mob_in, 'group', ref_level='uninvaded',
#                               type='discrete', log_scale=TRUE, n_perm = 199)

## ----echo=FALSE---------------------------------------------------------------
load('../vignettes/inv_deltaS.Rdata')

## ----fig.width=7, fig.height=3.5----------------------------------------------
plot(inv_deltaS, stat = 'b1', scale_by = 'indiv', display='S ~ effort')

## ----fig.width=7, fig.height=3.5----------------------------------------------
plot(inv_deltaS, stat = 'b1', scale_by = 'indiv', display='stat ~ effort')

## ----load fire data-----------------------------------------------------------
# plant community in response to a prescribed fire treatment in a
# central US woodland
data(fire_comm)
data(fire_plot_attr)
fire_mob_in <- make_mob_in(fire_comm, fire_plot_attr,
                          coord_names = c('x', 'y'))

## -----------------------------------------------------------------------------
par(mfrow=c(1,3))
plot_rarefaction(fire_mob_in, 'group', ref_level = 'unburned', 'IBR', 
                 leg_loc = NA)
plot_rarefaction(fire_mob_in, 'group', ref_level = 'unburned', 'sSBR',
                 leg_loc = 'bottomright')

## -----------------------------------------------------------------------------
oldpar <- par(no.readonly = TRUE) 
par(mfrow = c(1, 2))
plot_abu(fire_mob_in, 'group', ref_level = 'unburned', 'rad', leg_loc = 'topright',)
plot_abu(fire_mob_in, 'group', ref_level = 'unburned', 'sad', leg_loc = NA)
par(oldpar)

## -----------------------------------------------------------------------------
indices <- c('N', 'S', 'S_C', 'S_n', 'S_PIE')
fire_div <- tibble(fire_comm) %>% 
  group_by(group = fire_plot_attr$group) %>% 
  group_modify(~ calc_comm_div(.x, index = indices, effort = 5,
                               extrapolate = TRUE))

## ----fig.width = 7, fig.height = 3.5------------------------------------------
plot_comm_div(fire_div)

## ----eval = FALSE-------------------------------------------------------------
#  fire_deltaS <- get_delta_stats(fire_mob_in, 'group', ref_level = 'unburned',
#                                type = 'discrete', log_scale = TRUE, n_perm = 199,
#                                overall_p = TRUE)

## ----echo = FALSE-------------------------------------------------------------
load('../vignettes/fire_deltaS.Rdata')

## ----fig.width = 7, fig.height = 6--------------------------------------------
plot(fire_deltaS, stat = 'b1', scale_by = 'indiv')

## -----------------------------------------------------------------------------
data(tank_comm)
data(tank_plot_attr)
tank_mob_in <- make_mob_in(tank_comm, tank_plot_attr,
                          coord_names = c('x', 'y'))

## ----fig.width = 5, fig.height = 5--------------------------------------------
plot_rarefaction(tank_mob_in, 'group', ref_level = 'low', 'sSBR')

## ----fig.width = 7, fig.height=4----------------------------------------------
oldpar <- par(no.readonly = TRUE) 
par(mfrow = c(1, 2))
plot_abu(tank_mob_in, 'group', ref_level = 'low', 'rad')
plot_abu(tank_mob_in, 'group', ref_level = 'low', 'sad', leg_loc = NA)
par(oldpar)

## -----------------------------------------------------------------------------
indices <- c('N', 'S', 'S_C', 'S_n', 'S_PIE')
tank_div <- tibble(tank_comm) %>% 
  group_by(group = tank_plot_attr$group) %>% 
  group_modify(~ calc_comm_div(.x, index = indices, effort = 5,
                               extrapolate = TRUE))

## ----fig.width = 7, fig.height = 3.5------------------------------------------
plot_comm_div(tank_div)

## ----eval = FALSE-------------------------------------------------------------
#  tank_deltaS <- get_delta_stats(tank_mob_in, 'group', ref_level = 'low',
#                                inds = 10, log_scale = TRUE, type = 'discrete',
#                                n_perm=199)

## ----echo = FALSE-------------------------------------------------------------
load('../vignettes/tank_deltaS.Rdata')

## ----fig.width = 7, fig.height = 6--------------------------------------------
plot(tank_deltaS, stat = 'b1')

