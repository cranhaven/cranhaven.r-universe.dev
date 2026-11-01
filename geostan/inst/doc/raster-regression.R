## ----setup, message = FALSE, warning = FALSE, eval = TRUE---------------------
library(geostan)
library(sf)
set.seed(1127)

## ----eval = TRUE--------------------------------------------------------------
# creating a grid
row <- 40
col <- 30
c(N <- row * col)
sfc = st_sfc(st_polygon(list(rbind(c(0,0), c(col,0), c(col,row), c(0,0)))))
grid <- st_make_grid(sfc, cellsize = 1, square = TRUE)
grid <- st_as_sf(grid)

# create connectivity matrix
W <- shape2mat(grid, style = "W", method = "rook", quiet = TRUE)

# draw data from a spatial autoregressive model
set.seed(100)
grid$z <- sim_sar(rho = 0.8, w = W)
grid$y <- sim_sar(mu = -0.5 * grid$z, rho = .9, sigma = .3, w = W)

## ----fig.width = 3.25, fig.height = 3, fig.align = 'center'-------------------
plot(grid[ , 'y' ])

## ----eval = FALSE-------------------------------------------------------------
#  fit <- stan_sar(y ~ z, data = grid, C = W)

## ----eval = FALSE-------------------------------------------------------------
#  C <- shape2mat(grid, style = "B", queen = FALSE)
#  car_list <- prep_car_data(C, "WCAR")
#  fit <- stan_car(y ~ z, data = grid, car_parts = car_list)

## ----eval = TRUE--------------------------------------------------------------
# create connectivity matrix and its eigenvalues
sars <- prep_sar_data2(row = row, col = col, quiet = TRUE)

# if you want the matrix
W <- sars$W

# fit model
fit <- stan_sar(y ~ z, 
      data = grid,
      centerx = TRUE,
      sar_parts = sars,
      iter = 500,
      chains = 2, # for demo speed
      # cores = 4, # multi-core processing	  
      slim = TRUE
        )	
print(fit)

## ----eval = FALSE-------------------------------------------------------------
#  row = 100
#  col = 100
#  sar_list <- prep_sar_data2(row = row, col = col)
#  W <- sar_list$W
#  
#  z <- sim_sar(rho = .8, w = W, quick = TRUE)
#  y <- sim_sar(mu = -.5 * z, rho = .7, w = W, quick = TRUE)
#  dat <- cbind(y, z)

