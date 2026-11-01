## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.align = "center",
  fig.width = 3.5,
  fig.height = 3,
  comment = "#>"
)

## ----setup, message = FALSE, warning = FALSE----------------------------------
library(geostan)
library(ggplot2)
library(gridExtra)
data("georgia")

## ----fig.width = 8------------------------------------------------------------
sp_diag(georgia$college, georgia, name = "College (%)")

## -----------------------------------------------------------------------------
W <- shape2mat(georgia, style = "W")

## -----------------------------------------------------------------------------
moran_plot(georgia$college, W)

## -----------------------------------------------------------------------------
mc(georgia$college, W)

## -----------------------------------------------------------------------------
A <- shape2mat(georgia, "B")
moran_plot(georgia$college, A)

## -----------------------------------------------------------------------------
x <- georgia$college
W <- shape2mat(georgia, "W")
mc(x, W)
gr(x, W)
mc(x, W) + gr(x, W)

## -----------------------------------------------------------------------------
W <- shape2mat(georgia, "W")
x <- log(georgia$income)
Ii <- lisa(x, W)
head(Ii)

## -----------------------------------------------------------------------------
Ci <- lg(x, W)
head(Ci)

## ----fig.width = 5------------------------------------------------------------
Ci_map <- ggplot(georgia) + 
  geom_sf(aes(fill=Ci)) +
  # or try: scale_fill_viridis() 
  scale_fill_gradient(high = "navy",  
                      low = "white") +
  theme_void()

Li_map <- ggplot(georgia) + 
  geom_sf(aes(fill=Ii$Li)) +
  scale_fill_gradient2(name = "Ii") +
  theme_void()

gridExtra::grid.arrange(Ci_map, Li_map, nrow = 1)

## -----------------------------------------------------------------------------
x <- log(georgia$income)
rho <- aple(x, W)
n <- nrow(georgia)
ess <- n_eff(rho = rho, n = n)
c(nominal_n = n, rho = rho, MC = mc(x, W), ESS = ess)

## -----------------------------------------------------------------------------
C <- shape2mat(georgia)
fit <- stan_glm(deaths.male ~ offset(log(pop.at.risk.male)), 
                data = georgia, 
                re = ~ GEOID, 
                family = poisson(), 
                C = C,
                refresh = 0 # this line silences Stan's printing
                )

## -----------------------------------------------------------------------------
print(fit)

## ----fig.width = 8------------------------------------------------------------
sp_diag(fit, georgia)

