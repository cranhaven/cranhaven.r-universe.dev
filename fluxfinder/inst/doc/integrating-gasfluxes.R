## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fluxfinder)

# Load the data
# Data from a LI-7810
f <- system.file("extdata/TG10-01087-curvature.data", package = "fluxfinder")
dat <- ffi_read_LI7810(f)

# Fit a linear flux and QA/QC it
flux <- ffi_compute_fluxes(dat, group_column = NULL, time_column = "TIMESTAMP", gas_column = "CO2")
print(flux$lin_flux.estimate)
print(flux$lin_r.squared)
print(flux$poly_r.squared)
print(flux$HM81_flux.estimate)

## ----qaqc-plot, fig.width=7---------------------------------------------------
library(ggplot2)
dat$SECONDS <- dat$SECONDS-min(dat$SECONDS)
ggplot(dat, aes(SECONDS, CO2)) + geom_point() + 
  # naive linear model
  geom_smooth(method = "lm", formula = 'y ~ x') +
  # HM1981 flux line 
  geom_abline(slope = flux$HM81_flux.estimate, intercept = min(dat$CO2), linetype = 2)

## -----------------------------------------------------------------------------
library(gasfluxes)

# Add some columns that gasfluxes expects
dat$ID <- "test"
dat$V <- 0.1
dat$A <- 0.16

gasfluxes(dat, .times = "SECONDS", .C = "CO2", plot = FALSE)

## -----------------------------------------------------------------------------
# Define a small wrapper function to put things into the format
# that gasfluxes expects
f <- function(time, conc) {
  d <- data.frame(time = time, C = conc, ID = 1, A = 1, V = 1)
  gasfluxes(d, plot = FALSE)
}

ffi_compute_fluxes(dat, NULL, "SECONDS", "CO2", fit_function = f)

