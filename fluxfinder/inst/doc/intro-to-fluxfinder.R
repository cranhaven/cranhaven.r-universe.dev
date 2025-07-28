## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fluxfinder)

# Data from a LI-7810
f <- system.file("extdata/TG10-01087.data", package = "fluxfinder")
dat <- ffi_read_LI7810(f)

# Note that the fluxfinder read functions print some info after reading
# Set "options(fluxfinder.quiet = TRUE)" to suppress such messages

# Look at a subset of the data; the full data frame has 500+ rows and 25 columns
dat[1:6, 1:9]

## ----overview-plot, fig.width=7-----------------------------------------------
library(ggplot2)
ggplot(dat, aes(TIMESTAMP, CO2)) + geom_point()

## ----metadata-----------------------------------------------------------------
# Accompanying metadata
md <- system.file("extdata/TG10-01087-metadata.csv", package = "fluxfinder")
metadat <- read.csv(md)

print(metadat)

## ----matching, fig.width=7----------------------------------------------------
dat$metadat_row <- ffi_metadata_match(
  data_timestamps = dat$TIMESTAMP,
  start_dates = metadat$Date,
  start_times = metadat$Start_time,
  obs_lengths = metadat$Obs_length + 10) # 10 is expected dead band length

# Note that ffi_metadata_match() warns us that one metadata row didn't match any data

# Based on the row match information, add a "Plot" column to the data
dat$Plot <- metadat$Plot[dat$metadat_row]
metadat$metadat_row <- seq_len(nrow(metadat))

# ...and plot
p <- ggplot(dat, aes(TIMESTAMP, CO2, color = Plot)) + geom_point()
print(p)

## ----emphasize-problems, fig.width=7, echo=FALSE, message=FALSE---------------
library(lubridate)
p + annotate("rect", ymin = 455, ymax = 476, 
             xmin = ymd_hms("2022-10-27 10:39:30", tz = "EST"), 
             xmax = ymd_hms("2022-10-27 10:43:30", tz = "EST"), 
             color = "red", fill = NA, linewidth = 1.5)

## ----matching2, fig.width=7---------------------------------------------------
metadat$Obs_length[3:5] <- c(30, 45, 45)
dat$metadat_row <- ffi_metadata_match(
  data_timestamps = dat$TIMESTAMP,
  start_dates = metadat$Date,
  start_times = metadat$Start_time,
  obs_lengths = metadat$Obs_length + 10)
dat$Plot <- metadat$Plot[dat$metadat_row]

p %+% dat

## ----units--------------------------------------------------------------------
dat$CO2_umol <- ffi_ppm_to_umol(dat$CO2, 
                                volume = 0.1, # m3
                                temp = 24)    # degrees C

# See the message: because we didn't provide the 'atm' parameter, 
# ffi_ppm_to_umol assumed standard pressure.

# Also normalize by ground area (0.16 m2 in this example)
dat$CO2_umol_m2 <- dat$CO2_umol / 0.16

## -----------------------------------------------------------------------------
# Let's say volume varies by measurement; this can happen if the chamber
# height changes depending on the ground vegetation in each plot
metadat$Volume <- c(0.1, 0.2, 0.1, 0.1, 0.3, 0.1, 0.1)

# Merge the data and metadata
dat_changing_vol <- merge(dat, metadat[c("Plot", "Volume")], by = "Plot", all.x = TRUE)

# Unit conversion as above, but using the changing volume information:
dat_changing_vol$CO2_umol <- ffi_ppm_to_umol(dat_changing_vol$CO2, 
                                             volume = dat_changing_vol$Volume,
                                             temp = 24)
# We still have constant ground area in this example
dat_changing_vol$CO2_umol_m2 <- dat_changing_vol$CO2_umol / 0.16

# Relative to the previous constant-volume example, our area-normalized
# amounts (µmol) have now increased for plots B and E because
# of their larger volumes:
aggregate(CO2_umol_m2 ~ Plot, data = dat, FUN = mean)
aggregate(CO2_umol_m2 ~ Plot, data = dat_changing_vol, FUN = mean)

## ----compute-fluxes-----------------------------------------------------------
fluxes <- ffi_compute_fluxes(dat,
                             group_column = "Plot", 
                             time_column = "TIMESTAMP", 
                             gas_column = "CO2_umol_m2",
                             dead_band = 10) 
#Here we use a constant dead band value, but this can also vary by group; see the documentation


# By default, ffi_compute_fluxes returns a data.frame with one row per
# grouping variable value (i.e., per measurement). The first column is the
# group label; the second is the average value of the `time_column`;
# and the rest of the columns are fit statistics for a linear fit of
# concentration as a function of time, along with information about polynomial
# and robust-linear fits. See ?ffi_compute_fluxes for more details.
names(fluxes)

# For clarity, print out only a subset of the columns 
fluxes[c("Plot", "TIMESTAMP", "lin_r.squared", "lin_flux.estimate", "HM81_flux.estimate")]

## ----plot-fluxes, fig.width=7-------------------------------------------------
ggplot(fluxes, aes(Plot, lin_flux.estimate, color = lin_r.squared)) +
  geom_point() +
  geom_linerange(aes(ymin = lin_flux.estimate - lin_flux.std.error,
                     ymax = lin_flux.estimate + lin_flux.std.error)) +
  ylab("CO2 flux (µmol/m2/s)")

## ----figures-side, fig.show="hold", out.width="46%"---------------------------
ggplot(fluxes, aes(lin_flux.estimate, rob_flux.estimate, color = Plot)) +
  geom_point() + geom_abline() + theme(legend.position = "none")
ggplot(fluxes, aes(lin_r.squared, poly_r.squared, color = Plot)) +
  geom_point() + geom_abline() + theme(legend.position="none")

## ----fig.width=7--------------------------------------------------------------
ggplot(Puromycin, aes(conc, rate)) + geom_point() + geom_smooth(method = "lm")

## -----------------------------------------------------------------------------
ffi_compute_fluxes(Puromycin,
                   group_column = NULL,
                   time_column = "conc",
                   gas_column = "rate")

## ----include=FALSE------------------------------------------------------------
x <- ffi_compute_fluxes(Puromycin,
                        group_column = NULL,
                        time_column = "conc",
                        gas_column = "rate")
x <- round(x, 3)

