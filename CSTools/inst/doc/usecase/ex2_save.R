#*******************************************************************************
# Title: Example script to save 's2dv_cube' to NetCDF using CST_SaveExp
# Author: Eva Rifà Rovira 
# Date: 29/11/2024
#*******************************************************************************
# In this script, we'll see multiple ways to store the 's2dv_cube' (CST_SaveExp) 
# or the multidimensional array (SaveExp) to NetCDF. 

# Needed packages:
library(CSTools)
library(CSIndicators)
library(s2dv)
library(startR)

################################################################################
#-----------------------------------------------------
# Example 1: Multidimensional array and Dates, without metadata and coordinates
#-----------------------------------------------------
# (1.1) Minimal use case, without Dates
data <- array(1:5, dim = c(sdate = 5, lon = 4, lat = 4))
SaveExp(data, ftime_dim = NULL, memb_dim = NULL, dat_dim = NULL,
        var_dim = NULL, single_file = TRUE)
SaveExp(data, ftime_dim = NULL, memb_dim = NULL, dat_dim = NULL,
        var_dim = NULL, sdate_dim = NULL, single_file = FALSE) # same result

# (1.2) Forecast time dimension, without Dates
data <- array(1:5, dim = c(ftime = 5, lon = 4, lat = 4))
SaveExp(data, ftime_dim = 'ftime', memb_dim = NULL, dat_dim = NULL,
        var_dim = NULL, sdate_dim = NULL, single_file = TRUE)

# (1.3) Start date dimension, without Dates
data <- array(1:5, dim = c(sdate = 5, lon = 4, lat = 4))
SaveExp(data, ftime_dim = NULL, memb_dim = NULL, dat_dim = NULL,
        var_dim = NULL, sdate_dim = 'sdate', single_file = TRUE)

# (1.4) Only forecast time dimension (no sdate), with Dates
data <- array(1:5, dim = c(ftime = 5, lon = 4, lat = 4))
dates <- c('20000101', '20010102', '20020103', '20030104', '20040105')
dates <- as.Date(dates, format = "%Y%m%d", tz = "UTC")
dim(dates) <- c(ftime = 5)
SaveExp(data, ftime_dim = 'ftime', memb_dim = NULL, dat_dim = NULL,
        var_dim = NULL, sdate_dim = NULL, Dates = dates, single_file = FALSE)
SaveExp(data, ftime_dim = 'ftime', memb_dim = NULL, dat_dim = NULL,
        var_dim = NULL, sdate_dim = NULL, Dates = dates, single_file = TRUE)
# For this case we have the same result using: single_file = FALSE /TRUE.

# (1.5) Forecast time and 1 sdate, with Dates
data <- array(1:5, dim = c(sdate = 1, ftime = 5, lon = 4, lat = 4))
dates <- c('20000101', '20010102', '20020103', '20030104', '20040105')
dates <- as.Date(dates, format = "%Y%m%d", tz = "UTC")
dim(dates) <- c(sdate = 1, ftime = 5)
SaveExp(data, ftime_dim = 'ftime', memb_dim = NULL, dat_dim = NULL,
        var_dim = NULL, sdate_dim = 'sdate', Dates = dates, single_file = FALSE)
SaveExp(data, ftime_dim = 'ftime', memb_dim = NULL, dat_dim = NULL,
        var_dim = NULL, sdate_dim = 'sdate', Dates = dates, single_file = TRUE)

# (1.6) Test global attributes
SaveExp(data, ftime_dim = 'ftime', memb_dim = NULL, dat_dim = NULL,
        var_dim = NULL, sdate_dim = 'sdate', Dates = dates, single_file = TRUE, 
        extra_string = 'test', 
        global_attrs = list(system = 'tes1', reference = 'test2'))
# (1.7) Test global attributes
SaveExp(data, ftime_dim = 'ftime', memb_dim = NULL, dat_dim = NULL,
        var_dim = NULL, sdate_dim = 'sdate', Dates = dates, single_file = FALSE, 
        extra_string = 'test', 
        global_attrs = list(system = 'tes1', reference = 'test2'))
#-----------------------------------------------------
# Example 2: Test sample data from Start and from Load
#-----------------------------------------------------

# (2.1) Test SaveExp
exp <- CSTools::lonlat_prec_st
data <- exp$data
Dates = exp$attrs$Dates
coords = exp$coords
varname = exp$attrs$Variable$varName
metadata = exp$attrs$Variable$metadata
SaveExp(data = data, Dates = Dates, coords = coords, varname = varname, 
        metadata = metadata, ftime_dim = 'ftime', startdates = 1:4, 
        var_dim = 'var', memb_dim = 'member', dat_dim = 'dataset', 
        sdate_dim = 'sdate', single_file = FALSE)
SaveExp(data = data, Dates = Dates, coords = coords, varname = varname, 
        metadata = metadata, ftime_dim = 'ftime', startdates = 1:4, 
        var_dim = 'var', memb_dim = 'member', dat_dim = 'dataset', 
        sdate_dim = 'sdate', single_file = TRUE)

# (2.2) lonlat_temp_st$exp in a single file with units 'hours since'
# (2.2.1) We save the data
data <- lonlat_temp_st$exp
CST_SaveExp(data = data, ftime_dim = 'ftime', 
            var_dim = 'var', dat_dim = 'dataset', sdate_dim = 'sdate', 
            units_hours_since = TRUE, single_file = TRUE)

# (2.2.2) Now we read the output with Start:
sdate <- as.vector(lonlat_temp_st$exp$coords$sdate)
path <- paste0(getwd(),"/$var$_", sdate[1], "_", sdate[length(sdate)], ".nc")
out <- Start(dat = path, 
             var = 'tas', 
             member = 'all',
             sdate = 'all', 
             ftime = 'all', 
             lat = 'all',
             lon = 'all', 
             return_vars = list(lon = 'dat',
                                lat = 'dat',
                                ftime = NULL,
                                sdate = NULL),
             retrieve = TRUE)

attributes(out)$Variables$common$ftime
out_cube <- as.s2dv_cube(out)
out_cube <- CST_ChangeDimNames(out_cube,
                               original_names = c("dat"),
                               new_names = c("dataset"))
all.equal(data$data, out_cube$data)
identical(data$data, out_cube$data)

# Plot the results and compare
PlotEquiMap(out_cube$data[,,1,1,1,,], lon = out_cube$coords$lon, 
            lat = out_cube$coords$lat, filled.continents = FALSE)

PlotEquiMap(lonlat_temp_st$exp$data[,,1,1,1,,], lon = out_cube$coords$lon, 
            lat = out_cube$coords$lat, filled.continents = FALSE)

# (2.3) lonlat_temp_st$exp in a single file with units of time frequency
# (2.3.1) we save the data
data <- lonlat_temp_st$exp
CST_SaveExp(data = data, ftime_dim = 'ftime', 
            var_dim = 'var', dat_dim = 'dataset', sdate_dim = 'sdate', 
            single_file = TRUE, units_hours_since = FALSE)
dates <- lonlat_temp_st$exp$attrs$Dates
# (2.3.2) Now we read the output with Start:
sdate <- as.vector(lonlat_temp_st$exp$coords$sdate)
path <- paste0(getwd(),"/$var$_", sdate[1], "_", sdate[length(sdate)], ".nc")
out <- Start(dat = path,
             var = 'tas',
             lon = 'all',
             lat = 'all',
             ftime = 'all',
             sdate = 'all',
             member = 'all',
             return_vars = list(
                lon = 'dat',
                lat = 'dat',
                ftime = NULL,
                sdate = NULL),
             retrieve = TRUE)

attributes(out)$Variables$common$ftime
# [1] "1 months" "2 months" "3 months"
out_cube2 <- as.s2dv_cube(out)

# (2.4) lonlat_temp_st$exp in separated files with units of hours since
# (2.4.1) we save the data
data <- lonlat_temp_st$exp
CST_SaveExp(data = data, ftime_dim = 'ftime', 
            var_dim = 'var', dat_dim = 'dataset', sdate_dim = 'sdate', 
            single_file = FALSE)
# (2.4.2) we load the data
sdate <- as.vector(lonlat_temp_st$exp$coords$sdate)
path <- paste0(getwd(),"/dat1/$var$/$var$_$sdate$.nc")

out <- Start(dat = path, var = 'tas',
             sdate = sdate,
             lon = 'all',
             lat = 'all',
             ftime = 'all',
             member = 'all',
             return_vars = list(lon = 'dat',
                                lat = 'dat',
                                ftime = 'sdate'),
             retrieve = TRUE)
out_cube1 <- as.s2dv_cube(out)
# (2.5) lonlat_prec_st$exp in a single file with units of time frequency
# (2.5.1) we save the data
data <- lonlat_prec_st
CST_SaveExp(data = data, ftime_dim = 'ftime', 
            var_dim = 'var', dat_dim = 'dataset', sdate_dim = 'sdate', 
            single_file = TRUE, units_hours_since = FALSE)

# (2.5.2) we read the data
sdate <- as.vector(data$coords$sdate)
path <- paste0(getwd(),"/$var$_", sdate[1], "_", sdate[length(sdate)], ".nc")
out <- Start(dat = path,
             var = 'prlr',
             lon = 'all',
             lat = 'all',
             ftime = 'all',
             sdate = 'all',
             member = 'all',
             return_vars = list(
                lon = 'dat',
                lat = 'dat',
                ftime = NULL,
                sdate = NULL),
             retrieve = TRUE)

attributes(out)$Variables$common$ftime
#  [1] "1 days"  "2 days"  "3 days"  "4 days"  "5 days"  "6 days"  "7 days" 
#  [8] "8 days"  "9 days"  "10 days" "11 days" "12 days" "13 days" "14 days"
# [15] "15 days" "16 days" "17 days" "18 days" "19 days" "20 days" "21 days"
# [22] "22 days" "23 days" "24 days" "25 days" "26 days" "27 days" "28 days"
# [29] "29 days" "30 days" "31 days"
out_cube <- as.s2dv_cube(out)

# (2.6) Test observations: lonlat_temp
# (2.6.1) Save the data
data <- lonlat_temp$obs
CST_SaveExp(data = data, ftime_dim = 'ftime', memb_dim = NULL, 
            var_dim = NULL, dat_dim = 'dataset', sdate_dim = 'sdate', 
            single_file = TRUE, units_hours_since = FALSE)
# (2.6.2) Now we read the output with Start:
sdate <- c('20001101', '20051101')
path <- paste0(getwd(),"/$var$_", sdate[1], "_", sdate[length(sdate)], ".nc")
out <- Start(dat = path,
             var = 'tas', # tas
             lon = 'all',
             lat = 'all',
             ftime = 'all',
             member = 1,
             sdate = 'all',
             return_vars = list(
               lon = 'dat',
               lat = 'dat',
               ftime = NULL,
               sdate = NULL),
             retrieve = TRUE)
dim(out)
attributes(out)$Variables$common$ftime

# (2.7) Test lonlat_prec
# (2.7.1) Save the data
data <- lonlat_prec
CST_SaveExp(data = data, ftime_dim = 'ftime', memb_dim = NULL, 
            var_dim = NULL, dat_dim = 'dataset', sdate_dim = 'sdate', 
            single_file = TRUE, units_hours_since = FALSE)
# (2.7.2) Now we read the output with Start:
sdate <- as.vector(data$coords$sdate)
path <- paste0(getwd(),"/$var$_", sdate[1], "_", sdate[length(sdate)], ".nc")
out <- Start(dat = path,
             var = 'prlr', # tas
             lon = 'all',
             lat = 'all',
             ftime = 'all',
             sdate = 'all',
             member = 'all',
             return_vars = list(
                lon = 'dat',
                lat = 'dat',
                ftime = NULL,
                sdate = NULL),
             retrieve = TRUE)
dim(out)
lonlat_prec$dims

# (2.8) Test with ftime_dim NULL
data <- lonlat_temp$exp
data <- CST_Subset(data, along = 'ftime', indices = 1, drop = 'selected')

CST_SaveExp(data = data, ftime_dim = NULL, 
            var_dim = NULL, dat_dim = 'dataset', sdate_dim = 'sdate', 
            single_file = FALSE, units_hours_since = FALSE)

#-----------------------------------------------------
# Example 3: Special cases
#-----------------------------------------------------

# (3.1) Two variables and two datasets in separated files
# (3.1.1) We load the data from Start
repos <- "/esarchive/exp/ecmwf/system5_m1/monthly_mean/$var$_f6h/$var$_$sdate$.nc"
repos2 <- "/esarchive/exp/ecmwf/system4_m1/monthly_mean/$var$_f6h/$var$_$sdate$.nc"

data3 <- Start(dat = list(list(name = 'system4_m1', path = repos2),
                          list(name = 'system5_m1', path = repos)),
               var = c('tas', 'sfcWind'),
               sdate = c('20160101', '20170101'),
               ensemble = indices(1),
               time = indices(1:2),
               lat = indices(1:10),
               lon = indices(1:10),
               synonims = list(lat = c('lat', 'latitude'),
                               lon = c('lon', 'longitude')),
               return_vars =  list(time = 'sdate',
                                   longitude = 'dat',
                                   latitude = 'dat'),
               metadata_dims = c('dat', 'var'),
               retrieve = T)
cube3 <- as.s2dv_cube(data3)

# (3.1.2) We save the data
CST_SaveExp(data = cube3, ftime_dim = 'time', var_dim = 'var', 
             memb_dim = 'ensemble', dat_dim = 'dat')

# (3.1.3) We read again the data with start
repos <- paste0(getwd(), "/system4_m1/$var$/$var$_$sdate$.nc")
repos2 <- paste0(getwd(), "/system5_m1/$var$/$var$_$sdate$.nc")

data3out <- Start(dat = list(list(name = 'system4_m1', path = repos2),
                          list(name = 'system5_m1', path = repos)),
              var = c('tas', 'sfcWind'),
              sdate = c('20160101', '20170101'),
              ensemble = indices(1),
              time = indices(1:2),
              lat = indices(1:10),
              lon = indices(1:10),
              synonims = list(lat = c('lat', 'latitude'),
                              lon = c('lon', 'longitude')),
              return_vars =  list(time = 'sdate',
                                  longitude = 'dat',
                                  latitude = 'dat'),
              metadata_dims = c('dat', 'var'),
              retrieve = T)

summary(data3out)
summary(data3)

dim(data3)
dim(data3out)

# (3.2) Two variables and two datasets in the same file

CST_SaveExp(data = cube3, ftime_dim = 'time', var_dim = 'var', 
            memb_dim = 'ensemble', dat_dim = 'dat', 
            single_file = TRUE)
# TODO: Read the output with Start

# (3.3) Observations (from startR usecase)
repos_exp <- paste0('/esarchive/exp/ecearth/a1tr/cmorfiles/CMIP/EC-Earth-Consortium/',
                    'EC-Earth3/historical/r24i1p1f1/Amon/$var$/gr/v20190312/',
                    '$var$_Amon_EC-Earth3_historical_r24i1p1f1_gr_$sdate$01-$sdate$12.nc')

exp <- Start(dat = repos_exp,
             var = 'tas',
             sdate = as.character(c(2005:2008)),
             time = indices(1:3),
             lat = 1:10,
             lat_reorder = Sort(),
             lon = 1:10,
             lon_reorder = CircularSort(0, 360),
             synonims = list(lat = c('lat', 'latitude'),
                             lon = c('lon', 'longitude')),
             return_vars = list(lon = NULL,
                                lat = NULL,
                                time = 'sdate'),
             retrieve = FALSE)
dates <- attr(exp, 'Variables')$common$time
repos_obs <- '/esarchive/recon/ecmwf/erainterim/monthly_mean/$var$_f6h/$var$_$date$.nc'

obs <- Start(dat = repos_obs,
             var = 'tas',
             date = unique(format(dates, '%Y%m')),
             time = values(dates),  #dim: [sdate = 4, time = 3]
             lat = 1:10,
             lat_reorder = Sort(),
             lon = 1:10,
             lon_reorder = CircularSort(0, 360),
             time_across = 'date',
             merge_across_dims = TRUE,
             split_multiselected_dims = TRUE,
             synonims = list(lat = c('lat', 'latitude'),
                             lon = c('lon', 'longitude')),
             return_vars = list(lon = NULL,
                                lat = NULL,
                                time = 'date'),
             retrieve = TRUE)
obscube <- as.s2dv_cube(obs)
CST_SaveExp(data = obscube, ftime_dim = 'time', var_dim = 'var', 
            memb_dim = NULL, dat_dim = 'dat', 
            single_file = TRUE, extra_string = 'obs_tas')
CST_SaveExp(data = obscube, ftime_dim = 'time', var_dim = 'var', 
            memb_dim = NULL, dat_dim = 'dat', 
            single_file = FALSE, extra_string = 'obs_tas')

#-----------------------------------------------------
# Example 4: Time bounds:
#-----------------------------------------------------

# example: /esarchive/exp/ncep/cfs-v2/weekly_mean/s2s/tas_f24h/tas_20231128.nc
library(CSIndicators)
exp <- CSTools::lonlat_prec_st
exp$attrs$Dates <- Reorder(exp$attrs$Dates, c(2,1))
res <- CST_PeriodAccumulation(data = exp, time_dim = 'ftime',
                              start = list(10, 03), end = list(20, 03))
# > dim(res$attrs$Dates)
# sdate 
#     3 
# (4.1) All data in a single file
CST_SaveExp(data = res, ftime_dim = NULL, var_dim = 'var', 
            memb_dim = 'member', dat_dim = 'dataset', 
            startdates = res$attrs$Dates, single_file = TRUE)
# (4.1.1) Same with SaveExp          
SaveExp(data = res$data, coords = res$coords, 
        Dates = NULL, time_bounds = res$attrs$time_bounds,
        ftime_dim = NULL, var_dim = 'var', 
        varname = res$attrs$Variable$varName,
        metadata = res$attrs$Variable$metadata, 
        memb_dim = 'member', dat_dim = 'dataset', 
        startdates = res$attrs$Dates, single_file = TRUE)
# (4.2) All data in separated files
CST_SaveExp(data = res, ftime_dim = NULL, var_dim = 'var', 
            memb_dim = 'member', dat_dim = 'dataset', 
            startdates = res$attrs$Dates, single_file = FALSE)
# (4.2.1) Same with SaveExp  
SaveExp(data = res$data, coords = res$coords, 
        Dates = res$attrs$Dates, time_bounds = res$attrs$time_bounds,
        ftime_dim = NULL, var_dim = 'var', 
        varname = res$attrs$Variable$varName,
        metadata = res$attrs$Variable$metadata, 
        memb_dim = 'member', dat_dim = 'dataset', 
        startdates = res$attrs$Dates, single_file = FALSE)
# (4.3)
CST_SaveExp(data = res, ftime_dim = NULL, var_dim = 'var', 
            memb_dim = 'member', dat_dim = 'dataset', 
            startdates = 1:4, single_file = FALSE)

# (4.4) We change the time dimensions to ftime and sdate_dim = NULL
dim(res$attrs$time_bounds[[1]]) <- c(time = 3)
dim(res$attrs$time_bounds[[2]]) <- c(time = 3)
dim(res$attrs$Dates) <- c(time = 3)
dim(res$data) <- c(dataset = 1, var = 1, member = 6, time = 3, lat = 4, lon = 4)

# (4.4.1) All data in a single file
CST_SaveExp(data = res, ftime_dim = 'time', var_dim = 'var', 
            memb_dim = 'member', dat_dim = 'dataset', sdate_dim = NULL, 
            startdates = res$attrs$Dates, single_file = TRUE)
# (4.4.2) All data in separated files
CST_SaveExp(data = res, ftime_dim = 'time', var_dim = 'var', 
            memb_dim = 'member', dat_dim = 'dataset', sdate_dim = NULL,
            startdates = res$attrs$Dates, single_file = FALSE)

# (4.5) Forecast time units
CST_SaveExp(data = res, ftime_dim = 'time', var_dim = 'var', 
            memb_dim = 'member', dat_dim = 'dataset', sdate_dim = NULL, 
            startdates = res$attrs$Dates, single_file = TRUE,
            units_hours_since = FALSE)

#-----------------------------------------------------
# Example 5: Read data with Load
#-----------------------------------------------------

data <- lonlat_temp$exp
# data <- lonlat_temp$obs
# data <- lonlat_prec
CST_SaveExp(data = data, ftime_dim = 'ftime', 
            var_dim = NULL, dat_dim = 'dataset', sdate_dim = 'sdate', 
            single_file = FALSE, units_hours_since = FALSE)
# Now we read the output with Load:
# startDates <- c('20001101', '20011101', '20021101',
#                 '20031101', '20041101', '20051101')

# infile <- list(path = paste0(getwd(), 
#                              '/system5c3s/$VAR_NAME$/$VAR_NAME$_$START_DATE$.nc'))
# out_lonlat_temp <- CST_Load(var = 'tas', exp = list(infile), obs = NULL,
#                         sdates = startDates,
#                         nmember = 15,
#                         leadtimemax = 3,
#                         latmin = 27, latmax = 48,
#                         lonmin = -12, lonmax = 40,
#                         output = "lonlat")

# NOTE: This case hasn't been developed since the function to load data 
#       that will be maintianed will be CST_Start.
################################################################################