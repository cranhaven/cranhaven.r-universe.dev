#*******************************************************************************
# Title: Example script to create 's2dv_cube' objects
# Author: Eva Rifà Rovira 
# Date: 16/01/2024
#*******************************************************************************
# This example shows how to create an 's2dv_cube' object. 
# There are two ways of creating an 's2dv_cube' object. 
# (1) With the function s2dv_cube(): create it from scratch with any data. 
# (2) With the function CST_Start(). This function returns an 's2dv_cube' 
#     from an 'startR_array'.

# Needed packages 
library(CSTools)
library(startR)
################################################################################
#-----------------------------------------------------
# Example 1: Function s2dv_cube() from defined data
#-----------------------------------------------------
# Minimal use case, with s2dv_cube function.

# In this example we use the function s2dv_cube() to create an object of class 
# 's2dv_cube' with the correct structure. 

# (1) We define the array with named dimensions:
dat <- array(1:100, dim = c(time = 10, lat = 4, lon = 10))
# (2) We define the coordinates as a list of vectors:
coords <- list(time = 1:10, lat = 43:40, lon = 0:9)
# (3) The metadata:
metadata <- list(tas = list(level = '2m'),
                 lon = list(cdo_grid_name = 'r360x181'),
                 lat = list(cdo_grid_name = 'r360x181'))
# (4) The creation of Dates array. 
# First the initial date:
ini_date <- as.POSIXct('2010-01-01', format = '%Y-%m-%d')
# The sequence of dates
dates <- seq(ini_date, by = 'days', length.out = 10)
# We define the dates dimensions
dim(dates) <- c(time = 10)
# (5) We call the function s2dv_cube()
dat_cube <- s2dv_cube(data = dat, coords = coords,
                      varName = 'tas', metadata = metadata,
                      Dates = dates,
                      when = "2019-10-23 19:15:29 CET", 
                      source_files = c("/path/to/file1.nc", "/path/to/file2.nc"),
                      Datasets = 'test_dataset')

# We print the result to see the 's2dv_cube' structure:
# > dat_cube
# 's2dv_cube'
# Data          [ 1, 2, 3, 4, 5, 6, 7, 8 ... ] 
# Dimensions    ( time = 10, lat = 4, lon = 10 ) 
# Coordinates  
#  * time : 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 
#  * lat : 43, 42, 41, 40 
#  * lon : 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 
# Attributes   
#    Dates  : 2010-01-01 2010-01-02 2010-01-03 2010-01-04 2010-01-05 ... 
#    varName  : tas 
#    metadata :  
#       tas 
#         other : level 
#       lon 
#         other : cdo_grid_name 
#       lat 
#         other : cdo_grid_name 
#    Datasets  : test_dataset 
#    when  : 2019-10-23 19:15:29 CET 
#    source_files  : /path/to/file1.nc ... 

#-----------------------------------------------------
# Example 2: Function as.s2dv_cube() 
#-----------------------------------------------------
# (1) Example using CST_Start

# NOTE 1: CST_Start() is just a wrapper of function Start() with the  
#         transformation to 's2dv_cube' object.
# NOTE 2: In order that the input argument auxiliary functions from startR 
#         work, we need to call them explicitly the startR namespace. 
#         (e.g. startR::indices())

# We just need to define a CST_Start call with all the information: 

repos1 <- "/esarchive/exp/ecmwf/system5_m1/monthly_mean/$var$_f6h/$var$_$sdate$.nc"
repos2 <- "/esarchive/exp/ecmwf/system4_m1/monthly_mean/$var$_f6h/$var$_$sdate$.nc"

res <- CST_Start(dat = list(list(name = 'system4_m1', path = repos2),
                            list(name = 'system5_m1', path = repos1)),
                 var = c('tas', 'sfcWind'),
                 sdate = c('20160101', '20170101'),
                 ensemble = startR::indices(1:2),
                 time = startR::indices(1:2),
                 lat = startR::indices(1:10),
                 lon = startR::indices(1:10),
                 synonims = list(lat = c('lat', 'latitude'),
                                 lon = c('lon', 'longitude')),
                 return_vars =  list(time = 'sdate',
                                     longitude = 'dat',
                                     latitude = 'dat'),
                 metadata_dims = c('dat', 'var'),
                 retrieve = TRUE)


# Now we can explore the object:

# 1st level
names(res)
# "data"   "dims"   "coords" "attrs" 

dim(res$data)
#   dat      var    sdate ensemble     time      lat      lon 
#    2        2        2        2        2       10       10 

res$coords$lon
#  [1] 0.000000 0.703125 1.406250 2.109375 2.812500 3.515625 4.218750 4.921875
#  [9] 5.625000 6.328125
attr(res$coords$lon, 'indices')
# [1] FALSE
# NOTE: The attribute 'indices' is FALSE, it means that the longitude elements 
#       are the actual values of longitude coordinate.

res$coords$ensemble
# [1] 1 2
# attr(,"indices")
# [1] TRUE

# Now we take a look into the Dates array. It must have the time dimensions 
# of the data.
dim(res$attrs$Dates)
# sdate  time 
#     2     2 

# To see the nested list structure of the object, we just need to use the 
# function str():
str(res)

#-----------------------------------------------------

# (2) Example using as.s2dv_cube() function

# We'll load the data with Start and then we'll transform the 'startR_array' 
# to 's2dv_cube' object with the function as.s2dv_cube(). We are going 
# to load the same data as before, with the same call:

repos1 <- "/esarchive/exp/ecmwf/system5_m1/monthly_mean/$var$_f6h/$var$_$sdate$.nc"
repos2 <- "/esarchive/exp/ecmwf/system4_m1/monthly_mean/$var$_f6h/$var$_$sdate$.nc"

res <- Start(dat = list(list(name = 'system4_m1', path = repos2),
                        list(name = 'system5_m1', path = repos1)),
             var = c('tas', 'sfcWind'),
             sdate = c('20160101', '20170101'),
             ensemble = startR::indices(1:2),
             time = startR::indices(1:2),
             lat = startR::indices(1:10),
             lon = startR::indices(1:10),
             synonims = list(lat = c('lat', 'latitude'),
                             lon = c('lon', 'longitude')),
             return_vars =  list(time = 'sdate',
                                 longitude = 'dat',
                                 latitude = 'dat'),
             metadata_dims = c('dat', 'var'),
             retrieve = TRUE)

# Now, we use the function as.s2dv_cube() to transform the 'startR_array' 
# into an 's2dv_cube':
res_cube <- as.s2dv_cube(res)

# If we call directly the object directly into the terminal, we can see 
# all the elements nicely:

# > res_cube
# 's2dv_cube'
# Data          [ 248.241973876953, 247.365753173828, 6.80753087997437, 5.46453714370728, 247.256896972656, 248.500869750977, 6.25862503051758, 5.76889991760254 ... ] 
# Dimensions    ( dat = 2, var = 2, sdate = 2, ensemble = 2, time = 2, lat = 10, lon = 10 ) 
# Coordinates  
#  * dat : system4_m1, system5_m1 
#  * var : tas, sfcWind 
#  * sdate : 20160101, 20170101 
#    ensemble : 1, 2 
#    time : 1, 2 
#  * lat : 89.4628215685774, 88.7669513528422, 88.0669716474306, 87.366063433082, 86.6648030134408, 85.9633721608804, 85.2618460607126, 84.5602613830534, 83.8586381286076, 83.1569881285417 
#  * lon : 0, 0.703125, 1.40625, 2.109375, 2.8125, 3.515625, 4.21875, 4.921875, 5.625, 6.328125 
# Attributes   
#    Dates  : 2016-02-01 2017-02-01 2016-03-01 2017-03-01 
#    varName  : tas sfcWind 
#    metadata :  
#       time 
#         units : hours since 2016-01-01 00:00:00 
#         other : ndims, size, standard_name, calendar 
#       lon 
#         units : degrees_east 
#         long name : longitude 
#         other : ndims, size, standard_name, axis 
#       lat 
#         units : degrees_north 
#         long name : latitude 
#         other : ndims, size, standard_name, axis 
#       tas 
#         units : K 
#         long name : 2 metre temperature 
#         other : prec, dim, unlim, make_missing_value, missval, hasAddOffset, hasScaleFact, code, table, grid_type 
#       sfcWind 
#         units : m s**-1 
#         long name : 10 meter windspeed 
#         other : prec, dim, unlim, make_missing_value, missval, hasAddOffset, hasScaleFact, code, table, grid_type 
#    Datasets  : system4_m1 ... 
#    when  : 2024-01-17 11:38:27 
#    source_files  : /esarchive/exp/ecmwf/system4_m1/monthly_mean/tas_f6h/tas_20160101.nc ... 
#    load_parameters  : 
#        ( system4_m1 )  : dat = system4_m1, var = tas ..., sdate = 20160101 ... 
#        ... 

################################################################################