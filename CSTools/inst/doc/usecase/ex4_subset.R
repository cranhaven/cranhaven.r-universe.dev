#*******************************************************************************
# Title: Example script to subset any dimension of an 's2dv_cube'
# Author: Eva Rifà Rovira 
# Date: 16/11/2024
#*******************************************************************************
# This example shows how to subset any dimension of an 's2dv_cube'. To do it, 
# we will use the function CST_Subset. This function is the 's2dv_cube' method 
# version of Subset from the package ClimProjDiags.
# (1) First we will see how Subset works.
# (2) Then, we will use CST_Subset with an 's2dv_cube'

# Needed packages:
library(CSTools)
library(ClimProjDiags)

################################################################################
#-----------------------------------------------------
# Example 1: Subset an example array
#-----------------------------------------------------
# This is a minimal use case about spatial coordinates subset.

# (1) We create the array amd we print it:
dat <- array(1:100, dim = c(lat = 10, lon = 10))
dat
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#  [1,]    1   11   21   31   41   51   61   71   81    91
#  [2,]    2   12   22   32   42   52   62   72   82    92
#  [3,]    3   13   23   33   43   53   63   73   83    93
#  [4,]    4   14   24   34   44   54   64   74   84    94
#  [5,]    5   15   25   35   45   55   65   75   85    95
#  [6,]    6   16   26   36   46   56   66   76   86    96
#  [7,]    7   17   27   37   47   57   67   77   87    97
#  [8,]    8   18   28   38   48   58   68   78   88    98
#  [9,]    9   19   29   39   49   59   69   79   89    99
# [10,]   10   20   30   40   50   60   70   80   90   100

# (2) We call the function Subset from ClimProjDiags and we see the result:
dat_subset <- Subset(x = dat, along = c('lat', 'lon'), indices = list(1:5, 1:7),
                     drop = 'all')
dat_subset
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
# [1,]    1   11   21   31   41   51   61
# [2,]    2   12   22   32   42   52   62
# [3,]    3   13   23   33   43   53   63
# [4,]    4   14   24   34   44   54   64
# [5,]    5   15   25   35   45   55   65

#-----------------------------------------------------
# Example 2: Subset an 's2dv_cube' using sample data
#-----------------------------------------------------
# In this example we will not drop any dimension, we will select only the first 
# member, the first and the second start dates, and also subset the longitude and 
# keep only the values from [0, 21]:

# (1) Explore the sample data:
dat <- lonlat_temp_st$exp

dat$dims
# dataset     var  member   sdate   ftime     lat     lon 
#       1       1      15       6       3      22      53 

dat
# 's2dv_cube'
# Data          [ 279.994110107422, 280.337463378906, 279.450866699219, ... ] 
# Dimensions    ( dataset = 1, var = 1, member = 15, sdate = 6, ftime = 3, 
#                 lat = 22, lon = 53 ) 
# Coordinates  
#  * dataset : dat1 
#  * var : tas 
#    member : 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 
#  * sdate : 20001101, 20011101, 20021101, 20031101, 20041101, 20051101 
#    ftime : 1, 2, 3 
#  * lat : 48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, ...
#  * lon : 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, ...
# Attributes   
#    Dates  : 2000-11-01 2001-11-01 2002-11-01 2003-11-01 2004-11-01 ... 
#    varName  : tas 
#    metadata :  
#       lat 
#         units : degrees_north 
#         long name : latitude 
#       lon 
#         units : degrees_east 
#         long name : longitude 
#       ftime 
#         units : hours since 2000-11-01 00:00:00 
#       tas 
#         units : K 
#         long name : 2 metre temperature 
#    Datasets  : dat1 
#    when  : 2023-10-02 10:11:06 
#    source_files  : /monthly_mean/tas_f6h/tas_20001101.nc ... 
#    load_parameters  : 
#        ( dat1 )  : dataset = dat1, var = tas, sdate = 20001101 ... 
#        ... 

# (2) Call the function CST_Subset:
dat_subset <- CST_Subset(x = dat, along = c('member', 'sdate', 'lon'), 
                         indices = list(1, 1:2, 1:22), drop = 'none')

# (3) Explore the 's2dv_cube'                         
dat_subset
# 's2dv_cube'
# Data          [ 279.994110107422, 277.161102294922, 278.825836181641, 276.8271484375, 276.052703857422, 276.950805664062, 280.677215576172, 277.285247802734 ... ] 
# Dimensions    ( dataset = 1, var = 1, member = 1, sdate = 2, ftime = 3, lat = 22, lon = 22 ) 
# Coordinates  
#  * dataset : dat1 
#  * var : tas 
#    member : 1 
#  * sdate : 20001101, 20011101 
#    ftime : 1, 2, 3 
#  * lat : 48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27 
#  * lon : 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21 
# Attributes   
#    Dates  : 2000-11-01 2001-11-01 2000-12-01 2001-12-01 2001-01-01 ... 
#    varName  : tas 
#    metadata :  
#       ftime 
#         units : hours since 2000-11-01 00:00:00 
#         other : ndims, size, standard_name, calendar 
#       lat 
#         units : degrees_north 
#         long name : latitude 
#         other : ndims, size, standard_name, axis 
#       lon 
#         units : degrees_east 
#         long name : longitude 
#         other : ndims, size, standard_name, axis 
#       tas 
#         units : K 
#         long name : 2 metre temperature 
#         other : prec, dim, unlim, make_missing_value, missval, hasAddOffset, hasScaleFact, code, table 
#    Datasets  : dat1 
#    when  : 2023-10-02 10:11:06 
#    source_files  : /esarchive/exp/ecmwf/system5c3s/monthly_mean/tas_f6h/tas_20001101.nc ... 
#    load_parameters  : 
#        ( dat1 )  : dataset = dat1, var = tas, sdate = 20001101 ... 
#        ... 

################################################################################