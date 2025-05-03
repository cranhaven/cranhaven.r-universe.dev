#*******************************************************************************
# Title: Script to modify the dimensions of the 's2dv_cube'
# Author: Eva Rifà Rovira 
# Date: 18/01/2024
#*******************************************************************************
# In this example, we will explore different methods to modify the dimensions 
# of the 's2dv_cube':
# (1) Changing dimension names
# (2) Adding new dimensions
# (3) Merge 2 dimensions
# (4) Split a dimension

# Needed packages:
library(CSTools)

################################################################################
#-----------------------------------------------------
# Example 1: Change dimension names with CST_ChangeDimNames
#-----------------------------------------------------
# With using this function, we can change the dimension names in all elements 
# of the 's2dv_cube' object:

# (1) Check original dimensions and coordinates
lonlat_temp$exp$dims
names(lonlat_temp$exp$coords)
dim(lonlat_temp$exp$attrs$Dates)
# (2) Change 'dataset' to 'dat' and 'ftime' to 'time'
exp <- CST_ChangeDimNames(lonlat_temp$exp,
                          original_names = c("dataset", "ftime", "lon", "lat"),
                          new_names = c("dat", "time", "longitude", "latitude"))
# (3) Check new dimensions and coordinates
exp$dims
names(exp$coords)
dim(exp$attrs$Dates)

#-----------------------------------------------------
# Example 2: Insert a new dimension with CST_InsertDim
#-----------------------------------------------------
# With this function, we can add a dimension into the 's2dv_cube'. 
# NOTE: When the dimension that we want to add has length greater than 1, the 
#       values of the data are repeated for that new dimension.

# (1) Check original dimensions and coordinates
lonlat_temp$exp$dims
names(lonlat_temp$exp$coords)
# (2) Add 'variable' dimension
exp <- CST_InsertDim(lonlat_temp$exp,
                     posdim = 2,
                     lendim = 2,
                     name = "variable",
                     values = c("tas", "tos"))
# (3) Check new dimensions and coordinates
exp$dims
exp$coords$variable
# We see that the values will be repeated along the new dimension:
exp$data[, , 1, 1, 1, 1, 1]

#-----------------------------------------------------
# Example 3: Merge two dimensions with CST_MergeDims
#-----------------------------------------------------
# In this example, we will merge the dimensions corresponding to the latitude 
# and the longitude of the data. The new dimension will be named 'grid'.

# (1) Call the function:
new_data <- CST_MergeDims(lonlat_temp$exp, merge_dims = c('lat', 'lon'), 
                          rename_dim = 'grid')

# (2) Check the dimensions of the data:
dim(new_data$data)
# dataset  member   sdate   ftime    grid 
#       1      15       6       3    1166 

# (3) Check the names of the coordinates:
names(new_data$coords)
# [1] "dataset"  "member"  "sdate"   "ftime"   "grid"  

# (4) Explore the object by printing it in the terminal:
new_data
# NOTE: Be aware that when we print the object, we see that its name in 
# "Coordinates" field appears without the asterisk (*) at its left. This means 
# that the values of that coordinate, are indices, not the actual values. We 
# can also find this information with the attribute "indices": 
attributes(new_data$coords$grid)
# $indices
# [1] TRUE

# (5) Now, we want to merge time dimensions start date and forecast time:
new_data <- CST_MergeDims(data = lonlat_temp_st$exp, merge_dims = c('sdate', 'ftime'))
# In this case, the Dates dimensions will be merged too.
# (6) Check the dimensions of Dates:
dim(new_data$attrs$Dates)
# sdate 
#    18 

# NOTE: When we want to merge temporal and other dimensions nature, 
#       the Dates dimensions are kept as the original. In this case, the function 
#       returns a Warning Message, we must pay attention!
new_data <- CST_MergeDims(data = lonlat_temp$exp, 
                          merge_dims = c('lat', 'ftime'), 
                          rename_dim = 'newdim')

#-----------------------------------------------------
# Example 4: Split two dimensions with SplitDim and CST_SplitDim
#-----------------------------------------------------
# In this example, we will start working with the function SplitDim, 
# that it can be used to split dimensions of an array. 

# NOTE: Take into account that time dimensions will be treated differently than 
#       other dimensions:

# (1) Decadal example: We define an array of consecutive days of different years:
dates <- seq(as.Date("01-01-2000", "%d-%m-%Y", tz = 'UTC'),
             as.Date("31-12-2005","%d-%m-%Y", tz = 'UTC'), "day")
dim(dates) <- c(time = 2192)

# (2) Now, we will split the array in a new 'year' dimension:
dates_year <- SplitDim(dates, indices = dates,
                       split_dim = 'time', freq = 'year')
dim(dates_year)
# time year 
#  366    6 

# (3) Now, we can try: freq = 'month' and 'day'
dates_month <- SplitDim(dates, indices = dates,
                        split_dim = 'time', freq = 'month')

dates_day <- SplitDim(dates, indices = dates,
                      split_dim = 'time', freq = 'day')

# (4) Finnally, we need to convert them again from numeric to 'POSIXct':
dates_year <- as.POSIXct(dates_year * 24 * 3600, origin = '1970-01-01', tz = 'UTC')
dates_month <- as.POSIXct(dates_month * 24 * 3600, origin = '1970-01-01', tz = 'UTC')
dates_day <- as.POSIXct(dates_day * 24 * 3600, origin = '1970-01-01', tz = 'UTC')

#-----------------------------------------------------

# In the following example, we will use the sample data of the package. We 
# will use lonlat_prec_st because it is daily data:

# NOTE: By Jan 2024, a development is needed regarding updates in other fields 
#       of the 's2dv_cube'

# (1) Call the function CST_SplitDim with adding 'day' dimension:
data_day <- CST_SplitDim(lonlat_prec_st, indices = lonlat_prec_st$attrs$Dates[1, ],
                         split_dim = 'ftime', freq = 'day')
# (2) Explore the dimensions of the data array
dim(data_day$data)
# dataset     var  member   sdate   ftime     lat     lon     day 
#       1       1       6       3       1       4       4      31 

# (3) Call the function CST_SplitDim with adding 'month' dimension:
data_month <- CST_SplitDim(lonlat_prec_st, indices = lonlat_prec_st$attrs$Dates[1,],
                           split_dim = 'ftime', freq = 'month')

dim(data_month$data)
# dataset     var  member   sdate   ftime     lat     lon   month 
#       1       1       6       3      31       4       4       1 

# (4) Call the function CST_SplitDim with adding 'year' dimension:
data_year <- CST_SplitDim(lonlat_prec_st, indices = lonlat_prec_st$attrs$Dates[,1],
                          split_dim = 'sdate', freq = 'year')
dim(data_year$data)
# dataset     var  member   sdate   ftime     lat     lon    year 
#       1       1       6       1      31       4       4       3 

################################################################################