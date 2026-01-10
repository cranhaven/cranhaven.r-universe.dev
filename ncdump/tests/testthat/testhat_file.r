library(testthat)
if (Sys.info()["nodename"] == "ace-ecostats") {
romsfile <- file.path("/mnt/mertzdata/mdl/mer015_1", c("mer_his_0001.nc", "mer_his_0002.nc", "mer_his_0576.nc"))

library(ncdump)
roms1 <- NetCDF(romsfile[1])
roms2 <- NetCDF(romsfile[2])
roms3 <- NetCDF(romsfile[3])
roms2 <- roms3
names1 <- names(roms1)
names2 <- names(roms2)

expect_that(names1, equals(names2))
## dimension, dimvals, different since ocean_time is 25 in 1 but 24 in 2
## file different since filename changes
## unlimdims, groups, variable, vardim, and attribute are all identical

## much worse comparing 1 with "mer_his_0576.nc"
expect_that(roms1[[1]], equals(roms2[[1]]))
expect_that(roms1[[2]], equals(roms2[[2]]))
expect_that(roms1[[3]], equals(roms2[[3]]))
expect_that(roms1[[4]], equals(roms2[[4]]))
expect_that(roms1[[5]], equals(roms2[[5]]))
expect_that(roms1[[6]], equals(roms2[[6]]))
expect_that(roms1[[7]], equals(roms2[[7]]))
expect_that(roms1[[8]], equals(roms2[[8]]))

rfile <- "/rdsi/PRIVATE/data_local/acecrc.org.au/ROMS/s_corney/cpolar/ocean_his_3101.nc"
expect_silent(NetCDF(rfile))

}



