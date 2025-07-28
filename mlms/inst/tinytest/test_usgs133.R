library("tinytest")
library("checkmate")
using("checkmate")

# identify record
site_nm <- "USGS 133"
port_nu <- 6L
stime_dt <- "2008-08-27 12:25"

# check well altitude
is <- wells$site_nm == site_nm
d <- wells[is, ]
expect_equal(current = d$alt_va, target = 4893.62)

# check port depth
is <- ports$site_nm == site_nm & ports$port_nu == port_nu
d <- ports[is, ]
expect_equal(current = d$wl_depth_va, target = 453.47)
expect_equal(current = d$temp_compl_va, target = 11.57)
expect_equal(current = d$baro_compl_va, target = 12.328)
expect_equal(current = d$press_compl_va, target = 86.43)
expect_equal(current = d$tp_depth_va, target = 619.50)

# check hydraulic head calculation
is <- heads$site_nm == site_nm &
  heads$stime_dt == as.POSIXct(stime_dt, tz = "America/Denver") &
  heads$port_nu == port_nu
d <- heads[is, ]
expect_equal(current = d$temp_va, target = 11.57)
expect_equal(current = d$baro_va, target = 12.293)
expect_equal(current = d$press_va, target = 94.91)
expect_equal(current = d$press_head_va, target = 190.65)
expect_equal(current = d$total_head_va, target = 4464.8)
