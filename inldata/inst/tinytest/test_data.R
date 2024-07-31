# setup tinytest for checkmate functionality
library("tinytest")
library("checkmate")
using("checkmate")

# test representation of nondetect result value using confidence interval
dt <- as.POSIXct("1999-04-20 10:55", tz = "America/Denver")
site_no <- "433322112564301" # well USGS 38
pcode <- "01030" # chromium, water, filtered, micrograms per liter
is <- samples$sample_dt == dt & samples$site_no == site_no & samples$pcode == pcode
d <- samples[is, ]
expect_set_equal(as.character(d$remark_cd), "<")
expect_set_equal(d$lab_li_va, 0)
expect_set_equal(d$lab_ui_va, d$result_va)

# test representation of radionuclide result value using confidence interval
dt <- as.POSIXct("2013-04-03 11:42", tz = "America/Denver")
site_no <- "433253112545901" # well USGS 20
pcode <- "07000" # tritium, water, unfiltered, picocuries per liter
is <- samples$sample_dt == dt & samples$site_no == site_no & samples$pcode == pcode
d <- samples[is, ]
expect_set_equal(d$result_va, 2750)
expect_set_equal(d$lab_li_va, 2534)
expect_set_equal(d$lab_ui_va, 2966)

# test groundwater level
dt <- as.POSIXct("2007-01-03 15:49", tz = "America/Denver")
site_no <- "432700112470801" # well USGS 1
is <- gwl$lev_dt == dt & gwl$site_no == site_no
d <- gwl[is, ]
expect_set_equal(d$lev_va, 593.27)
expect_set_equal(d$sl_lev_va, 4432.53)
expect_set_equal(d$lev_acy_va, 0.01)
expect_set_equal(d$sl_lev_acy_va, 0.02)

# test surface-water measurement
dt <- as.POSIXct("1999-05-11 15:00", tz = "America/Denver")
site_no <- "13132500" # station BIG LOST RIVER NR ARCO ID
is <- swm$stage_dt == dt & swm$site_no == site_no
d <- swm[is, ]
expect_set_equal(d$stage_va, 3.65)
expect_set_equal(d$disch_va, 108)
expect_set_equal(d$stage_acy_va, 0.18)
expect_set_equal(d$disch_acy_va, 5.4)

# test spatial extent
x <- terra::unwrap(dem) |> terra::ext() |> as.vector() |> round(digits = 1)
y <- c("xmin" = 142686.6, "xmax" = 257186.6, "ymin" = 181538.2, "ymax" = 295738.2)
expect_set_equal(x, y)
