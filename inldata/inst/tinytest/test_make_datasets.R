# setup tinytest for checkmate functionality
library("tinytest")
library("checkmate")
using("checkmate")

# test existence of data-raw folder
destdir <- tempfile("")
expect_error(
  current = make_datasets(path = tempdir(), destdir = destdir),
  pattern = "does not exist"
)
unlink(destdir)

# test making parameters
pcodes <- "07000"
n <- length(pcodes)
x <- inldata:::mds_parameters(pcodes)
expect_data_frame(x, nrows = n, col.names = "named")
x <- inldata:::tabulate_parm_data(x, samples)
expect_data_frame(x, nrows = n, col.names = "named")

# test making detection limits
data <- data.frame(
  "srsname" = c("Tritium", "Tritium"),
  "pcode" = c("07000", "07000"),
  "unit_cd" = c("pCi/L", "pCi/L"),
  "lab_det_lim_va" = c("500", "200"),
  "min_dt" = c("1949-01-16", "2003-04-01"),
  "reference" = c("Bartholomay", "Bartholomay")
)
x <- inldata:::mds_dl(data, parameters)
expect_data_frame(x, nrows = nrow(data), col.names = "named")

# test making benchmarks
bm <- data.frame(
  "Chemical Name" = "trans-1,3-Dichloropropene",
  "CAS Registry Number" = "10061-02-6",
  "USGS Parameter Code" = "34699",
  "Chemical Class" = "VOC",
  "MCL (micrograms/L)" = NA_character_,
  "Chronic Noncancer HHBP (micrograms/L)" = NA_character_,
  "Carcinogenic HHBP (micrograms/L)" = NA_character_,
  "Noncancer HBSL (micrograms/L)" = "200",
  "Cancer HBSL (micrograms/L)" = "0.3-30",
  "Benchmark Remarks" = NA_character_,
  check.names = FALSE
)
mcl_extras <- data.frame(
  "srsname" = "Tritium",
  "pcode" = "07000",
  "mcl" = "20000",
  "unit_cd" = "pCi/L"
)
n <- nrow(bm) + nrow(mcl_extras)
x <- inldata:::mds_benchmarks(bm, mcl_extras, parameters)
expect_data_frame(x, nrows = n, col.names = "named")

# test making sites
data <- data.frame(
  "agency_cd" = "USGS",
  "site_no" = "432700112470801",
  "station_nm" = "02N 31E 35DCC1 USGS 1",
  "network_cd" = "A",
  "pos" = "3"
)
x <- inldata:::mds_sites(data, crs)
expect_multi_class(x, classes = c("sf", "data.frame"))
expect_set_equal(nrow(x), nrow(data))
y <- inldata:::tabulate_site_data(x, samples, gwl, swm)
expect_data_frame(y, nrows = nrow(x), col.names = "named")

# test making surface-water measurements
site_no <- "13131000"
is <- sites$site_no == site_no
x <- inldata:::mds_swm(sites = sites[is, ], tz = "America/Denver")
expect_data_frame(x, min.rows = 1, col.names = "named")

# test making groundwater levels
site_no <- "433500112572501"
is <- sites$site_no == site_no
x <- inldata:::mds_gwl(sites = sites[is, ], tz = "America/Denver")
expect_data_frame(x, min.rows = 1, col.names = "named")

# test making units
data <- data.frame(
  "unit_cd" = "deg C",
  "unit_ds" = "degrees Celsius"
)
x <- inldata:::mds_units(data)
expect_data_frame(x, nrows = nrow(data), col.names = "named")

# test making background concentrations
data <- data.frame(
  "srsname" = "Sodium",
  "pcode" = "00930",
  "unit_cd" = "mg/L",
  "bkgrd_min" = "8.3",
  "bkgrd_max" = "14.8",
  "reference" = "Bartholomay"
)
x <- inldata:::mds_background(data, parameters)
expect_data_frame(x, nrows = nrow(data), col.names = "named")

# test unit conversion
data <- data.frame(
  "pcode" = c("01065", "00631"),
  "unit_cd" = c("ug/L", "ug/L")
)
x <- inldata:::convert_units(data, parameters)
expect_data_frame(x,
  types = c("character", "character", "character", "numeric"),
  nrows = nrow(data),
  col.names = "named"
)
expect_set_equal(colnames(x), c("pcode", "from", "to", "mult"))
