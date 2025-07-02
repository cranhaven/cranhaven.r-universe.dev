## -----------------------------------------------------------------------------
#| eval: false
# install.packages("parzer")


## -----------------------------------------------------------------------------
#| eval: false
# remotes::install_github("ropensci/parzer")


## -----------------------------------------------------------------------------
#| eval: true
library(parzer)


## -----------------------------------------------------------------------------
#| label: "lat"
#| collapse: true
parse_lat("45N54.2356")
parse_lat("-45.98739874")
parse_lat("40.123°")
parse_lat("40.123N")
parse_lat("N45 04.25764")

# Invalid values -> NaN
parse_lat("191.89")

# Many inputs
x <- c("40.123°", "40.123N", "11.89", 12, "N45 04.25764")
parse_lat(x)

# Many inputs but with problems
x_warnings <- c("40.123°", "40.123N74.123W", "191.89", 12, "N45 04.25764")
parse_lat(x_warnings)


## -----------------------------------------------------------------------------
#| label: "lon"
#| collapse: true
parse_lon("45W54.2356")
parse_lon("-45.98739874")
parse_lon("40.123°")
parse_lon("74.123W")
parse_lon("W45 04.25764")

# Invalid values
parse_lon("361")

# Many inputs
x <- c("45W54.2356", "181", 45, 45.234234, "-45.98739874")
parse_lon(x)


## -----------------------------------------------------------------------------
#| label: "lonlat"
lons <- c("45W54.2356", "181", 45, 45.234234, "-45.98739874")
lats <- c("40.123°", "40.123N", 40, 12, "N45 04.25764")
parse_lon_lat(lons, lats)


## -----------------------------------------------------------------------------
#| label: "llstr"
lat_lon_strings <- c(
  "40.123°, 45W54.2356",
  "N40.123 E181.456",
  "40, 45",
  "12.9786 45.234234",
  "N45 04.25764, -45.98739874W"
)

parse_llstr(lat_lon_strings)


## -----------------------------------------------------------------------------
#| label: "parts"
#| collapse: true
parse_parts_lat("45N54.2356")
parse_parts_lon("-74.6411133")

# Many inputs
x <- c("40.123°", "40.123W", "191.89", 12, "E45 04.25764")
parse_parts_lon(x)

# Also handles invalid inputs gracefully
x_warning <- c("40.123°", "40.123N74.123W", "191.89", 12, "N45 04.25764")
parse_parts_lon(x_warning)


## -----------------------------------------------------------------------------
#| label: "pz_minute"
#| collapse: true
coords <- c(45.23323, "40:25:6N", "40° 25´ 5.994\" N")
pz_degree(lat = coords)
pz_minute(lat = coords)
pz_second(lat = coords)

coords <- c(15.23323, "40:25:6E", "192° 25´ 5.994\" E")
pz_degree(lon = coords)
pz_minute(lon = coords)
pz_second(lon = coords)


## -----------------------------------------------------------------------------
#| label: "pz_d"
#| collapse: true
pz_d(31)
pz_d(31) + pz_m(44)
pz_d(31) - pz_m(44)
pz_d(31) + pz_m(44) + pz_s(59)
pz_d(-121) + pz_m(1) + pz_s(33)


## -----------------------------------------------------------------------------
#| label: "hemisphere"
#| collapse: true
parse_hemisphere("74.123E", "45N54.2356")
parse_hemisphere("-120", "40.4183318")
parse_hemisphere("-120", "-40.4183318")
parse_hemisphere("120", "-40.4183318")

