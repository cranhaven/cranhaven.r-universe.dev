# setup tinytest for checkmate functionality
library("tinytest")
library("checkmate")
using("checkmate")

# test parsing of station name
x <- parse_station_nm("03N 29E 33CCC1 MIDDLE 2051 PORT6 ZONE6 826.8FT")
expect_set_equal(x, "MIDDLE 2051")

# test parsing of help documentation
l <- parse_rd_db(package = "inldata")
expect_list(l, types = "list", names = "named")

# test metadata validation
if (test_file_exists(Sys.which("mp"), access = "x")) {
  dir <- tempfile("")
  x <- validate_metadata(
    file = system.file("extdata/test.xml", package = "inldata"),
    destdir = dir,
    opts = NULL
  )
  expect_false(x)
  unlink(dir, recursive = TRUE)
}

# test cleaning simple feature
x <- clean_sf(cities,
  cols = c("key" = "id", "geometry" = "geometry"),
  agr = c("key" = "identity"),
  crs = sf::st_crs(3857),
  extent = sf::st_bbox(esrp)
)
expect_multi_class(x, classes = c("sf", "data.frame"))

# test creating an entity relationship diagram
file <- make_dm() |> make_erd()
expect_file_exists(file, access = "rw")
unlink(file)

# test creating a data release
destdir <- tempfile("")
include <- c("crs", "units", "cities", "dem")
rngdates <- c(samples$sample_dt, gwl$lev_dt) |> range()
l <- make_data_release(
  metadata = system.file("extdata/metadata.json", package = "inldata"),
  package = "inldata",
  destdir = destdir,
  include = include,
  quiet = TRUE,
  bounding = sites,
  rngdates = rngdates
)
expect_list(l, names = "named")
x <- list.files(destdir)
expect_character(x, min.len = length(include), names = "unnamed")
tools::file_path_sans_ext(x) |> unique() |> expect_subset(choices = c(include, "metadata"))
unlink(destdir, recursive = TRUE)

# test cleaning a simple feature
x <- clean_sf(inl, cols = "geometry", crs = sf::st_crs(3857))
expect_multi_class(x, classes = c("sf", "data.frame"))

# test URL assertion
expect_silent(assert_url("https://www.usa.gov/"))
expect_null(assert_url("https://fail/on/bad/url/"))
expect_null(assert_url("https://anyapi.io/api/v1/exchange/rates?base=NAN&apiKey=123"))
