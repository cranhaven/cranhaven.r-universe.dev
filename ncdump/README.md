
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ncdump)](https://CRAN.R-project.org/package=ncdump)
<!-- badges: end -->

Please note: the ideas in the this package were taken forward in
[tidync](https://CRAN.R-project.org/package=tidync) and
[ncmeta](https://CRAN.R-project.org/package=ncmeta) which are probably
the packages you should use instead.

# NetCDF metadata in tables in R

The `ncdump` package aims to simplify the way we can approach NetCDF in
R.

Currently the only real functionality is to return the complete file
metadata in tidy form. There is some experimental function to return
specific entities, but in my experience the existing tools `ncdf4`,
`raster`, `rgdal`, `RNetCDF` and `rhdf5` are sufficient for easily
accessing almost everything. Where they fall down is in providing easy
and complete information about what is available in the files, so that’s
where `ncdump::NetCDF` comes in.

Create an object that has a complete description of the file so that we
can easily see the available **variables** and **dimensions** and
**attributes**, and perform queries that find the details we need in a
form we can used, rather than just printed out on the screen.

``` r
library(dplyr)
library(ncdump)

ifile <- system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package = "ncdump")
con <- NetCDF(ifile)
```

Note that this is just the *metadata* in the file, but we’ve organized
it into a tidy list of tables so we can use it.

It looks good when we print those tables out, and it’s pretty clear how
we can use this information to drive the reader functions in the API
packages.

``` r
for (i in seq_along(con)) {
  print(names(con)[i])
  print(con[[i]])
}
#> [1] "dimension"
#> # A tibble: 4 × 9
#>   name    len unlim group_index group_id    id create_dimvar .dimension_ .group_
#>   <chr> <int> <lgl>       <int>    <int> <int> <lgl>               <int>   <int>
#> 1 lat    2160 FALSE           1    65536     0 TRUE                    0   65536
#> 2 lon    4320 FALSE           1    65536     1 TRUE                    1   65536
#> 3 rgb       3 FALSE           1    65536     2 FALSE                   2   65536
#> 4 eigh…   256 FALSE           1    65536     3 FALSE                   3   65536
#> [1] "unlimdims"
#> NULL
#> [1] "dimension_values"
#> # A tibble: 6,739 × 3
#>       id  vals .dimension_
#>    <int> <dbl>       <int>
#>  1     0  90.0           0
#>  2     0  89.9           0
#>  3     0  89.8           0
#>  4     0  89.7           0
#>  5     0  89.6           0
#>  6     0  89.5           0
#>  7     0  89.5           0
#>  8     0  89.4           0
#>  9     0  89.3           0
#> 10     0  89.2           0
#> # ℹ 6,729 more rows
#> [1] "group"
#> # A tibble: 3 × 7
#>      id name                 ndims nvars natts fqgn                      .group_
#>   <int> <chr>                <int> <int> <int> <chr>                       <int>
#> 1 65536 ""                       4     4    65 ""                          65536
#> 2 65537 "processing_control"     0     0     4 "processing_control"        65537
#> 3 65538 "input_parameters"       0     0    21 "processing_control/inpu…   65538
#> [1] "file"
#> # A tibble: 1 × 12
#>   filename    writable    id error safemode format is_GMT ndims natts unlimdimid
#>   <chr>       <lgl>    <int> <lgl> <lgl>    <chr>  <lgl>  <dbl> <dbl>      <dbl>
#> 1 /perm_stor… FALSE    65536 FALSE FALSE    NC_FO… FALSE      4    90         -1
#> # ℹ 2 more variables: nvars <dbl>, .file_ <int>
#> [1] "variable"
#> # A tibble: 2 × 18
#>   name  ndims natts prec  units longname group_index storage shuffle compression
#>   <chr> <int> <int> <chr> <chr> <chr>          <int>   <int>   <int>       <int>
#> 1 chlo…     2    12 float "mg … Chlorop…           1       2       0           4
#> 2 pale…     2     0 unsi… ""    palette            1       1       0          NA
#> # ℹ 8 more variables: unlim <lgl>, make_missing_value <lgl>,
#> #   hasAddOffset <lgl>, addOffset <dbl>, hasScaleFact <lgl>, scaleFact <dbl>,
#> #   .variable_ <dbl>, .group_ <int>
#> [1] "vardim"
#> # A tibble: 4 × 2
#>   .variable_ .dimension_
#>        <dbl>       <int>
#> 1          0           1
#> 2          0           0
#> 3          3           3
#> 4          3           2
#> [1] "attribute"
#> [1] "NetCDF attributes:"
#> [1] "Global"
#> [1] "\n"
#> # A tibble: 1 × 65
#>   product_name                  instrument title project platform temporal_range
#>   <chr>                         <chr>      <chr> <chr>   <chr>    <chr>         
#> 1 S2008001.L3m_DAY_CHL_chlor_a… SeaWiFS    SeaW… Ocean … Orbview… day           
#> # ℹ 59 more variables: processing_version <chr>, date_created <chr>,
#> #   history <chr>, l2_flag_names <chr>, time_coverage_start <chr>,
#> #   time_coverage_end <chr>, start_orbit_number <int>, end_orbit_number <int>,
#> #   map_projection <chr>, latitude_units <chr>, longitude_units <chr>,
#> #   northernmost_latitude <dbl>, southernmost_latitude <dbl>,
#> #   westernmost_longitude <dbl>, easternmost_longitude <dbl>,
#> #   geospatial_lat_max <dbl>, geospatial_lat_min <dbl>, …
#> [1] "\n"
#> [1] "Variable attributes:"
#> character(0)
```

## Rationale

The main problems with the existing packages are that they are either
too high-level or too low-level and it’s this twilight zone in the
middle where `ncdump` comes in, where the user needs to reach down a bit
deeper to make things work. Using the wrappers around the raw API
`ncdf4` and `RNetCDF` is very raw, and for the most part is analogous to
the NetCDF API itself, though you don’t have to compile the code. These
packages aren’t the same, and neither can read all kinds of NetCDF
files. Both do support API features like NetCDF 4, server interfaces
(OpenDAP and Thredds and the like), support for HDF, and parallel
processing but it depends on how you build the underlying libraries. The
details on this are not on the agenda here.

The worker for most low-level NetCDF access is the command line utility
`ncdump -h` - if not called with the `h`eader argument you can actually
dump the data contents in various ways, including binary forms to other
formats. The header is usually what you are expected to read to figure
out how to program against the contents. It’s not completely
automatable, because it’s so general it’s unlikely that your generalized
code will solve any particular practical task and there’s no general
libary for NetCDF that will give you practical outcomes for all types of
data. It’s a non-virtuous cycle.

The `raster` package is excellent for NetCDF, but it only works
“off-the-shelf” for regularly xy-gridded data in 2D, 3D or 4D variables.
The mapping of dimensions to these variables is patchy, because the
format is so general and many data variables have rectilinear or
curvilinear coordinates, which are either ignored by raster or cause it
to stop without reading anything. If you know what you want you can get
it to work with any 2D or higher variable. Raster is a high-level
domain-specific library, for GIS-y rasters, or grids that use an affine
transform to define their coordinate system in xy.

GDAL, available in `rgdal` has similar limitations to raster for the
same affine/GIS reason, but also makes different choices when
approaching higher dimensions. Essentially all higher dimensions get
unrolled as extra bands for a GDAL data set. They are completely
independently implemented, GDAL has an in-built NetCDF driver (also a
GMT variant) and raster uses `ncdf4` directly. (Raster will use `rgdal`
when it can for many formats, but you cannot actually override raster’s
used of ncdf4 without uninstalling that package. It’s all quite
complicated and poorly understood unfortunately, and it keeps changing).

There is another package `rhdf5` which provides excellent modern
facilities for HDF5 and NetCDF4, and notably this can read NetCDF files
with compound types that no other package can do. This would be perfect
if it could also read NetCDF “classic format”, i.e. version 3.x prior to
NetCDF 4.

I use `ncdump` in conjunction with `angstroms` and various other
projects. I can perform joins and select/filter tasks on the tables it
returns, but it’s not designed in any high-level consistent way, it just
gives all the information it can get.

## Development

A format method for the object above could recreate the print out you
see from ncdump itself.

A future package may wrap the various NetCDF R packages to do the actual
read, but each is useful in different ways. The main ones are `RNetCDF`,
`ncdf4` and `rhdf5` (Bioconductor).

This project was split out of the “R and NetCDF interface development”
project. <https://github.com/hypertidy/rancid>

## Code of Conduct

Please note that the ncdump project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
