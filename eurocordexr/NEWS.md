# eurocordexr 0.2.5

- added `nc_grid_to_dt_raw()`, which gives a more basic access to the netcdf file using the RNetCDF library
- added pkgdown website and some usage articles

---

# eurocordexr 0.2.4

- added internal data vectors with short names for GCMs and RCMs
- removed depends, only namespace imports
- added tests
- added some test data to run examples in `get_inventory()` and `check_inventory()`
- added function `get_inventory_cmip5()` to get CMIP5 gcm info

---

# eurocordexr 0.2.3

- updated `check_inventory()` to work with CORDEX-Adjust data, too. Check for full period also works if historical is merged to the RCP in filenames.
- added `date_range` parameter to `nc_grid_to_dt()` to allow easier extraction of subset of data
- added helper function `get_varnames()` to list variable names in a netcdf file 
- `variable` argument in `nc_grid_to_dt()` and `rotpole_nc_point_to_dt()` will be guessed if not supplied 
- made the inventory data.table from `get_inventory()` a separate class, with specific print method that prints more nicely. For example, list of files is not printed anymore, thus also changed the default of `add_files` in `get_inventory()` to `TRUE` instead of `FALSE`.
- added workaround for "months since" calendar, which does not work well with ncdf4.helpers
- added further check in `check_inventory()` to test if rcp scenarios have a corresponding historical run

---

# eurocordexr 0.2.2

- updated description with links, put packages and API in single quotes
- spelled out TRUE and FALSE
- check_inventory() now returns an overloaded list of class "eurocordex_inv_check" with specific 
  print method
- added test netCDF files, cropped from EURO-CORDEX for size; used for examples

---

# eurocordexr 0.2.1

- changed package imports to not use `.onLoad()` with `library()` but rely on `#' @import`
- updated `check_inventory()` and `compare_variables_in_inventory()` to reflect changes from `get_inventory()`
- added a check for complete periods in `check_inventory()`
- misc polishing for submission to CRAN
- consolidate licenses for CRAN and github
- added `globalVariables()` and reformatted examples to remove R CHECK NOTES

---

# eurocordexr 0.2.0

- improved `get_inventory()` overview of file dates
- initial version imported from Eurac gitlab
- Added a `NEWS.md` file to track changes to the package.
