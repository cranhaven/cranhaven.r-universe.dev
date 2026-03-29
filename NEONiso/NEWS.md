# NEONiso (development version)

# NEONiso 0.8.0

* Added an HDF5 abstraction layer (`R/hdf5_utils.R`) that supports both hdf5r
(CRAN) and rhdf5 (Bioconductor) backends. Both packages are now listed as
suggested dependencies rather than hard imports, and the backend is selected
automatically at runtime based on availability.
* Removed the dependency on the `caret` package. Cross-validation in
`estimate_calibration_error()` is now performed using base R, which reduces
the package's dependency footprint. Note that `Rsquared` is no longer
included in the cross-validation output (R-squared is still reported from the
model fit itself).
* Various performance improvements to calibration regression routines:
  - Data frames in `fit_carbon_regression()` and `fit_water_regression()` are
    now pre-allocated to the correct size rather than a fixed 200,000 rows.
  - Time vectors are constructed once outside the calibration loop instead of
    being grown element-by-element inside it.
  - Formula objects and the calibration half-width are hoisted out of the loop
    to avoid repeated construction.
  - Model summaries are cached to avoid redundant recomputation.
* Simplified `convert_POSIXct_to_NEONhdf5_time()` to use `format()` instead
of manual string construction with individual `lubridate` accessor functions.
* Output file writing functions (`setup_output_file`, `write_carbon_*`,
`write_water_*`) now accept an open file identifier, reducing redundant file
open/close operations during calibration output. The `rhdf5::h5closeAll()`
calls in the main calibration functions have been replaced with targeted
`h5_close()` calls.
* Now requires `neonUtilities` >= 2.3.0 (previously >= 2.1.1).
* Expanded the test suite with new test files for HDF5 utility functions,
HDF5 round-trip operations, and regression snapshot tests.

# NEONiso 0.7.2

* Fixes a bug that causes the manage_local_ec_archive function
to miss a large set of files that are on the remote server.

# NEONiso 0.7.1

* Substantial upgrades to package documentation.
* The name "Bowling_2003" in methods and functions has been deprecated in
favor of "gainoffset." This change was made primarily to improve clarity of
what the methods actually do. Code that uses the old "Bowling_2003" nomenclature
will continue to work until at least version 0.8.0, but will issue a deprecation
warning.
# NEONiso 0.7.0

* calibrate_carbon_bymonth and calibrate_water_bymonth have been removed. Use
calibrate_carbon and calibrate_water instead.
* terrestrial_relocatable_sites() has been renamed terrestrial_gradient_sites()
to comply with a similar change made by NEON.
* Patch to hard-coded 9 minute interval to comply with switch to 6 minute averaging
interval for isoCo2 product, following ATM ISO TWG recommendations.

# NEONiso 0.6.4

* Small patch adding a minimum number of observations argument to the 
calibrate_carbon functions.

# NEONiso 0.6.3

* Small patch to address change requested by CRAN. Issue with package documentation 
introduced in ROxygen 7.0.0: https://github.com/r-lib/roxygen2/issues/1491. 

# NEONiso 0.6.2

* This will be the last release that has the *_by_month functions! They have
deprecated for about a year, and will be removed before next release. Use
calibrate_carbon (mature) or calibrate_water (experimental).
* Adds an argument to manage_local_EC_archive that allows checking out
specific releases (e.g., RELEASE-2023) instead of release + provisional. To check
out all data, set to NULL.
* Exports get_Rstd, a helper function to return the heavy-to-light isotope
ratio of the relevant international standard.
* Allows use of the useFasttime argument in dependency neonUtilities::stackEddy, 
for neonUtilities >= 2.3.0. stackEddy(useFasttime = TRUE) is ~3x faster on large
datasets than stackEddy(useFasttime = FALSE), which improves performance significantly
when trying to work on larger timeseries in the NEON archives.
* Adds new cross-validation error metrics to linear regression calibration method
for carbon. Note that these metrics are still to be considered experimental and
may continue to change.

# NEONiso 0.6.1

* Exports new helper function for getting sites with water isotopes, 
water_isotope_sites().
* The reference_corrections vignette was blank in the previous release - 
it is updated in this release (#81)
* Makes select functions used internally consistent with upcoming changes
to tidyselect (h/t Hadley Wickham)

# NEONiso 0.6.0

* An experimental calibration routine for water isotopes has been added. It does
have some known issues (e.g., no correction is made for concentration dependence
of the analyzers yet), and any data produced from this function should be considered
provisional.
* Added capability to plot data used in carbon calibration regression in order
to help identify periods where calibration parameters seem to be okay, but
quality of calibrated data is degraded.
* Added cross-validation error estimates to carbon calibration routines.
* The calibrate_carbon_bymonth() function has been marked as deprecated, but will
be removed no earlier than version 0.7.0 or the end of 2022. 
The more flexible calibrate_carbon() function should be used.
* A bug that generated non-nonsensical CO2 and d13C values has been fixed (#72).
* The calibrate_carbon() function now also provides calibrated values for
reference material measurement. These are useful for determining calibration
error when one of the reference materials is omitted from the calibration (e.g., 
generate calibration relationships to the high and medium standards, then estimate
error as the difference between calibrated measurement and known reference values).
* Reduced the number of tests that run on CRAN to minimize compute resources requested.
* Made a function that corrects a few mismatched reference values more visible,
and added a few more instances where reference data needs to be corrected.

# NEONiso 0.5.3

* Small maintenance release that addresses a NOTE on CRAN.

# NEONiso 0.5.2

* Fixes a bug where an rhdf5 function was not being imported properly,
generating warnings and errors on CRAN.
* Updates manage_local_ec_archive to avoid a scenario that was creating
duplicate files, thereby being more efficient with drive space.

# NEONiso 0.5.1

* This update takes advantage of upgrades to the stackEddy function 
in neonUtilities 2.1.1, which allows the calibration routines to run
more efficiently on NEON's HDF5 files. For example, calibrate_carbon(),
runs ~2x faster if neonUtilities 2.1.1 is used instead of earlier versions.

# NEONiso 0.5.0

* Adds the calibrate_carbon() function, which can be used to generate
monthly output files or can be used to bundle all of the months of data
available for an individual site into a single file. In the long-term,
calibrate_carbon() will be the 'workhorse' function for NEON's carbon data,
and calibrate_carbon_bymonth will be deprecated in a future release.

# NEONiso 0.4.0

* This is the initial release on CRAN.
