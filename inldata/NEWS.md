# inldata 1.2.5

- Ensure functions `assert_url` and `download_file` fail gracefully with an informative message
  if the internet resource is not available (and not give a check warning nor error).

# inldata 1.2.4

- Ensure that unit tests do not fail when the internet connection is unavailable.
- Add `mountains` dataset based on the slope threshold of the digital elevation model.

# inldata 1.2.3

- Add a unit test for the `clean_sf` function to replace the example in its help documentation.

# inldata 1.2.2

- Remove graphic images from the help documentation of spatial datasets.
- Prevent unit tests from writing to the working directory.

# inldata 1.2.1

- Revise the examples in the help documentation for the `clean_sf`, `make_data_release`,
  and `write_datasets` functions to reduce the execution time.

# inldata 1.2.0

- In the `dl` dataset, `sdate` field renamed to `min_dt`.
- The `iwd` dataset has been geocorrected by shifting it to the east.
- In the `percopnds` dataset, fields `status`, `min_dt`, and `max_dt` have been added,
  along with more percolation ponds.
- Substitute 7-zip system dependency with the 'archive' R-package, which necessitates 'libarchive-dev' on Linux.
- In the `samples` dataset, replace the empty-character string (`""`) values
  in the `remark_cd` and `rpt_lev_cd` fields with missing values (`NA`).
- In `units`, `samples`, and `parameters` datasets, rename field form `parm_unit` to `unit_cd`.
- In `backgroud`, `benchmarks`, and `dl` datasets,
  substitute `srsname` and `parm_unit` fields with `parm_nm` field.
- Revised `iwd ` dataset location
- In the  `percopnds` dataset, added `status`, `start_dt`, `end_dt` fields and added more percolation ponds.

# inldata 1.1.5

- Revise datasets to incorporate data from the 2023 U.S. Census, which was previously based on the 2022 data.
- Remove `mountains` dataset because of limited spatial coverage.
- Replace coordinate accuracy code with accuracy value in seconds (`sites$coord_acy_va`).
- Remove `inldata::` from examples in help documentation and unit tests because unnecessary.

# inldata 1.1.4

- Reduce ellapsed time for `make_data_release` function example.
- Export 'internal' functions to avoid using `:::` in examples.
- Replace `\dontrun{}` with `\donttest{}` in the `assert_url` function example.
- Avoid using `print()` within functions.

# inldata 1.1.3

- Address issues with failing CRAN checks: remove hyperlink and refactor CITATION file.

# inldata 1.1.2

- Decrease run time (< 5s) for example in `make_data_release` function help documentation.

# inldata 1.1.1

- Fix popups in leaflet map markers.

# inldata 1.1.0

- Add latest data records.
- Add data for new monitoring sites.
- Add vignettes to package website.
- Display summary tables for sites and parameters in package vignettes.
- Display Entity Relationship Diagram (ERD) in package vignette.
- Replace `projection` dataset (text string) with `crs` dataset (crs-class object).
- Rename `topo` dataset to `dem`, convert elevation from meters to feet, and remove hill shade layer.
- Add `make_shade` function, used to calculate hill shade from a elevation raster.
- Add `make_data_release` function, used to create a data release.
- Add `make_datasets` function, used to create package datasets.
- Add and rename fields in many of the datasets.

# inldata 1.0.4

- Rename Git branch from `master` to `main`.
- Remove `labels` dataset.
- Build website using **pkgdown**.
- Remove **sp** and **raster** package dependencies, replaced with **sf** and **terra** packages.
- Fix broken URL's.
- Update license.
- Re-style code based on static code analysis using the **lintr** package.

# inldata 1.0.3

- Fix invalid URL.

# inldata 1.0.2

- Fix invalid URL's.

# inldata 1.0.1

- In DESCRIPTION file, move **sp** and **raster** form Suggests to Imports.

# inldata 1.0.0

- Host repo on USGS OpenSource GitLab (code.usgs.gov)
