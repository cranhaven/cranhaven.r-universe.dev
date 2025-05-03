# MazamaTimeSeries 0.2.17

Updated dependencies:

* MazamaCoreUtils 0.4.15 => 0.5.2

* All `mts_~()` functions that return an _mts_ object now return an empty _mts_
when an empty _mts_ is used as input. This prevents breaks in the middle of 
pipelines so that "emptiness" only needs to be checked at the end.
* All `sts_~()` functions that return an _sts_ object now return an empty _sts_
when an empty _sts_ is used as input.

# MazamaTimeSeries 0.2.16

* Added `mts_pull()` to get columns of data from `mts$meta` or `mts$data`.

# MazamaTimeSeries 0.2.15

* Fixed `mts_setTimeAxis()` so that always retains the original timezone 
associated with `mts$data$datetime`.
* Removed non-compliant timezones like `"US/Hawaii"` from the codebase.
* Deprecated `mts_filterDatetime()` in favor of `mts_setTimeAxis()` which is
more general.

# MazamaTimeSeries 0.2.14

* Added `mts_slice_head()` and `mts_slice_tail()`.
* Added `mts_setTimeAxis()` to modify mts time spans.

# MazamaTimeSeries 0.2.13

* Bumped **MazamaCoreUtils** dependency to 0.4.15.
* Added `includeEnd` argument to `mts/sts_filterDatetime()`.

# MazamaTimeSeries 0.2.12

* Added error message when calling `mts_select()` with duplicate `deviceDeploymentIDs`.
* Added warning message when calling `mts_select()` with `deviceDeploymentIDs` not
found in `mts`.
* Added `mts_arrange()` to order time series based on values of a `mts$meta` column.

# MazamaTimeSeries 0.2.11

* Improved error message from `mts_filterDate()` and `mts_filterDatetime()` when
a `POSIXct` value is encountered with no timezone information. This can happen
when using `lubridate::now()`.
* Update R dependency to 4.0.0.

# MazamaTimeSeries 0.2.10

* Fixed bug in `mts_collapse()` so that it now handles metadata columns  of class
`POSIXct`.

# MazamaTimeSeries 0.2.9

* Added `mts_trim()` to remove all data records with only missing data.
* Updated `mts_combine()` with an `overlapStrategy` argument. With
`overlapStrategy = "replace all"`, values from later timeseries (including `NA`)
always replace values from earlier timeseries. With `overlapStrategy = "replace na"`, 
values from later timeseries only replace `NA` values in earlier timeseries.

# MazamaTimeSeries 0.2.8

* Updated `Carmel_Valley` to match the latest version of the **AirMonitor** package.
* Added `Camp_Fire` dataset from the **AirMonitor** package.
* Added `mts_selectWhere()` to select time series based on data values.

# MazamaTimeSeries 0.2.7

* Updated `mts/sts_filterMeta()` to return an empty _mts/sts_ object if an empty _mts/sts_
object is passed in. Previous behavior was to stop with an error message. The
new behavior allows multiple filtering steps to be piped together without having
to check for an empty _mts/sts_ at each step. Now you can check once at the end
of the pipeline.

# MazamaTimeSeries 0.2.6

* Added dependency on **MazamaRollUtils**
* Added internal functions: `.sample()`, `.findOutliers()`.
* Added `mts_sample()`.

# MazamaTimeSeries 0.2.5

* Removed **readr** package from dependencies.
* Addressed CRAN check issues.

# MazamaTimeSeries 0.2.4

* Updated to add a Zenodo DOI badge.

# MazamaTimeSeries 0.2.3

* Added `sts_summarize()`.
* Updated `example_raws` dataset.

# MazamaTimeSeries 0.2.2

* Documentation fixes requested by CRAN.

# MazamaTimeSeries 0.2.1

* Fixed urls and typos during CRAN preparation.

# MazamaTimeSeries 0.2.0

Version 0.2 of the package is ready for operational use.

* Replaced `sts_join()` with`sts_combine()`.

# MazamaTimeSeries 0.1.6

* Improved default parameter settings in `mts_collapse()`.
* Added `trimEmptyDays` argument to `mts_trimDate()`.

# MazamaTimeSeries 0.1.5

* Fixed bug in `mts_collapse()`.
* Additional consistency checks in `monitor_isValid()`.

# MazamaTimeSeries 0.1.4

* Renamed `mts_distance()` to `mts_getDistance()`.
* Fixed bugs related to leftover `monitorID` references.

# MazamaTimeSeries 0.1.3

* Added `replaceMeta` argument to `mts_combine()`.

# MazamaTimeSeries 0.1.2

* Added `mts_summarize()`.

# MazamaTimeSeries 0.1.1

* Fixed bug in `mts_combine()`.

# MazamaTimeSeries 0.1.0

* Full documentation, examples and tests for basic _mts_ functionality.

# MazamaTimeSeries 0.0.9

* Now depending on **MazamaCoreUtils** 0.4.10.

# MazamaTimeSeries 0.0.8

* Added `mts_collapse()`, `mts_distance()` and `mts_select()`.
* Renamed `mts_filter()` to `mts_filterData()` to be more explicit

# MazamaTimeSeries 0.0.7

* Added `timeInfo()` and supporting functions.
* Added `Carmel_Valley` example dataset.

# MazamaTimeSeries 0.0.6

* Added "location" utility functions.
* Removed dependency on **MazamaLocationUtils**
* Fixed bug in `~_filterDate()`.
* Removed `sts_from~()` functions.

# MazamaTimeSeries 0.0.5

* Added tests for all functions.
* Added `mts_combine()`.
* Adding `mts_filter~()` equivalents to `sts_filter~()` functions.
* Improved warning messages in `sts_isValid()` and `mts_isValid()`.

# MazamaTimeSeries 0.0.4

* Added functions for loading data into the `sts` format:
  - `sts_fromTidyDF()`
  - `sts_fromCSV()`

# MazamaTimeSeries 0.0.3

* Added basic unit tests for `sts` functions.
* Added the Developer Style Guide vignette

# MazamaTimeSeries 0.0.2

* Added basic utility functions for `sts` and `mts` objects.
* Added the following `sts` functions:
  - `sts_filter()`
  - `sts_filterDate()`
  - `sts_filterDatetime()`
  - `sts_join()`
  - `sts_toTidyDF()`
  - `sts_trimDate()`

# MazamaTimeSeries 0.0.1

* Initial setup.
