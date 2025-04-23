# *News*

# njtr1 0.1
Initial public release

# njtr1 0.1.1
Hotfix to address archival by CRAN, one vignette updated

# njtr1 0.2.0
Support added for downloading historical crash data from 2001 - 2016

Date fields are now automatically parsed for driver DOB and crash date fields using `{lubridate}`

`get_njtr1()` refactored to fail gracefully when internet resources unavailable

New argument added to select only geotagged crash data suitable for mapping (geo = TRUE)

Fix an error that caused coordinates to be unavailable in crash data due to parsing failures

# njtr1 0.3.0
Added new function `read_njtr1()` for reading offline crash table data. This function supports reading in and cleaning a TXT or ZIP file containing NJTR-1 data.

It is now possible to download data for a specific county by specifying its name in the county argument in `get_njtr1()`. The default option still downloads data for the entire state.

Added example data for testing package functionality without internet connectivity.

# njtr1 0.3.1
Excess white space is now removed by default for character columns.
`get_njtr1()` has been refactored to utilize `read_njtr1()` on downloaded files to avoid repetitions in code & better maintainability.

# njtr1 0.3.2
Added support for 2020 crash tables released by NJDOT in 2022

Fixed an issue with lazydata on Windows

Updates to package documentation
