# quicR 2.1.0

## New features
- Included a new function `calculate_metrics` which will make calls to the "calculate" family of quicR functions, and return a dataframe of all the metrics passed to the function.
- Added the `plot_metrics` function which accepts the output of the `calculate_metrics` function and returns a faceted plot of the calculated metrics.

# quicR 2.0.0

## Bug fixes
- Adjusted the delimiter in `get_meta` so that the second column doesn't have a leading white space.
- Fixed example scripts to work with the updated functions.
- Now suppressing messages from `get_wells`. This wasn't necessarily a bug, but was not ideal and the messages didn't provide any relevant information about what the function was doing.
- Fixed a typo in `get_wells` which caused the function to assigning a data frame as a variable instead of returning the data frame.
- Fixed a bug where the plate view would not render properly if the plate was not completely full.


## New features
- Provided the `calculate_threshold` function for calculating detection thresholds using a common method done in the literature.
- Added a `sep` argument to `get_sample_locations` for delimiting sample IDs and dilution factors.
- Converted the `calculate_MS` function to a vectorized function, and instead of using `lm`, the function looks at differences within the moving window. This provides a much faster calculation, but does reduce the accuracy slightly. May provide the option to do either methods in the future.
- Added the default option "Sample IDs" for `tab_name` in the `get_sample_locations` function.

## Miscellaneous
- Made the quicR logo!
- Provided better test files which have more ideal RT-QuIC data.
- Now includes the manuscript in the man folder which will be submitted for publication.

# quicR 1.1.1

-   Fixed organize_tables function to accept Excel files which do not have the metadata preamble. Will not affect output if file still has the preamble.
-   Sped up the get_meta function by having it read in sheet 1 from the Excel file which typically has much less data to read in.

# quicR 1.1.0

-   Added a new function "transpose_real.R" which takes the output of "get_real.R" and transposes it. This format makes it such that each read is a column labelled with the time of the read.

-   Removed the transposition sub-function from "normalize_RFU.R" to be its own separate function.

-   Updated example scripts to incorporate the new transposition function.

# quicR 1.0.3

-   Added janitor as a dependency.
-   Added a new test file which includes multiple real-time datasets. This is useful for the get_real function since the output returns a list of data frames if there are multiple real-time reads.
-   Converted many of the functions to be more in line with functional programming paradigm.
-   Included a documentation file that fixes the NOTE when using "." in a dplyr pipeline.
-   Fixed bugs in the example scripts.

# quicR 1.0.2

# quicR 1.0.1

# quicR 1.0.0

# quicR 0.1.1

-   Initial CRAN submission.
