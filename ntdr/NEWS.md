# ntdr 0.4.0

* Remove httr from Imports and tidyverse from Suggests
* Add tidyr, rappdirs, httr2, and rlang to Imports and add withr to Suggests.
* Improve validation for `agency` and `modes` arguments
* Add supporting for setting `cache` argument with an option
* Add package-level documentation
* Thanks to new contributor [Eli Pousson](https://github.com/elipousson/) for making all these improvements


# ntdr 0.3.4
* small updates to documentation to account for changes in April data release (#14)

# ntdr 0.3.3
* Bugfix: Fixing breaking change with the August data release (#12)

# ntdr 0.3.2
* Bugfix: Change in source data leads to incorrect column labels (#9)

# ntdr 0.3.1
* Bugfix: Change in source data layout breaks package (https://github.com/vgXhc/ntdr/issues/6)

# ntdr 0.3.0
* retire `get_ntd_url` as user-facing function
* update documentation to reflect new NTD language about data type

# ntdr 0.2.1
* Bugfix: Cache function fixed (https://github.com/vgXhc/ntdr/issues/2)

# ntdr 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Returned data frame now includes `ntd_variable` to denote what `value` represents
