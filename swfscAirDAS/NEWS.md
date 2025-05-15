# swfscAirDAS 0.3.1

* Moved repo to https://github.com/swfsc, and updated all associated documentation

* Changed license to Apache License (== 2), based on NMFS guidance at https://nmfs-opensci.github.io/GitHub-Guide


# swfscAirDAS 0.3.0

* `airdas_sight` now has both ObsStd and SightStd columns; SightStd definition updated. `airdas_effort` standardizes the definition for the 'included' column. (#3)

* Added `purrr` as a dependency for `airdas_sight` functionality


# swfscAirDAS 0.2.3

* swfscAirDAS now depends on R >= 4.0.0 for consistency across DAS packages

* Updated License to CC0 for consistency across DAS packages


# swfscAirDAS 0.2.2

* The 'segdata' output of `das_effort` now includes DateTime1 and DateTime2, the date/time of the start and end points of each segment. The column order of the segdata data frame was also slightly changed for consistency

* `airdas_chop_condition`, `airdas_chop_equallength`, `airdas_chop_section`, and `airdas_segdata` are now exported for documentation purposes only


# swfscAirDAS 0.2.1

* Users can now capitalize all transect codes using the `trans.upper` argument in `das_process`

* Added info messages to `airdas_check`, as well as a check for valid latitude and longitude values

* Fixed a bug in `airdas_check` when checking multiple files


# swfscAirDAS 0.2.0

* Catch up internally with changes made in swfscDAS. This package now depends on swfscDAS version >= 0.3

* `airdas_sight` column name changes: 'Sp' is now 'SpCode' for consistency with swfscDAS, and 'TurtleSizeFt' is now 'TurtleSize' since this data can be a character code

* Changed `airdas_chop_equal` to `airdas_chop_equallength` for naming consistency. This change should not affect users, as it does not affect any of the syntax required when using `airdas_effort`

* Renamed items in effort list output of airdas_effort and airdas_effort_sight: ‘siteinfo’ is now ‘sightinfo’, and segdata columns indicating the number of sightings or animals on a segment are now formatted as ‘description’_‘species code’, e.g. “nSI_mn”

* Where applicable, error, warning, and informational messages now report both the file name and line number. This makes the messages more useful when working with concatenated AirDAS objects with multiple files


# swfscAirDAS 0.1.0

* Initial version, presented to NMFS users
