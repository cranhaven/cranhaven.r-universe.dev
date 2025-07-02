# ARUtools 0.7.2

## Bugs

  * Remove sessioninfo dependency. sessioninfo::session_info() was throwing errors in tests
  * `stringr::str_detect()` could fail with NULL pattern from `osVersion` in `check_sox` (MichaelChirico:patch-1).
  * Base pipe placeholder in `calc_sun()` causing build to fail for R version 4.1. 
     Placeholder has been removed

# ARUtools 0.7.1

## Bugs 

  * `clip_wave()` was failing due to error in rlang evaulation in `nse_names()`. (#44)

# ARUtools 0.7.0

## New

  * `clean_metadata()` now can detect time zone offsets in filenames and return
       a new column `tz_offset` (#27).
       - `create_pattern_tz_offset()` creates the pattern to look for the offset.
  * Look around patterns added to `create_pattern_XX` functions (#6)
      - new function `create_lookaround()` helps create these patterns.
  * Add internal environment for global variables and `get_pattern()`,
      `add_pattern_aru_type()`, and `set_pattern()` were added as helper functions.
  * ARU type detection is improved and now includes manufacturer and type. New function
      `guess_ARU_type()` can be called on a path to guess the ARU type.

## Update
 
   * "job" package removed from `clip_wav()` ( #25 )
   * Updates to `clean_logs()` (#28, #34)
  
## Bugs 

 * Fix issue with no oversamples (GRTS fails from select_samples() if n is a data frame and all n_os are zero. #40)
 * Fix to_lower() in check_names() causes error if upper cases used. #41
 
 
# ARUtools 0.6.2

* Initial CRAN submission.

1st CRAN version! 🥳 
Changes since 0.6.1 relate to getting package ready for CRAN submission and so included improved documentation, final coverage of tests, removing unnecessary functions. 

- #36 tracks workflow to submission
- Removed `play_random_track()` (dd3ddb195fe50c37cb83a15110651c4c659014dd) #38 
- Added examples to final functions and remove \dontrun where necessary (44879bd3508af743049f0c5528d386d9f56a8475)

# ARUtools 0.6.0

## New
* Cleaned up functions for recording selection, spectrograms, clipping waves, 
  acoustic indices and creating dirs (#8, #11, #12, #13, #23)
* Added NSE for relevant functions, these now use `col_COLNAME`, changed arguments
for non-NSE functions to `name_COLNAME` (#15)
* Ensure that data order is unchanged when passing through cleaning functions (#19)
* Added sampling vignette

## Bugs
* Fixed bug `calc_sun()` corrupting date column (#18)

# ARUtools 0.5.1

## New
* `add_wildtrax()` - New helper function to create and append WildTrax file names

## Bugs
* `clean_gps()` - Fixed errors when processing only GPX files


# ARUtools 0.5.0

* `clean_gps()`
  * can handle GPX files via `sf` now
  * distance cutoff results in a warning (not an error), returning
    the data with the `max_dist` column, so users can see which site was problematic.
  * now checks for distance by site groups `aru_id` and `site_id` by default
  * pattern matches for GPS column headers have been expanded
  * catches errors but continues reporting on failed loading (remove `skip_bad` argument)
  * `check_problems()` now also checks GPS meta data

* `create_pattern_XXX()` 
  * Now accept multiple options
  * all separators are non-optional, but provide `""` as pseudo-optional
  * `create_pattern_site_id()` ids do not have to have a suffix
  
* `clean_metadata()` accepts multiple pattern options

* `clean_site_index()` allows no date columns (`col_date_time = NULL`)

* `add_sites()`
  * Rename `dt_type` to `by_date`
  * Take mean of multiple sites with `by_date = "date"` (instead of truncating)
  * Use `by_date = NULL` to skip joining by date range

* Workflow now works with sf input (must be POINT geometries)
  * `clean_site_index()`
  * `add_sites()`
  * `calc_sun()`

* Timezones are now more explicit
  * Expect local time marked with UTC
  * Existing non-UTC timezones are stripped with a message
  * Errors returned if there are more than one relevant date_time column with 
    different timezones
    
* Vignettes
  * Mini spatial workflow (vignettes/spatial.Rmd)
  * Explaining timezones (vignettes/timezones.Rmd)

# ARUtools 0.4.0.9000
* Major overhaul of first half of the workflow
* Main functions now
 - `clean_metadata()`
 - `clean_gps()` / `clean_site_index()`
 - `add_sites()`
 - `calc_sun()`
* Helper functions for checking, and for creating regex patterns 
