# extractox 1.2.0

# extractox 1.1.0 

## Major Changes & Enhancements
 * Automated Server Compatibility: Functions connecting to EPA servers (e.g., `extr_comptox`,
  `extr_iris`, `extr_pprtv`) now automatically handle connection issues that can occur on systems with
  modern `libcurl` and `OpenSSL`. The functions detect problematic configurations and use the
  `{condathis}` package to perform requests with a compatible version of `curl` in an isolated
  environment. This ensures reliability without requiring manual user intervention.

## Minor Enhancements
* `extr_chem_info` now accepts the argument `domain` to specify the PubChem domain (`substance` 
  or `compound`). 
* `extr_tox` now accepts a `delay` parameter to control the delay between requests, 
  helping to avoid rate-limiting errors.
* Request Throttling**: Added a `delay` parameter to PubChem functions to allow for a pause
  between requests, helping to avoid rate-limiting errors.
* Verbose Option: A `verbose` option has been added to several functions for more detailed
  output during execution.

## Bug Fixes
* Testing: Corrected various tests, including a fix for an incorrect row count expectation in
      the PubChem test.
* Fixed `extr_chem_info` duplicated name in outout columns (#47).

* Function Cleanup: Refined the `extr_comptox` function, removing unnecessary requirements and
      cleaning up the code.

# extractox 1.0.0

## Bug Fixes
* Fixed `extr_iris` extracting the correct number of chemicals without 
  repetition (#15).
* Fixed `extr_comtox` working when a single `download_items` different from 
  `DTXCID` is selected (#17).
* Fixed `extr_casrn_from_cid` failure when no results are found.

## New Features
* Added `extr_ice_assay_names` to retrieve ICE assay names (#16).
* Added `extr_monograph` to check if a substance is listed in WHO IARC 
  monograph and return its details (#19).
* Added `extr_pprtv` to extract information from the EPA Provisional 
  Peer-Reviewed Toxicity Values database (#20). Introduced `save_to_cache` 
  and `load_from_cache` functions to avoid re-downloading the file each time. 
  See `force` argument.

## Other Breaking Changes
* Removed `cancer_types` argument from `extr_iris`. Database returns a 
  dataframe with different columns based on `request` arguments.
* Removed `stop_at_warning` argument from `extr_casrn_from_cid`. Now warns 
  and returns a dataframe with NA if no IDs are found.
* `extr_tox` now returns a longer list of dataframes, including the outputs 
  of `extr_monograph` and `extr_pprtv`.

## Enhancements and Fixes
* Added `verbose` argument to all `extr_` functions (#18).
* Unified behavior across all `extr_` functions when chemicals are not found 
  (#30-#35):
   - For all functions except `extr_comptox`, a `query` column reports the IDs 
     searched. In `extr_comptox`, this info is in the `main_sheet` element. For 
     `extr_ice`, `query` values contain all IDs found.
   - Results now contain rows with NA values for all columns (except `query`).
   - `extr_pprtv` and `extr_monograph` use `save_and_match` to output results 
     with NA for missing IDs.
* Improved and extended all unit tests.
* `extr_comptox` now outputs a list of dataframes with clean names.
* Fixed `extr_ctd` column names: `pub_med_ids` or `pub_med_i_ds` are now 
  `pubmed_ids`.
* Introduced `extr_pubchem_section_` internal function to fetch FEMA and GHS 
  info, avoiding repeated code.
* Introduced `check_na_warn` internal function to generate warnings for 
  missing IDs.
* Created `with_extr_sandbox` to handle cache for CRAN examples.


# extractox 0.1.0

* Initial CRAN submission.
