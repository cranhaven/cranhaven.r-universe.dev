# nhanesR 0.1.4

## CRAN resubmission fixes

* Expanded all unexpanded acronyms in `DESCRIPTION`: "NCHS" → "National Center
  for Health Statistics (NCHS)" and "NDI" → "National Death Index (NDI)".
* Added NHANES methodology URL reference to `DESCRIPTION`.
* Replaced all `\dontrun{}` with `\donttest{}` in examples; all examples
  require CDC network access so none were unwrapped.
* Fixed `\d` regex in `NH_describe` documentation (unknown Rd macro on Windows);
  replaced with `[0-9]`.

## Bug fixes

* Removed "2017-2020" from the internal LMF registry: CDC has not published
  `NHANES_2017_2020_MORT_2019_PUBLIC.dat`; `has_lmf_public` set to `FALSE`
  for that cycle.
* Fixed `cli` pluralization crash in `nhanes_merge()` when duplicate columns
  are found (`{i}` and `{?s}` quantity conflict resolved with `cli::qty()`).
* Fixed several example bugs exposed by switching from `\dontrun` to `\donttest`:
  missing `cycles` argument in `nhanes_download_analyte()` calls; list passed
  to `nhanes_mortality_link()` instead of a stacked data frame; undefined
  `linked_data` object in `nhanes_followup_summary()` example.

# nhanesR 0.1.3

## Bug fixes

* Default cache directory changed from a platform-specific path under `~` to
  `file.path(tempdir(), "nhanesR")`. This prevents nhanesR from writing to the
  user's home filespace without explicit consent, in compliance with CRAN Policy.
  To retain a persistent cache across sessions, set `nhanesR.cache_dir` in
  `~/.Rprofile`.
* `nhanes_cache_dir("~/my_nhanes_cache")` example wrapped in `\dontrun{}` to
  prevent directory creation during `R CMD CHECK`.

# nhanesR 0.1.2

## Bug fixes

* Fixed broken CDC mortality linkage URL in three vignettes (CDC reorganised
  their site; updated to current NCHS Data Linkage landing page).
* Removed non-standard sentence from `DESCRIPTION`.
* Added `inst/WORDLIST` to suppress spell-check NOTEs for domain abbreviations
  (NHANES, NCHS, LMF, NDI) and "codebook".

# nhanesR 0.1.1

## Breaking changes

* `nhanes_harmonize()`: the `prefer_mgdl` argument has been replaced by
  `units = c("conventional", "SI", "both")`. The default (`"conventional"`)
  preserves prior behaviour; set `units = "SI"` to retain SI columns and drop
  conventional duplicates instead.

# nhanesR 0.1.0

Initial release.

## New functions

* `nhanes_cycles()` — list all continuous NHANES cycles (1999–present) and
  optionally NHANES III (1988–1994), with cycle labels, begin/end years, and
  file-name suffixes.

* `nhanes_manifest()` — query the CDC data catalog for available files within
  a given cycle and component.

* `nhanes_download()` — download one or more NHANES XPT files from CDC,
  parse them into R data frames, and cache them locally. Falls back to
  `foreign::read.xport()` when `haven::read_xpt()` cannot parse older files.

* `nhanes_stack()` — row-bind per-cycle data frames into a single data frame,
  filling columns absent in some cycles with `NA`.

* `nhanes_merge()` — join NHANES components by SEQN (and optionally cycle),
  with survey-design-aware defaults.

* `nhanes_mortality_link()` — download the NCHS Public-Use Linked Mortality
  Files (LMF) for the relevant cycles and left-join them onto an analytic
  dataset by SEQN. Mortality follow-up runs through December 31, 2019.

* `nhanes_survival_prep()` — prepare a survival analysis dataset from a
  mortality-linked data frame: removes ineligible participants (`ELIGSTAT != 1`),
  creates `time` and `event` columns, optionally creates `event_cause` for
  cause-specific mortality, and warns about asymmetric follow-up across cycles.

* `nhanes_followup_summary()` — summarize follow-up time and event rates by
  cycle.

* `nhanes_ucod_labels()` — return the 11-category ICD-10 underlying cause-of-
  death recode used in the public-use LMF.

* `nhanes_search_variables()` — search the CDC NHANES variable catalog for
  variables whose name or description matches a keyword. Results are cached
  locally. The `summarize` argument (default `TRUE`) collapses one-row-per-cycle
  output into one row per unique variable name, with comma-separated file names,
  cycles, and an `n_cycles` count.

* `nhanes_variable_map()` — wraps `nhanes_search_variables()` to return a
  single-row-per-cycle lookup table (`cycle`, `variable_name`, `file_name`)
  for a given analyte. Automatically drops comment-code variables, prefers
  non-SI variables when both exist in a cycle, and accepts a `keep_vars`
  argument to disambiguate serum from urine forms of the same analyte
  (e.g. serum creatinine vs. urinary creatinine).

* `nhanes_cache_dir()` — view or change the local cache directory.

## Bug fixes and infrastructure

* Fixed CDC data file URLs: CDC reorganized all NHANES file paths from
  `/Nchs/Nhanes/{cycle}/` to `/Nchs/Data/Nhanes/Public/{begin_year}/DataFiles/`.
  All download functions updated accordingly.

* Added Content-Type check in the HTTP download helper: CDC returns HTTP 200
  with an HTML error page when a file has moved; `nhanesR` now detects this and
  aborts with an informative message rather than saving a corrupt file.

* Added `foreign::read.xport()` fallback for XPT files that `haven::read_xpt()`
  cannot parse (affects some files from NHANES cycles prior to 2003).

* MD5 hash sidecar files (`.md5`) are written alongside every cached RDS to
  detect corruption and trigger re-download when needed.

## Vignette

* Added "NHANES Mortality Linkage: A Complete Workflow" vignette illustrating
  the full pipeline from file discovery through survey-weighted Cox proportional
  hazards modeling, using serum total cholesterol and cardiovascular mortality
  across ten cycles (1999–2018) as a worked example.
