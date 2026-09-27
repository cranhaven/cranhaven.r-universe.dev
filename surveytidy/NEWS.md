# surveytidy 0.6.0

## New features

### survey_collection support

Collection-aware methods for all standard dplyr/tidyr verbs are now
dispatched per-survey when called on a `survey_collection`. The result
is a new `survey_collection` whose `@id`, `@if_missing_var`, and
`@groups` properties are preserved.

- Data-masking verbs: `filter()`, `filter_out()`, `mutate()`, `arrange()`
- Tidyselect verbs: `select()`, `relocate()`, `rename()`, `rename_with()`,
  `drop_na()`, `distinct()`, `rowwise()`
- Grouping verbs: `group_by()`, `ungroup()`, `group_vars()`, `is_rowwise()`
- Slicing verbs: `slice()`, `slice_head()`, `slice_tail()`, `slice_min()`,
  `slice_max()`, `slice_sample()`
- Collapsing verbs: `pull()` (returns a vector via `vctrs::vec_c()`),
  `glimpse()` (default mode binds members; `.by_survey = TRUE` per-member)

The `.if_missing_var` argument on each verb (`"error"` (default) or
`"skip"`) lets you override the collection's stored missing-variable
behaviour for a single call. Skipped surveys are reported via the typed
message class `surveytidy_message_collection_skipped_surveys`.

The 6 join verbs (`left_join()`, `right_join()`, `inner_join()`,
`full_join()`, `semi_join()`, `anti_join()`) error with
`surveytidy_error_collection_verb_unsupported` when called on a
`survey_collection`. Apply joins inside a per-survey pipeline before
constructing the collection.

### surveycore re-exports

`library(surveytidy)` is now sufficient to use the collection
construction and setter API. The following surveycore symbols are
re-exported:

- `as_survey_collection()`
- `set_collection_id()`
- `set_collection_if_missing_var()`
- `add_survey()`
- `remove_survey()`

## Bug fixes

* `replace_when()` and `replace_values()` no longer retain stale value labels
  for values absent from the recoded result. Previously, collapsing or replacing
  values left label entries (e.g. `"High" = 4`) attached to a vector that
  contained no such values. User-supplied `.value_labels` are always preserved.

## New error / warning / message classes

- `surveytidy_error_collection_verb_emptied`
- `surveytidy_error_collection_verb_failed`
- `surveytidy_error_collection_by_unsupported`
- `surveytidy_error_collection_select_group_removed`
- `surveytidy_error_collection_rename_group_partial`
- `surveytidy_error_collection_slice_zero`
- `surveytidy_error_collection_pull_incompatible_types`
- `surveytidy_error_collection_glimpse_id_collision`
- `surveytidy_error_collection_verb_unsupported`
- `surveytidy_warning_collection_rowwise_mixed`
- `surveytidy_message_collection_skipped_surveys`

## Dependency changes

- Adds `vctrs (>= 0.6.0)` to `Imports` (used by `pull.survey_collection`
  and `glimpse.survey_collection`).
- Bumps `surveycore` minimum-version pin to `(>= 0.8.2)`.

---

# surveytidy 0.5.0

## New features

### Survey-aware join functions

Eight join and binding functions are now available for combining survey design
objects with plain data frames. All join functions protect design variable
columns (weights, strata, PSU, etc.) from being overwritten by `y` and append
a typed sentinel to `@variables$domain` for traceability by Phase 1 estimation
functions.

* `left_join()` — appends lookup columns from `y` without removing any rows.
  Errors if duplicate keys in `y` would expand rows.

* `inner_join()` — two modes: domain-aware (`.domain_aware = TRUE`, default)
  marks unmatched rows out-of-domain without removing them; physical mode
  (`.domain_aware = FALSE`) removes rows and issues a warning. Physical mode
  is not supported for `survey_twophase` designs.

* `semi_join()` — marks rows matching `y` as in-domain; unmatched rows are
  marked out-of-domain. No rows are physically removed.

* `anti_join()` — inverse of `semi_join()`; marks rows that do NOT match `y`
  as in-domain.

* `right_join()` and `full_join()` — always error when called on a survey
  object. Both would add rows with `NA` design variable values, which would
  invalidate variance estimation.

* `bind_cols()` — appends columns from `...` to a survey object. Validates
  that all inputs have the same row count. Passes through to
  `dplyr::bind_cols()` for non-survey inputs.

* `bind_rows()` — always errors when called with a survey object. Combining
  two survey designs has undefined variance structure. Passes through to
  `dplyr::bind_rows()` for non-survey inputs.

### Row statistic helpers

Two helper functions are now available for computing row-wise statistics
inside `mutate()`. Both propagate metadata automatically and accept `.label`
and `.description` to document the new variable in a single step.

* `row_means()` — computes the row mean across selected columns. Accepts
  `na.rm` to control `NA` handling. Issues a warning if any selected column
  is a design variable.

* `row_sums()` — computes the row sum across selected columns. Accepts
  `na.rm` to control `NA` handling. Issues a warning if any selected column
  is a design variable.

## Bug fixes

* `mutate()` — transformation metadata is now correctly written back to the
  design object. Previously, `[[<-` assignment on S7 properties silently
  failed; the fix uses `S7::prop<-()` to sync all six metadata properties
  correctly (#25).

---

# surveytidy 0.4.0

## New features

### Survey-aware transformation functions

Five vector-level transformation functions are now available for converting,
collapsing, and reversing variables inside `mutate()`. All five propagate value
labels automatically and accept `.label` and `.description` arguments to
attach metadata in a single step.

* `make_factor()` — converts labelled, numeric, character, or factor vectors
  to an R `factor`. Levels are ordered by the numeric value of each value label.
  Accepts `ordered`, `drop_levels`, `force`, and `na.rm` to control level
  creation.

* `make_dicho()` — collapses a multi-level factor to two levels by stripping
  the first word of each label and merging labels that reduce to the same
  stem. Accepts `.exclude` to keep specific levels as `NA`, and `flip_levels`
  to reverse the resulting order.

* `make_binary()` — converts a dichotomous variable to a 0/1 integer. Thin
  wrapper around `make_dicho()`; accepts `flip_values` to control which level
  maps to 1.

* `make_rev()` — reverses a numeric scale using `min + max - x` and remaps
  value labels to match. Issues a warning when all values are `NA`.

* `make_flip()` — reverses the semantic valence of a variable by reversing the
  label strings while keeping the underlying values unchanged. Requires a
  `label` argument to document the new meaning.

---

# surveytidy 0.3.0

## New features

### Survey-aware recoding functions

Six vector-level recoding functions are now available. Each shadows its dplyr
equivalent and adds optional arguments for attaching variable labels, value
labels, and transformation notes directly inside `mutate()`. Without any of
those arguments, output is identical to dplyr.

* `case_when()` — a survey-aware `dplyr::case_when()`. Evaluates a sequence
  of `condition ~ value` formulas and uses the first match for each element.
  Use this to create an entirely new vector from conditions. Accepts `.label`
  to set a variable label, `.value_labels` to attach a named vector of value
  labels, `.factor = TRUE` to return an ordered factor (levels follow formula
  order), and `.description` to record a plain-language note about the
  transformation.

* `if_else()` — a survey-aware `dplyr::if_else()`. Applies a single binary
  condition element-wise (`true`/`false`/`missing`). Stricter than base
  `ifelse()`: `true`, `false`, and `missing` are cast to a common type.
  Accepts `.label`, `.value_labels`, and `.description`.

* `na_if()` — a survey-aware `dplyr::na_if()`. Converts specific values to
  `NA`. Unlike dplyr's scalar-only `y`, this version accepts a vector `y` and
  replaces all matching values in a single call. When the input carries value
  labels, they are inherited automatically; `.update_labels = TRUE` (the
  default) removes label entries for the NA'd values, while
  `.update_labels = FALSE` retains them (useful for documenting what was set
  to missing). Also accepts `.description`.

* `recode_values()` — a survey-aware `dplyr::recode_values()`. Replaces values
  found in `from` with the corresponding value from `to`; values not in `from`
  are kept unchanged or trigger an error (`.unmatched = "error"`). Intended for
  full remapping of every value in a vector. Set `.use_labels = TRUE` to build
  the `from`/`to` map automatically from the input's existing value labels
  (codes become `from`; label strings become `to`). Also accepts `.label`,
  `.value_labels`, `.factor`, and `.description`.

* `replace_values()` — a survey-aware `dplyr::replace_values()`. Replaces
  values found in `from` with the corresponding value from `to`; all other
  values are left unchanged. Use this for partial in-place replacement of
  specific values in an existing vector. Automatically inherits both the
  variable label and value labels from the input; supply `.label` or
  `.value_labels` to override. Also accepts `.description`.

* `replace_when()` — a survey-aware `dplyr::replace_when()`. Like `case_when()`
  but for partial in-place updates: evaluates `condition ~ value` formulas and
  replaces only matching elements, leaving all others at their original value.
  Automatically inherits labels from the input; supply `.label` or
  `.value_labels` to override. Also accepts `.description`.

### Shared label arguments

All six functions support a common set of label arguments that propagate into
`@metadata` when used inside `mutate()`:

* `.label` — a character string stored in `@metadata@variable_labels` as the
  human-readable variable label for the new column.
* `.value_labels` — a named vector stored in `@metadata@value_labels`, where
  names are label strings and values are the corresponding data values.
* `.description` — a plain-language string stored in
  `@metadata@transformations` describing how the variable was derived.

`case_when()` and `recode_values()` also accept `.factor = TRUE`, which
returns an ordered factor instead of a character vector (levels follow formula
or `to` order respectively). `.factor` and `.label` cannot be combined.

### `mutate()` enhancements

`mutate()` now coordinates label propagation automatically: it pre-attaches
label attributes from `@metadata` before the inner dplyr call so recode
functions can see existing labels, reads the label output back from recoded
columns, and writes it into `@metadata` — all without extra user steps. The
weight-column warning has also been split into two distinct classes:
`surveytidy_warning_mutate_weight_col` for the weight column and
`surveytidy_warning_mutate_structural_var` for strata, PSU, FPC, and
replicate weights.

---

# surveytidy 0.2.1

## Website & branding

* Added package hex logo.
* Updated pkgdown site colours to a teal theme.
* `README` now displays the hex logo.
* `LICENSE.md` updated to credit third-party hex sticker icon (Freepik / Flaticon, CC BY 3.0).
* `DESCRIPTION` author entry updated with current email, ORCID, and copyright-holder (`cph`) role.

## Infrastructure

* Added a hotfix-sync check to the CI strategy and `merge-main` skill to detect
  commits on `main` that have not been merged back to `develop`.
* Archived planning documents for the `survey-result` and dedup-rename-rowwise phases.

---

# surveytidy 0.2.0

## New verbs

* `filter_out()` — the complement of `filter()`. Marks rows matching the
  condition as out-of-domain while leaving all other rows in-domain. Like
  `filter()`, no rows are removed. Chains with `filter()` via AND-accumulation
  on the domain column. `filter_out(d, group == "control")` is often clearer
  than `filter(d, group != "control")` for exclusion use-cases.

* `distinct()` — removes duplicate rows while always retaining all columns
  (design variables are never dropped). With no column arguments, deduplicates
  on non-design columns only (survey-safe default). Always issues
  `surveycore_warning_physical_subset`.

* `rename_with()` — function-based column renaming. Applies `.fn` to columns
  selected by `.cols` and propagates renames to `@variables`, `@metadata`,
  `@groups`, and `visible_vars`. Validates `.fn` output and errors with
  `surveytidy_error_rename_fn_bad_output` for non-character, wrong-length, or
  duplicate output.

* `rowwise()` — enables row-by-row computation in `mutate()` (e.g.,
  `max(c_across(...))`). Rowwise state is stored in `@variables$rowwise` —
  never in `@groups`, keeping those clean for estimation functions. `group_by()`
  and `ungroup()` exit rowwise mode, mirroring dplyr behaviour.

## New predicates

* `is_rowwise()` — returns `TRUE` when the survey object is in rowwise mode.
* `is_grouped()` — returns `TRUE` when `@groups` is non-empty.
* `group_vars()` — returns the current grouping column names from `@groups`.

## Verb support for `survey_result` objects

* `filter()`, `arrange()`, `mutate()`, `slice()`, `slice_head()`,
  `slice_tail()`, `slice_min()`, `slice_max()`, `slice_sample()`, and
  `drop_na()` are now registered for `survey_result` objects (the S3 base class
  for surveycore analysis outputs: `survey_means`, `survey_freqs`,
  `survey_totals`, `survey_quantiles`, `survey_corr`, `survey_ratios`).
  Previously, applying dplyr verbs to these objects could silently strip the
  class and `.meta` attribute. Now both are preserved, and `mutate()` keeps
  `meta$group` coherent when `.keep` drops grouping columns.

* `select()`, `rename()`, and `rename_with()` are now registered for
  `survey_result` objects with active `.meta` updates. `select()` prunes stale
  `meta$group` entries when grouping columns are dropped and handles inline
  renames (`select(r, grp = group)`). `rename()` and `rename_with()` propagate
  column renames to all `.meta` key references (`$group`, `$x`,
  `$numerator$name`, `$denominator$name`). `rename_with()` errors with
  `surveytidy_error_rename_fn_bad_output` if `.fn` returns non-character,
  wrong-length, `NA`, or duplicate names.

## Bug fixes

* `drop_na()` now performs domain-aware filtering instead of physically removing
  rows. Previously, `drop_na()` removed rows with `NA` values, changing which
  units contributed to variance estimation and producing incorrect standard
  errors. It now marks incomplete rows as out-of-domain — equivalent to the
  corresponding `filter(!is.na(col1), ...)` chain — giving correct variance
  estimates for downstream analyses.

* `filter()`: the `.by` unsupported-argument error was mis-classified as a
  `surveycore_error_*`; corrected to `surveytidy_error_filter_by_unsupported`.

## Improvements

* `rename()` and `rename_with()` now update `@groups` when a grouped column is
  renamed, and correctly update twophase design variable references
  (`@variables$phase1`, `@variables$phase2`, `@variables$subset`). The domain
  column (`..surveycore_domain..`) is silently protected from renaming.

* `filter()` and `filter_out()` support `if_any()` and `if_all()` in
  conditions.

## Documentation

* Roxygen documentation standardised across all verb files to mirror the
  dplyr/tidyr reference style, with `@details` subsections for
  surveytidy-specific behaviour and examples using `nhanes_2017`.

* Rd files consolidated from per-method (e.g., `arrange.survey_base.Rd`) to
  per-verb (e.g., `arrange.Rd`), fixing the "S3 methods shown with full name"
  R CMD check NOTE.

---

# surveytidy 0.1.0

First release. Implements a complete set of dplyr and tidyr verbs for survey
design objects created with the `surveycore` package.

## New verbs

* `filter()` — domain-aware filtering. Marks rows in-domain rather than
  removing them, preserving correct variance estimation for subpopulation
  analyses. Chained `filter()` calls AND their conditions together.

* `select()` — column selection. Physically removes non-selected columns while
  always retaining design variables (weights, strata, PSU, FPC, replicate
  weights). Sets `@variables$visible_vars` so `print()` hides design columns
  the user did not explicitly request.

* `relocate()` — column reordering. Reorders `visible_vars` when a prior
  `select()` has been called; reorders `@data` directly otherwise.

* `pull()` — extract a column as a plain vector (terminal operation).

* `glimpse()` — concise column summary, respecting `visible_vars`.

* `mutate()` — add or modify columns. Re-attaches design variables dropped by
  `.keep = "none"` or `.keep = "used"`. Issues
  `surveytidy_warning_mutate_design_var` when a mutation's left-hand side names
  a design variable. Respects `@groups` set by `group_by()`.

* `rename()` — rename columns. Automatically keeps `@variables` (design
  specification) and `@metadata` (variable labels, value labels, etc.) in sync
  with the new column names. Issues `surveytidy_warning_rename_design_var` when
  a design variable is renamed.

* `arrange()` — row sorting. The domain column moves correctly with the rows
  after sorting. Supports `.by_group = TRUE` using `@groups`.

* `slice()`, `slice_head()`, `slice_tail()`, `slice_min()`, `slice_max()`,
  `slice_sample()` — physical row selection with a
  `surveycore_warning_physical_subset` warning. `slice_sample(weight_by = )`
  additionally issues `surveytidy_warning_slice_sample_weight_by` to flag that
  the `weight_by` column is independent of the survey design weights.

* `group_by()` — store grouping columns in `@groups`. Does not attach a
  `grouped_df` attribute to `@data`; grouping is kept on the survey object.
  Supports `.add = TRUE` for incremental grouping and computed expressions
  (e.g., `group_by(d, above_median = y1 > median(y1))`).

* `ungroup()` — remove all groups (no arguments) or remove specific columns
  from `@groups` (partial ungroup).

* `drop_na()` — domain-aware NA handling. Marks rows with `NA` in specified
  columns (or any column) as out-of-domain without removing them. Equivalent
  to `filter(!is.na(col1), !is.na(col2), ...)` and gives correct variance
  estimates for downstream analyses. Successive `drop_na()` calls AND their
  conditions together.

* `subset()` — physical row removal with `surveycore_warning_physical_subset`.
  Prefer `filter()` for subpopulation analyses.

## Statistical design

The key design decision in surveytidy is that **`filter()` never removes rows**.
Removing rows from a survey design changes which units contribute to variance
estimation and produces incorrect standard errors for subpopulation statistics.
`filter()` instead writes a logical domain column (`..surveycore_domain..`) to
`@data`. Phase 1 estimation functions will read this column to restrict
calculations to the domain while retaining all rows for variance estimation.

## Infrastructure

* `dplyr_reconstruct.survey_base()` ensures complex dplyr pipelines (joins,
  `across()`, internal slice operations) return survey objects rather than
  plain tibbles. Errors with `surveycore_error_design_var_removed` if a
  pipeline drops a design variable.

* Invariant 6 added to `test_invariants()`: every column name listed in
  `@variables$visible_vars` must exist in `@data`.
