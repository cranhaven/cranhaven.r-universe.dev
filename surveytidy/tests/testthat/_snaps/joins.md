# left_join() errors when y has duplicate keys that would expand rows

    Code
      suppressWarnings(dplyr::left_join(d, y_dup, by = "group"))
    Condition
      Error in `.check_join_row_expansion()`:
      x The join would expand `x` from 100 to 128 rows because `y` has duplicate keys.
      i Duplicate respondent rows corrupt variance estimation.
      v Use `dplyr::distinct()` to deduplicate `y` before joining.

# left_join() errors when y is a survey object

    Code
      dplyr::left_join(d, d2, by = "group")
    Condition
      Error in `.check_join_y_type()`:
      x `y` is a survey design object, not a data frame.
      i Joining two survey objects requires manual reconciliation of design specifications.
      v Extract `y@data` to join the underlying data, then re-specify the design with `surveycore::as_survey()`.

# semi_join() errors when y is a survey object

    Code
      dplyr::semi_join(d, d2, by = "group")
    Condition
      Error in `.check_join_y_type()`:
      x `y` is a survey design object, not a data frame.
      i Joining two survey objects requires manual reconciliation of design specifications.
      v Extract `y@data` to join the underlying data, then re-specify the design with `surveycore::as_survey()`.

# semi_join() errors when x@data contains the reserved row index column

    Code
      dplyr::semi_join(d, lookup, by = "group")
    Condition
      Error in `dplyr::semi_join()`:
      x `x@data` contains a reserved internal column ..surveytidy_row_index.. that conflicts with masking logic.
      i This column name is reserved for internal use by surveytidy.
      v Rename the column in your data before passing it to `semi_join()` or `anti_join()`.

# anti_join() errors when x@data contains the reserved row index column

    Code
      dplyr::anti_join(d, lookup, by = "group")
    Condition
      Error in `dplyr::anti_join()`:
      x `x@data` contains a reserved internal column ..surveytidy_row_index.. that conflicts with masking logic.
      i This column name is reserved for internal use by surveytidy.
      v Rename the column in your data before passing it to `semi_join()` or `anti_join()`.

# anti_join() errors when y is a survey object

    Code
      dplyr::anti_join(d, d2, by = "group")
    Condition
      Error in `.check_join_y_type()`:
      x `y` is a survey design object, not a data frame.
      i Joining two survey objects requires manual reconciliation of design specifications.
      v Extract `y@data` to join the underlying data, then re-specify the design with `surveycore::as_survey()`.

# bind_cols() errors when row counts differ

    Code
      bind_cols(d, extra)
    Condition
      Error in `bind_cols.survey_base()`:
      x `bind_cols()` requires all inputs to have the same number of rows.
      i `x` has 100 rows; the new data has 99 rows.
      v Ensure the data frame you are binding has exactly 100 rows before calling `bind_cols()`.

# bind_cols() errors when ... contains a survey object

    Code
      bind_cols(d, d2)
    Condition
      Error in `bind_cols.survey_base()`:
      x Survey objects cannot be combined with `bind_cols()`.
      i One or more objects in `...` is a survey design object.
      v Extract `@data` from each survey object and bind the raw data frames instead.

# inner_join() domain-aware errors on duplicate keys in y

    Code
      suppressWarnings(dplyr::inner_join(d, y_dup, by = "group"))
    Condition
      Error in `.check_join_row_expansion()`:
      x The join would expand `x` from 100 to 128 rows because `y` has duplicate keys.
      i Duplicate respondent rows corrupt variance estimation.
      v Use `dplyr::distinct()` to deduplicate `y` before joining.

# inner_join() domain-aware errors on reserved column name in x@data

    Code
      dplyr::inner_join(d, lookup, by = "group")
    Condition
      Error in `dplyr::inner_join()`:
      x `x@data` contains a reserved internal column ..surveytidy_row_index.. that conflicts with masking logic.
      i This column name is reserved for internal use by surveytidy.
      v Rename the column in your data before passing it to `inner_join()`.

# inner_join(.domain_aware=FALSE) errors for twophase designs

    Code
      dplyr::inner_join(d, lookup, by = "group", .domain_aware = FALSE)
    Condition
      Error in `dplyr::inner_join()`:
      x `inner_join(.domain_aware = FALSE)` cannot physically remove rows from a two-phase design.
      i Removing rows from a two-phase design can orphan phase 2 rows or corrupt the phase 1 sample frame.
      v Use `.domain_aware = TRUE` (the default) or `semi_join()` for domain-aware filtering.

# inner_join(.domain_aware=FALSE) errors when all rows are removed

    Code
      suppressWarnings(dplyr::inner_join(d, y_no_match, by = "group", .domain_aware = FALSE))
    Condition
      Error in `dplyr::inner_join()`:
      x inner_join() condition matched 0 rows.
      i Survey objects require at least 1 row.
      v Use `semi_join()` for domain estimation (keeps all rows).

# inner_join(.domain_aware=FALSE) errors on duplicate keys in y

    Code
      suppressWarnings(dplyr::inner_join(d, y_dup, by = "group", .domain_aware = FALSE))
    Condition
      Error in `.check_join_row_expansion()`:
      x The join would expand `x` from 100 to 200 rows because `y` has duplicate keys.
      i Duplicate respondent rows corrupt variance estimation.
      v Use `dplyr::distinct()` to deduplicate `y` before joining.

# inner_join() errors when y is a survey object (both modes)

    Code
      dplyr::inner_join(d, d2, by = "group")
    Condition
      Error in `.check_join_y_type()`:
      x `y` is a survey design object, not a data frame.
      i Joining two survey objects requires manual reconciliation of design specifications.
      v Extract `y@data` to join the underlying data, then re-specify the design with `surveycore::as_survey()`.

# right_join() always errors for survey objects

    Code
      dplyr::right_join(d, lookup, by = "group")
    Condition
      Error in `dplyr::right_join()`:
      x `right_join()` would add rows from `y` that have no match in the survey.
      i New rows would have `NA` for all design variables (weights, strata, PSU), producing an invalid design object.
      v Use `left_join()` to add lookup columns from `y`, or `filter()` / `semi_join()` to restrict the survey domain.

# full_join() always errors for survey objects

    Code
      dplyr::full_join(d, lookup, by = "group")
    Condition
      Error in `dplyr::full_join()`:
      x `full_join()` would add rows from `y` that have no match in the survey.
      i New rows would have `NA` for all design variables (weights, strata, PSU), producing an invalid design object.
      v Use `left_join()` to add lookup columns from `y`, or `filter()` / `semi_join()` to restrict the survey domain.

# bind_rows() always errors when x is a survey object

    Code
      bind_rows(d, extra)
    Condition
      Error in `bind_rows()`:
      x `bind_rows()` cannot stack survey design objects.
      i Stacking two surveys changes the design — the combined object requires a new design specification.
      v Extract `@data` from each survey object with `surveycore::survey_data()`, bind the raw data frames with `dplyr::bind_rows()`, then re-specify the combined design with `surveycore::as_survey()`.

# All join functions error when y is also a survey object

    Code
      dplyr::left_join(d1, d2, by = "group")
    Condition
      Error in `.check_join_y_type()`:
      x `y` is a survey design object, not a data frame.
      i Joining two survey objects requires manual reconciliation of design specifications.
      v Extract `y@data` to join the underlying data, then re-specify the design with `surveycore::as_survey()`.

---

    Code
      dplyr::semi_join(d1, d2, by = "group")
    Condition
      Error in `.check_join_y_type()`:
      x `y` is a survey design object, not a data frame.
      i Joining two survey objects requires manual reconciliation of design specifications.
      v Extract `y@data` to join the underlying data, then re-specify the design with `surveycore::as_survey()`.

---

    Code
      dplyr::anti_join(d1, d2, by = "group")
    Condition
      Error in `.check_join_y_type()`:
      x `y` is a survey design object, not a data frame.
      i Joining two survey objects requires manual reconciliation of design specifications.
      v Extract `y@data` to join the underlying data, then re-specify the design with `surveycore::as_survey()`.

---

    Code
      bind_cols(d1, d2)
    Condition
      Error in `bind_cols.survey_base()`:
      x Survey objects cannot be combined with `bind_cols()`.
      i One or more objects in `...` is a survey design object.
      v Extract `@data` from each survey object and bind the raw data frames instead.

---

    Code
      dplyr::right_join(d1, d2, by = "group")
    Condition
      Error in `.check_join_y_type()`:
      x `y` is a survey design object, not a data frame.
      i Joining two survey objects requires manual reconciliation of design specifications.
      v Extract `y@data` to join the underlying data, then re-specify the design with `surveycore::as_survey()`.

---

    Code
      dplyr::full_join(d1, d2, by = "group")
    Condition
      Error in `.check_join_y_type()`:
      x `y` is a survey design object, not a data frame.
      i Joining two survey objects requires manual reconciliation of design specifications.
      v Extract `y@data` to join the underlying data, then re-specify the design with `surveycore::as_survey()`.

