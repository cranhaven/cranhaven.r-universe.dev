# as_caldata() errors on empty base_weights (n = 0, R-6)

    Code
      as_caldata(numeric(0), numeric(0), matrix(0, 0, 1))
    Condition
      Error in `as_caldata()`:
      x `base_weights` must be strictly positive.
      i All values must be > 0. Got 0 non-positive values.
      v Ensure all sampling weights are positive before calibration.

# as_caldata() rejects non-positive base_weights

    Code
      as_caldata(bw_bad, g, mm)
    Condition
      Error in `as_caldata()`:
      x `base_weights` must be strictly positive.
      i All values must be > 0. Got 1 non-positive value.
      v Ensure all sampling weights are positive before calibration.

# as_caldata() rejects NA in base_weights (B-4)

    Code
      as_caldata(bw_na, g, mm)
    Condition
      Error in `as_caldata()`:
      x `base_weights` contains non-finite values (1 value).
      i `base_weights` must be a fully finite numeric vector. Check for `NA`, `NaN`, or `Inf` values.
      v Remove or impute non-finite values before calling `as_caldata()`.

# as_caldata() rejects Inf in base_weights (B-4)

    Code
      as_caldata(bw_inf, g, mm)
    Condition
      Error in `as_caldata()`:
      x `base_weights` contains non-finite values (1 value).
      i `base_weights` must be a fully finite numeric vector. Check for `NA`, `NaN`, or `Inf` values.
      v Remove or impute non-finite values before calling `as_caldata()`.

# as_caldata() rejects non-positive g_weights

    Code
      as_caldata(base_w, g_bad, mm)
    Condition
      Error in `as_caldata()`:
      x `g_weights` must be strictly positive.
      i All g-factors must be > 0. Got 1 non-positive value.
      v Ensure all g-factors are positive. A g-factor of 1.0 means no adjustment.

# as_caldata() rejects NA in g_weights (B-4)

    Code
      as_caldata(base_w, g_na, mm)
    Condition
      Error in `as_caldata()`:
      x `g_weights` contains non-finite values.
      i `g_weights` must be a fully finite numeric vector. Check for `NA`, `NaN`, or `Inf` values.
      v Remove or impute non-finite values before calling `as_caldata()`.

# as_caldata() rejects g_weights of wrong length (B-3)

    Code
      as_caldata(base_w, g_short, mm)
    Condition
      Error in `as_caldata()`:
      x `g_weights` length (5) must equal `base_weights` length (10).
      i Each observation must have exactly one base weight and one g-factor. Supply vectors of equal length.

# as_caldata() rejects near-zero g_weights * sqrt(base_weights)

    Code
      as_caldata(base_w, g_tiny, mm)
    Condition
      Error in `as_caldata()`:
      x `g_weights` * sqrt(`base_weights`) contains near-zero values (10 values).
      i The product `g_weights` * sqrt(`base_weights`) must be above `.Machine$double.eps^0.5` for all observations.
      v Review g-factors; values near 0 indicate near-zero calibrated weights.

# as_caldata() rejects model_matrix with wrong number of rows

    Code
      as_caldata(base_w, g, mm_wrong)
    Condition
      Error in `as_caldata()`:
      x `model_matrix` has 5 rows but `base_weights` has length 10.
      i The number of rows in `model_matrix` must equal the number of observations (length of `base_weights`).

# as_caldata() rejects model_matrix with 0 columns

    Code
      as_caldata(base_w, g, mm_empty)
    Condition
      Error in `as_caldata()`:
      x `model_matrix` must have at least 1 column.
      i A calibration model requires at least one covariate column. An intercept-only model uses a column of all 1s.
      v Use `matrix(1, nrow = n, ncol = 1)` for an intercept-only calibration model.

# as_caldata() rejects model_matrix with NA values

    Code
      as_caldata(base_w, g, mm_na)
    Condition
      Error in `as_caldata()`:
      x `model_matrix` contains non-finite values.
      i `model_matrix` must contain only finite numeric values. Check for `NA`, `NaN`, or `Inf` in the matrix.
      v Impute or remove non-finite entries before calling `as_caldata()`.

# as_caldata() rejects model_matrix with Inf values

    Code
      as_caldata(base_w, g, mm_inf)
    Condition
      Error in `as_caldata()`:
      x `model_matrix` contains non-finite values.
      i `model_matrix` must contain only finite numeric values. Check for `NA`, `NaN`, or `Inf` in the matrix.
      v Impute or remove non-finite entries before calling `as_caldata()`.

# .apply_caldata_projection() errors when any caldata entry has stage != 0L [direct]

    Code
      surveycore:::.apply_caldata_projection(u, list(cd_bad))
    Condition
      Error in `surveycore:::.apply_caldata_projection()`:
      x Within-PSU calibration (stage != 0) is not supported in v1.
      i 1 caldata element has stage != 0L.
      v Use population-level calibration only.

# .apply_caldata_projection() errors on dimension mismatch between u and cd$w [direct]

    Code
      surveycore:::.apply_caldata_projection(u_10, list(cd_5))
    Condition
      Error in `surveycore:::.apply_caldata_projection()`:
      x Calibration projection dimension mismatch.
      i Linearization vector has 10 rows; caldata `w` has length 5.

# .apply_caldata_projection() errors on NULL element in caldata list [direct, B-5]

    Code
      surveycore:::.apply_caldata_projection(u, list(cd, NULL))
    Condition
      Error in `surveycore:::.apply_caldata_projection()`:
      x caldata element(s) 2 are NULL.
      i All @calibration list elements must be constructed via `as_caldata()`.
      v Inspect `design@calibration` for NULL entries.

# .validate_calibration_arg() errors on numeric vector (CAL-15) [direct]

    Code
      surveycore:::.validate_calibration_arg(c(1, 2, 3), 10L)
    Condition
      Error in `surveycore:::.validate_calibration_arg()`:
      x `calibration` must be a list of `as_caldata()` outputs or `NULL`.
      i Got <numeric>.

# .validate_calibration_arg() errors on data frame (CAL-15) [direct]

    Code
      surveycore:::.validate_calibration_arg(data.frame(x = 1:3), 10L)
    Condition
      Error in `surveycore:::.validate_calibration_arg()`:
      x `calibration` must be a list of `as_caldata()` outputs or `NULL`.
      i Got <data.frame>.

# .validate_calibration_arg() errors on bare caldata (not wrapped) (CAL-15) [direct]

    Code
      surveycore:::.validate_calibration_arg(bare_cd, 10L)
    Condition
      Error in `surveycore:::.validate_calibration_arg()`:
      x `calibration` must be a list of `as_caldata()` outputs or `NULL`.
      i Got <list>. It looks like you passed the result of `as_caldata()` directly -- wrap it in `list()`: `calibration = list(cd)`.

# .validate_calibration_arg() errors on NULL element in list (CAL-16) [direct]

    Code
      surveycore:::.validate_calibration_arg(list(cd, NULL), 10L)
    Condition
      Error in `surveycore:::.validate_calibration_arg()`:
      x Element 2 of `calibration` is not a valid caldata object.
      i Each element must be a named list with fields "qr", "w", "stage", and "index", produced by `as_caldata()`.

# .validate_calibration_arg() errors on element missing index field (CAL-16) [direct]

    Code
      surveycore:::.validate_calibration_arg(list(cd_bad), 10L)
    Condition
      Error in `surveycore:::.validate_calibration_arg()`:
      x Element 1 of `calibration` is not a valid caldata object.
      i Each element must be a named list with fields "qr", "w", "stage", and "index", produced by `as_caldata()`.

# .validate_calibration_arg() errors when length(cd$w) != nrow_data (CAL-16) [direct]

    Code
      surveycore:::.validate_calibration_arg(list(cd), 5L)
    Condition
      Error in `surveycore:::.validate_calibration_arg()`:
      x Element 1 of `calibration` is not a valid caldata object.
      i Each element must be a named list with fields "qr", "w", "stage", and "index", produced by `as_caldata()`.

# as_survey() errors on bare caldata (not wrapped) (CAL-15)

    Code
      as_survey(df, weights = wt, calibration = bare_cd)
    Condition
      Error in `.validate_calibration_arg()`:
      x `calibration` must be a list of `as_caldata()` outputs or `NULL`.
      i Got <list>. It looks like you passed the result of `as_caldata()` directly -- wrap it in `list()`: `calibration = list(cd)`.

# as_survey() errors on numeric vector calibration (CAL-15)

    Code
      as_survey(df, weights = wt, calibration = c(1, 2, 3))
    Condition
      Error in `.validate_calibration_arg()`:
      x `calibration` must be a list of `as_caldata()` outputs or `NULL`.
      i Got <numeric>.

# as_survey() errors on data frame calibration (CAL-15)

    Code
      as_survey(df, weights = wt, calibration = data.frame(x = 1:3))
    Condition
      Error in `.validate_calibration_arg()`:
      x `calibration` must be a list of `as_caldata()` outputs or `NULL`.
      i Got <data.frame>.

# as_survey() errors on element missing index field (CAL-16)

    Code
      as_survey(df, weights = wt, calibration = list(cd_bad))
    Condition
      Error in `.validate_calibration_arg()`:
      x Element 1 of `calibration` is not a valid caldata object.
      i Each element must be a named list with fields "qr", "w", "stage", and "index", produced by `as_caldata()`.

# as_survey() errors on NULL element in calibration list (CAL-16)

    Code
      as_survey(df, weights = wt, calibration = list(cd, NULL))
    Condition
      Error in `.validate_calibration_arg()`:
      x Element 2 of `calibration` is not a valid caldata object.
      i Each element must be a named list with fields "qr", "w", "stage", and "index", produced by `as_caldata()`.

# as_survey() errors when length(cd$w) != nrow(data) (CAL-16)

    Code
      as_survey(df, weights = wt, calibration = list(cd_wrong))
    Condition
      Error in `.validate_calibration_arg()`:
      x Element 1 of `calibration` is not a valid caldata object.
      i Each element must be a named list with fields "qr", "w", "stage", and "index", produced by `as_caldata()`.

# as_survey() errors on second bad element in two-element list (CAL-16)

    Code
      as_survey(df, weights = wt, calibration = list(cd_good, cd_bad))
    Condition
      Error in `.validate_calibration_arg()`:
      x Element 2 of `calibration` is not a valid caldata object.
      i Each element must be a named list with fields "qr", "w", "stage", and "index", produced by `as_caldata()`.

# as_survey_replicate() errors on non-list calibration (CAL-15)

    Code
      as_survey_replicate(df, weights = wt, repweights = tidyselect::all_of(
        repwt_cols), type = "BRR", calibration = c(1, 2, 3))
    Condition
      Error in `.validate_calibration_arg()`:
      x `calibration` must be a list of `as_caldata()` outputs or `NULL`.
      i Got <numeric>.

# as_survey_replicate() errors on invalid caldata structure (CAL-16)

    Code
      as_survey_replicate(df, weights = wt, repweights = tidyselect::all_of(
        repwt_cols), type = "BRR", calibration = list(cd_bad))
    Condition
      Error in `.validate_calibration_arg()`:
      x Element 1 of `calibration` is not a valid caldata object.
      i Each element must be a named list with fields "qr", "w", "stage", and "index", produced by `as_caldata()`.

# as_survey_replicate() errors when length(cd$w) != nrow(data) (CAL-16)

    Code
      as_survey_replicate(df, weights = wt, repweights = tidyselect::all_of(
        repwt_cols), type = "BRR", calibration = list(cd_wrong))
    Condition
      Error in `.validate_calibration_arg()`:
      x Element 1 of `calibration` is not a valid caldata object.
      i Each element must be a named list with fields "qr", "w", "stage", and "index", produced by `as_caldata()`.

