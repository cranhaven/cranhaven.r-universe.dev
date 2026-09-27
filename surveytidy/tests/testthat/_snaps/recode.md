# case_when() error: .label not scalar -> surveytidy_error_recode_label_not_scalar

    Code
      mutate(d, cat = case_when(y1 > 50 ~ "high", .label = c("a", "b")))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `cat = case_when(y1 > 50 ~ "high", .label = c("a", "b"))`.
      Caused by error in `.validate_label_args()`:
      x `.label` must be a single character string.
      i Got <character> of length 2.

# case_when() error: .value_labels unnamed -> surveytidy_error_recode_value_labels_unnamed

    Code
      mutate(d, cat = case_when(y1 > 50 ~ 1L, .value_labels = c(1L, 0L)))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `cat = case_when(y1 > 50 ~ 1L, .value_labels = c(1L, 0L))`.
      Caused by error in `.validate_label_args()`:
      x `.value_labels` must be a named vector.
      i Got an unnamed <integer>.
      v Use `c("Label" = value, ...)` to name the entries.

# case_when() error: .factor = TRUE + .label -> surveytidy_error_recode_factor_with_label

    Code
      mutate(d, cat = case_when(y1 > 50 ~ "high", .factor = TRUE, .label = "bad"))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `cat = case_when(y1 > 50 ~ "high", .factor = TRUE, .label = "bad")`.
      Caused by error in `case_when()`:
      x `.label` cannot be used with `.factor = TRUE`.
      i Factor levels carry their own labels.

# na_if() error: .update_labels not logical -> surveytidy_error_na_if_update_labels_not_scalar

    Code
      mutate(d, y3_na = na_if(y3, 0L, .update_labels = "yes"))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `y3_na = na_if(y3, 0L, .update_labels = "yes")`.
      Caused by error in `na_if()`:
      x `.update_labels` must be a single <logical> value.
      i Got <character> of length 1.

# recode_values() .use_labels = TRUE combined with formulas errors

    Code
      mutate(d, y3_r = recode_values(y3, 0L ~ "no", .use_labels = TRUE))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `y3_r = recode_values(y3, 0L ~ "no", .use_labels = TRUE)`.
      Caused by error in `recode_values()`:
      x `.use_labels = TRUE` cannot be combined with formulas in `...`.
      i Both describe the `from`/`to` map.
      v Remove the formulas, or set `.use_labels = FALSE` and let the formulas define the map.

# error snapshots for all recode error classes

    Code
      mutate(d, cat = case_when(y1 > 50 ~ "high", .label = c("a", "b")))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `cat = case_when(y1 > 50 ~ "high", .label = c("a", "b"))`.
      Caused by error in `.validate_label_args()`:
      x `.label` must be a single character string.
      i Got <character> of length 2.

---

    Code
      mutate(d, cat = case_when(y1 > 50 ~ 1L, .value_labels = c(1L, 0L)))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `cat = case_when(y1 > 50 ~ 1L, .value_labels = c(1L, 0L))`.
      Caused by error in `.validate_label_args()`:
      x `.value_labels` must be a named vector.
      i Got an unnamed <integer>.
      v Use `c("Label" = value, ...)` to name the entries.

---

    Code
      mutate(d, cat = case_when(y1 > 50 ~ "hi", .factor = TRUE, .label = "x"))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `cat = case_when(y1 > 50 ~ "hi", .factor = TRUE, .label = "x")`.
      Caused by error in `case_when()`:
      x `.label` cannot be used with `.factor = TRUE`.
      i Factor levels carry their own labels.

---

    Code
      mutate(d, cat = recode_values(y3, from = 1L, to = 2L, .use_labels = TRUE))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `cat = recode_values(y3, from = 1L, to = 2L, .use_labels = TRUE)`.
      Caused by error in `recode_values()`:
      x `x` has no value labels.
      i `.use_labels = TRUE` requires `x` to carry value labels.
      v Provide `from` and `to` explicitly instead.

---

    Code
      mutate(d, cat = recode_values(y3, .use_labels = FALSE))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `cat = recode_values(y3, .use_labels = FALSE)`.
      Caused by error in `recode_values()`:
      x No recoding map supplied.
      v Pass `old ~ new` formulas in `...`, supply `from` and `to`, or set `.use_labels = TRUE`.

---

    Code
      mutate(d, cat = recode_values(y3, from = 99L, to = "other", .unmatched = "error"))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `cat = recode_values(y3, from = 99L, to = "other", .unmatched = "error")`.
      Caused by error in `recode_values()`:
      x Some values in `x` were not found in `from`.
      i Set `.unmatched = "default"` to keep unmatched values.
      Caused by error in `dplyr::recode_values()`:
      ! Each location must be matched.
      x Locations 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, ..., 99, and 100 are unmatched.

---

    Code
      mutate(d, cat = case_when(y1 > 50 ~ "high", .description = c("a", "b")))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `cat = case_when(y1 > 50 ~ "high", .description = c("a", "b"))`.
      Caused by error in `.validate_label_args()`:
      x `.description` must be a single character string.
      i Got <character> of length 2.

# mutate() warns surveytidy_warning_mutate_structural_var when mutating strata [taylor]

    Code
      mutate(d, strata = paste0(strata, "_mod"))
    Condition
      Warning:
      ! mutate() modified structural design variable(s): strata.
      i Structural recoding can invalidate variance estimates.
      i Use `subset()` or `filter()` to restrict the domain; do not recode design variables.
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_taylor> (Taylor series linearization)
      Sample size: 100
      
    Output
      # A tibble: 100 x 8
         psu   strata          fpc    wt    y1      y2    y3 group
         <chr> <chr>         <dbl> <dbl> <dbl>   <dbl> <int> <chr>
       1 psu_1 stratum_1_mod   742  14.3  48.8 -1.05       0 C    
       2 psu_1 stratum_1_mod   742  21.8  51.9 -0.646      0 A    
       3 psu_1 stratum_1_mod   742  14.4  51.2 -0.185      1 C    
       4 psu_1 stratum_1_mod   742  18.9  49.7 -1.20       1 C    
       5 psu_1 stratum_1_mod   742  23.0  51.1  2.04       0 A    
       6 psu_1 stratum_1_mod   742  11.0  45.1  0.108      0 C    
       7 psu_2 stratum_1_mod   742  13.8  45.0 -0.0841     0 A    
       8 psu_2 stratum_1_mod   742  14.2  33.4  0.496      0 C    
       9 psu_2 stratum_1_mod   742  16.5  46.2  0.0374     1 B    
      10 psu_2 stratum_1_mod   742  13.7  44.9 -0.132      0 C    
      # i 90 more rows

