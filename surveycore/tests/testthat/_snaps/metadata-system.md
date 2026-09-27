# snapshot: extract_var_label() surveycore_error_format_invalid

    Code
      extract_var_label(d, format = "tibble")
    Condition
      Error:
      x `extract_var_label()` received an invalid `format` value "tibble".
      i `format` must be one of "named_vector", "list", and "data_frame".

# snapshot: set_var_label() surveycore_error_not_survey_or_df

    Code
      set_var_label(list(x = 1), age = "A")
    Condition
      Error:
      x `x` must be a survey design object or a data frame, not <list>.
      v Create a survey object with `as_survey()`, `as_survey_replicate()`, or `as_survey_twophase()`.

# snapshot: set_var_label() surveycore_error_setter_ambiguous

    Code
      set_var_label(d, age = "A", variable = "income")
    Condition
      Error:
      x Provide variable names via `...` or via `variable`, not both.
      i Use named `...` args, a named vector in `...`, or `variable` + `label` — not a mix.

# snapshot: set_var_label() surveycore_error_setter_empty

    Code
      set_var_label(d)
    Condition
      Error:
      x `set_var_label()` requires at least one variable-label pair.
      v Use named `...` args: `set_var_label(x, age = 'Age in years')`.

# snapshot: set_var_label() surveycore_error_setter_mismatched_lengths

    Code
      set_var_label(d, variable = c("age", "income"), label = "Age")
    Condition
      Error:
      x `variable` has 2 elements but `label` has 1 element.
      i They must be the same length (one content value per variable name).

# snapshot: set_var_label() surveycore_error_old_positional_setter

    Code
      set_var_label(d, age, "Age in years")
    Condition
      Error:
      x The old positional calling form `set_var_label(x, var, content)` is no longer supported.
      i The new unified setter uses named arguments.
      v Use `set_var_label(x, age = "Age in years")` instead.

# snapshot: set_var_label() surveycore_error_label_not_scalar

    Code
      set_var_label(d, age = 123L)
    Condition
      Error:
      x Label content for age must be a character scalar, not <integer> of length 1.
      v Pass a single character string, e.g. `set_var_label(x, age = 'My label')`.

# snapshot: set_var_label() surveycore_warning_var_not_found

    Code
      set_var_label(d, zzz_missing = "Label")
    Condition
      Warning:
      ! Variable zzz_missing not found in `x` and was skipped.

# snapshot: set_var_label() surveycore_warning_setter_empty_variables

    Code
      set_var_label(d, variable = character(0))
    Condition
      Warning:
      ! `set_var_label()` was called with `variable` of length 0.
      i No metadata was set. Did you accidentally filter all variable names out?

# snapshot: set_val_labels() surveycore_error_labels_unnamed

    Code
      set_val_labels(d, sex = c(1L, 2L))
    Condition
      Error:
      x `labels` must be a fully named vector.
      i All elements must have names.

# snapshot: set_val_labels() surveycore_warning_missing_labels

    Code
      set_val_labels(d, sex = c(Male = 1L))
    Condition
      Warning:
      ! Not all values of sex are labeled.
      i Unlabeled values: "2"

# snapshot: set_val_labels() surveycore_error_not_survey_or_df

    Code
      set_val_labels(list(x = 1), sex = c(Male = 1L))
    Condition
      Error:
      x `x` must be a survey design object or a data frame, not <list>.
      v Create a survey object with `as_survey()`, `as_survey_replicate()`, or `as_survey_twophase()`.

# snapshot: set_val_labels() surveycore_error_setter_empty

    Code
      set_val_labels(d)
    Condition
      Error:
      x `set_val_labels()` requires at least one variable-label pair.
      v Use named `...` args: `set_val_labels(x, age = 'Age in years')`.

# snapshot: set_val_labels() surveycore_error_setter_ambiguous

    Code
      set_val_labels(d, sex = c(Male = 1L), variable = "age")
    Condition
      Error:
      x Provide variable names via `...` or via `variable`, not both.
      i Use named `...` args, a named vector in `...`, or `variable` + `labels` — not a mix.

# snapshot: set_val_labels() surveycore_error_setter_mismatched_lengths

    Code
      set_val_labels(d, variable = c("sex", "age"), labels = list(c(Male = 1L)))
    Condition
      Error:
      x `variable` has 2 elements but `labels` has 1 element.
      i They must be the same length (one content value per variable name).

# snapshot: set_val_labels() surveycore_warning_var_not_found

    Code
      set_val_labels(d, zzz_missing = c(A = 1L))
    Condition
      Warning:
      ! Variable zzz_missing not found in `x` and was skipped.

# snapshot: set_val_labels() surveycore_warning_setter_empty_variables

    Code
      set_val_labels(d, variable = character(0))
    Condition
      Warning:
      ! `set_val_labels()` was called with `variable` of length 0.
      i No metadata was set. Did you accidentally filter all variable names out?

# snapshot: set_question_preface() surveycore_error_not_survey_or_df

    Code
      set_question_preface(list(x = 1), age = "Q text")
    Condition
      Error:
      x `x` must be a survey design object or a data frame, not <list>.
      v Create a survey object with `as_survey()`, `as_survey_replicate()`, or `as_survey_twophase()`.

# snapshot: set_question_preface() surveycore_error_label_not_scalar

    Code
      set_question_preface(d, age = 123L)
    Condition
      Error:
      x Label content for age must be a character scalar, not <integer> of length 1.
      v Pass a single character string, e.g. `set_var_label(x, age = 'My label')`.

# snapshot: set_question_preface() surveycore_error_setter_ambiguous

    Code
      set_question_preface(d, age = "Q text", variable = "income")
    Condition
      Error:
      x Provide variable names via `...` or via `variable`, not both.
      i Use named `...` args, a named vector in `...`, or `variable` + `preface` — not a mix.

# snapshot: set_question_preface() surveycore_error_setter_empty

    Code
      set_question_preface(d)
    Condition
      Error:
      x `set_question_preface()` requires at least one variable-label pair.
      v Use named `...` args: `set_question_preface(x, age = 'Age in years')`.

# snapshot: set_question_preface() surveycore_error_setter_mismatched_lengths

    Code
      set_question_preface(d, variable = c("age", "income"), preface = "Q")
    Condition
      Error:
      x `variable` has 2 elements but `preface` has 1 element.
      i They must be the same length (one content value per variable name).

# snapshot: set_question_preface() surveycore_warning_var_not_found

    Code
      set_question_preface(d, zzz_missing = "Q text")
    Condition
      Warning:
      ! Variable zzz_missing not found in `x` and was skipped.

# snapshot: set_question_preface() surveycore_warning_setter_empty_variables

    Code
      set_question_preface(d, variable = character(0))
    Condition
      Warning:
      ! `set_question_preface()` was called with `variable` of length 0.
      i No metadata was set. Did you accidentally filter all variable names out?

# snapshot: set_var_note() surveycore_error_not_survey_or_df

    Code
      set_var_note(list(x = 1), age = "A note")
    Condition
      Error:
      x `x` must be a survey design object or a data frame, not <list>.
      v Create a survey object with `as_survey()`, `as_survey_replicate()`, or `as_survey_twophase()`.

# snapshot: set_var_note() surveycore_error_setter_ambiguous

    Code
      set_var_note(d, age = "A note", variable = "income")
    Condition
      Error:
      x Provide variable names via `...` or via `variable`, not both.
      i Use named `...` args, a named vector in `...`, or `variable` + `note` — not a mix.

# snapshot: set_var_note() surveycore_error_setter_empty

    Code
      set_var_note(d)
    Condition
      Error:
      x `set_var_note()` requires at least one variable-label pair.
      v Use named `...` args: `set_var_note(x, age = 'Age in years')`.

# snapshot: set_var_note() surveycore_error_setter_mismatched_lengths

    Code
      set_var_note(d, variable = c("age", "income"), note = "A note")
    Condition
      Error:
      x `variable` has 2 elements but `note` has 1 element.
      i They must be the same length (one content value per variable name).

# snapshot: set_var_note() surveycore_error_label_not_scalar

    Code
      set_var_note(d, age = 123L)
    Condition
      Error:
      x Label content for age must be a character scalar, not <integer> of length 1.
      v Pass a single character string, e.g. `set_var_label(x, age = 'My label')`.

# snapshot: set_var_note() surveycore_warning_var_not_found

    Code
      set_var_note(d, zzz_missing = "Some note")
    Condition
      Warning:
      ! Variable zzz_missing not found in `x` and was skipped.

# snapshot: set_var_note() surveycore_warning_setter_empty_variables

    Code
      set_var_note(d, variable = character(0))
    Condition
      Warning:
      ! `set_var_note()` was called with `variable` of length 0.
      i No metadata was set. Did you accidentally filter all variable names out?

# snapshot: set_universe() surveycore_error_not_survey_or_df

    Code
      set_universe(list(x = 1), age = "Adults 18+")
    Condition
      Error:
      x `x` must be a survey design object or a data frame, not <list>.
      v Create a survey object with `as_survey()`, `as_survey_replicate()`, or `as_survey_twophase()`.

# snapshot: set_universe() surveycore_error_label_not_scalar

    Code
      set_universe(d, age = 123L)
    Condition
      Error:
      x Label content for age must be a character scalar, not <integer> of length 1.
      v Pass a single character string, e.g. `set_var_label(x, age = 'My label')`.

# snapshot: set_universe() surveycore_error_setter_ambiguous

    Code
      set_universe(d, age = "Adults 18+", variable = "income")
    Condition
      Error:
      x Provide variable names via `...` or via `variable`, not both.
      i Use named `...` args, a named vector in `...`, or `variable` + `universe` — not a mix.

# snapshot: set_universe() surveycore_error_setter_empty

    Code
      set_universe(d)
    Condition
      Error:
      x `set_universe()` requires at least one variable-label pair.
      v Use named `...` args: `set_universe(x, age = 'Age in years')`.

# snapshot: set_universe() surveycore_error_setter_mismatched_lengths

    Code
      set_universe(d, variable = c("age", "income"), universe = "Adults 18+")
    Condition
      Error:
      x `variable` has 2 elements but `universe` has 1 element.
      i They must be the same length (one content value per variable name).

# snapshot: set_universe() surveycore_warning_var_not_found

    Code
      set_universe(d, zzz_missing = "Some universe")
    Condition
      Warning:
      ! Variable zzz_missing not found in `x` and was skipped.

# snapshot: set_universe() surveycore_warning_setter_empty_variables

    Code
      set_universe(d, variable = character(0))
    Condition
      Warning:
      ! `set_universe()` was called with `variable` of length 0.
      i No metadata was set. Did you accidentally filter all variable names out?

# snapshot: set_missing_codes() surveycore_error_missing_codes_not_vector

    Code
      set_missing_codes(d, age = list("bad"))
    Condition
      Error:
      x Missing codes for age must be an atomic vector, not a list.
      i Got class <list>.
      v Use `set_missing_codes(x, age = c(Missing = -1L))` instead.

# snapshot: set_missing_codes() surveycore_error_not_survey_or_df

    Code
      set_missing_codes(list(x = 1), age = c(Missing = -1L))
    Condition
      Error:
      x `x` must be a survey design object or a data frame, not <list>.
      v Create a survey object with `as_survey()`, `as_survey_replicate()`, or `as_survey_twophase()`.

# snapshot: set_missing_codes() surveycore_error_setter_ambiguous

    Code
      set_missing_codes(d, age = c(Missing = -1L), variable = "income")
    Condition
      Error:
      x Provide variable names via `...` or via `variable`, not both.
      i Use named `...` args, a named vector in `...`, or `variable` + `codes` — not a mix.

# snapshot: set_missing_codes() surveycore_error_setter_empty

    Code
      set_missing_codes(d)
    Condition
      Error:
      x `set_missing_codes()` requires at least one variable-label pair.
      v Use named `...` args: `set_missing_codes(x, age = 'Age in years')`.

# snapshot: set_missing_codes() surveycore_error_setter_mismatched_lengths

    Code
      set_missing_codes(d, variable = c("age", "income"), codes = list(c(Missing = -
        1L)))
    Condition
      Error:
      x `variable` has 2 elements but `codes` has 1 element.
      i They must be the same length (one content value per variable name).

# snapshot: set_missing_codes() surveycore_warning_var_not_found

    Code
      set_missing_codes(d, zzz_missing = c(Missing = -1L))
    Condition
      Warning:
      ! Variable zzz_missing not found in `x` and was skipped.

# snapshot: set_missing_codes() surveycore_warning_setter_empty_variables

    Code
      set_missing_codes(d, variable = character(0))
    Condition
      Warning:
      ! `set_missing_codes()` was called with `variable` of length 0.
      i No metadata was set. Did you accidentally filter all variable names out?

# snapshot: .check_is_survey_or_df() surveycore_error_not_survey_or_df for list input

    Code
      surveycore:::.check_is_survey_or_df(list(x = 1))
    Condition
      Error:
      x `x` must be a survey design object or a data frame, not <list>.
      v Create a survey object with `as_survey()`, `as_survey_replicate()`, or `as_survey_twophase()`.

# snapshot: surveycore_error_setter_mismatched_lengths message

    Code
      surveycore:::.parse_setter_input(dots = list(), variable = c("age", "income"),
      content = c("Age in years"), content_arg_name = "label", content_type = "scalar",
      fn_name = "set_var_label")
    Condition
      Error:
      x `variable` has 2 elements but `label` has 1 element.
      i They must be the same length (one content value per variable name).

# snapshot: surveycore_error_setter_ambiguous message

    Code
      surveycore:::.parse_setter_input(dots = list(age = "Age"), variable = "income",
      content = "Annual income", content_arg_name = "label", content_type = "scalar",
      fn_name = "set_var_label")
    Condition
      Error:
      x Provide variable names via `...` or via `variable`, not both.
      i Use named `...` args, a named vector in `...`, or `variable` + `label` — not a mix.

# snapshot: surveycore_error_setter_empty message

    Code
      surveycore:::.parse_setter_input(dots = list(), variable = NULL, content = NULL,
      content_arg_name = "label", content_type = "scalar", fn_name = "set_var_label")
    Condition
      Error:
      x `set_var_label()` requires at least one variable-label pair.
      v Use named `...` args: `set_var_label(x, age = 'Age in years')`.

# snapshot: surveycore_error_setter_mixed_dots message

    Code
      surveycore:::.parse_setter_input(dots = dots, variable = NULL, content = NULL,
        content_arg_name = "label", content_type = "scalar", fn_name = "set_var_label")
    Condition
      Error:
      x All `...` arguments must be named when using Convention 1.
      i Got 0 named and 1 unnamed element.
      v Use `set_var_label(x, age = 'Age', income = 'Annual income')` or a fully named vector.

# snapshot: .format_list_result() surveycore_error_format_invalid message

    Code
      surveycore:::.format_list_result(result_list, format = "named_vector", fn_name = "extract_val_labels")
    Condition
      Error in `surveycore:::.format_list_result()`:
      x `extract_val_labels()` received an invalid `format` value "named_vector".
      i `format` must be one of "list" and "data_frame".

# snapshot: extract_metadata() surveycore_error_not_survey_or_df

    Code
      extract_metadata(list(a = 1), a)
    Condition
      Error:
      x `x` must be a survey design object or a data frame, not <list>.
      v Create a survey object with `as_survey()`, `as_survey_replicate()`, or `as_survey_twophase()`.

# snapshot: extract_metadata() surveycore_error_fill_invalid

    Code
      extract_metadata(d, fill = NA_character_)
    Condition
      Error:
      x `extract_metadata()` does not accept `fill = NA`.
      i Valid values for `extract_metadata()`: "NULL" (omit empty) or "\"include\"" (return all).

# set_sata() errors when x is not a survey or data frame

    Code
      set_sata(list(a = 1), a)
    Condition
      Error:
      x `x` must be a survey design object or a data frame, not <list>.
      v Create a survey object with `as_survey()`, `as_survey_replicate()`, or `as_survey_twophase()`.

# set_sata() errors when both ... and variable provided

    Code
      set_sata(d, riagendr, variable = "ridageyr")
    Condition
      Error:
      x Provide variable names via `...` or via `variable`, not both.

# set_sata() errors when neither ... nor variable provided

    Code
      set_sata(d)
    Condition
      Error:
      x `set_sata()` requires at least one variable name.

# set_sata() errors when sata = NA

    Code
      set_sata(d, riagendr, sata = NA)
    Condition
      Error:
      x `sata` must be `TRUE` or `FALSE`.

# set_sata() errors when sata = 'yes' (non-logical)

    Code
      set_sata(d, riagendr, sata = "yes")
    Condition
      Error:
      x `sata` must be `TRUE` or `FALSE`.

# extract_sata() errors when x is not a survey or data frame

    Code
      extract_sata(list(a = 1))
    Condition
      Error:
      x `x` must be a survey design object or a data frame, not <list>.
      v Create a survey object with `as_survey()`, `as_survey_replicate()`, or `as_survey_twophase()`.

# extract_sata() errors when fill = TRUE

    Code
      extract_sata(d, fill = TRUE)
    Condition
      Error:
      x `fill` must be `FALSE` or `NULL`.

# extract_sata() errors when fill = 'x' (invalid)

    Code
      extract_sata(d, fill = "x")
    Condition
      Error:
      x `fill` must be `FALSE` or `NULL`.

# extract_sata() errors when format is invalid

    Code
      extract_sata(d, format = "tibble")
    Condition
      Error:
      x `extract_sata()` received an invalid `format` value "tibble".
      i `format` must be one of "named_vector", "list", and "data_frame".

# set_higher_is() errors when x is not a survey or data frame

    Code
      set_higher_is(list(x = 1), bpxsy1 = "worse")
    Condition
      Error:
      x `x` must be a survey design object or a data frame, not <list>.
      v Create a survey object with `as_survey()`, `as_survey_replicate()`, or `as_survey_twophase()`.

# extract_higher_is() errors when x is not a survey or data frame

    Code
      extract_higher_is(list(x = 1), bpxsy1)
    Condition
      Error:
      x `x` must be a survey design object or a data frame, not <list>.
      v Create a survey object with `as_survey()`, `as_survey_replicate()`, or `as_survey_twophase()`.

# set_higher_is() errors for invalid direction (Conv 1)

    Code
      set_higher_is(d, bpxsy1 = "neutral")
    Condition
      Error:
      x `direction` must be "\"better\"" or "\"worse\"" (or `NULL` to remove). Got "neutral".

# set_higher_is() errors for invalid direction (Conv 3)

    Code
      set_higher_is(d, variable = "bpxsy1", direction = "neutral")
    Condition
      Error:
      x `direction` must be "\"better\"" or "\"worse\"" (or `NULL` to remove). Got "neutral".

# set_higher_is() errors when both ... and variable provided

    Code
      set_higher_is(d, bpxsy1 = "worse", variable = "bpxsy1")
    Condition
      Error:
      x Provide variable names via `...` or via `variable`, not both.

# extract_higher_is() errors when both ... and variable provided

    Code
      extract_higher_is(d, bpxsy1, variable = "bpxsy1")
    Condition
      Error:
      x Provide variable names via `...` or via `variable`, not both.

# set_higher_is() errors when no variable names provided

    Code
      set_higher_is(d)
    Condition
      Error:
      x `set_higher_is()` requires at least one variable name.

# set_higher_is() warns when variable not found in x (setter)

    Code
      set_higher_is(d, nonexistent_var_xyz = "worse")
    Condition
      Warning:
      ! 1 variable not found in `x` and was skipped: nonexistent_var_xyz.

# set_reverse_coded() errors for non-survey non-df input

    Code
      set_reverse_coded(list(x = 1), bpxsy1)
    Condition
      Error:
      x `x` must be a survey design object or a data frame, not <list>.
      v Create a survey object with `as_survey()`, `as_survey_replicate()`, or `as_survey_twophase()`.

# extract_reverse_coded() errors for non-survey non-df input

    Code
      extract_reverse_coded(list(x = 1), bpxsy1)
    Condition
      Error:
      x `x` must be a survey design object or a data frame, not <list>.
      v Create a survey object with `as_survey()`, `as_survey_replicate()`, or `as_survey_twophase()`.

# set_reverse_coded() errors when reverse_coded = NA

    Code
      set_reverse_coded(d, bpxsy1, reverse_coded = NA)
    Condition
      Error:
      x `reverse_coded` must be `TRUE` or `FALSE`.

# set_reverse_coded() errors when both ... and variable provided (setter)

    Code
      set_reverse_coded(d, bpxsy1, variable = "bpxsy1")
    Condition
      Error:
      x Provide variable names via `...` or via `variable`, not both.

# extract_reverse_coded() errors when both ... and variable provided (extractor)

    Code
      extract_reverse_coded(d, bpxsy1, variable = "bpxsy1")
    Condition
      Error:
      x Provide variable names via `...` or via `variable`, not both.

# set_reverse_coded() errors when no variable names provided

    Code
      set_reverse_coded(d)
    Condition
      Error:
      x `set_reverse_coded()` requires at least one variable name.

# set_reverse_coded() warns when variable not found in x

    Code
      set_reverse_coded(d, variable = "nonexistent_var_xyz")
    Condition
      Warning:
      ! 1 variable not found in `x` and was skipped: nonexistent_var_xyz.

# .check_is_survey() error message uses single v bullet (no split ' ' key)

    Code
      surveycore:::.check_is_survey(list(x = 1))
    Condition
      Error:
      x `x` must be a survey design object, not <list>.
      v Create a survey object with `as_survey()`, `as_survey_replicate()`, or `as_survey_twophase()`.

# extract_sata() fill = TRUE errors with surveycore_error_fill_not_logical

    Code
      extract_sata(d, fill = TRUE)
    Condition
      Error:
      x `fill` must be `FALSE` or `NULL`.

