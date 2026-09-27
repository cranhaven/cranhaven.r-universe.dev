# as_survey_collection() rejects unnamed non-symbol arguments

    Code
      as_survey_collection(suppressMessages(suppressWarnings(as_survey(df1, ids = psu,
        weights = wt, strata = strata, fpc = fpc))))
    Condition
      Error in `.resolve_caller_names()`:
      x Argument 1 passed to `as_survey_collection()` is unnamed and is not a bare symbol.
      i Collection elements must be named, or be supplied as bare variable names so they can be auto-named.
      v Name the argument explicitly, e.g. `as_survey_collection(wave1 = as_survey(df, ...))`.

# as_survey_collection() duplicate-name repair snapshot

    Code
      coll <- as_survey_collection(d1, d1)
    Condition
      Warning:
      ! Duplicate survey name in collection repaired.
      i Rename: `d1 -> d1_1`.

# remove_survey() errors on unknown name

    Code
      remove_survey(coll, "nope")
    Condition
      Error in `remove_survey()`:
      x Name not found in collection: "nope".
      i Available: "a" and "b".

# remove_survey() error snapshot for invalid name type

    Code
      remove_survey(coll, 1L)
    Condition
      Error in `remove_survey()`:
      x `name` must be a character vector, not <integer>.
      i Got <integer> instead.

# as_survey_collection() rejects .id = NA_character_

    Code
      as_survey_collection(a = s$d1, .id = NA_character_)
    Condition
      Error in `.validate_collection_id()`:
      x `.id` must be a single non-empty, non-NA character string.
      i Got <character> of length 1: NA.

# as_survey_collection() rejects .if_missing_var = "warn"

    Code
      as_survey_collection(a = s$d1, .if_missing_var = "warn")
    Condition
      Error in `.validate_collection_if_missing_var()`:
      x `.if_missing_var` must be one of "error" or "skip".
      i Got <character> of length 1: "warn".

# print() renders id: and if_missing_var: at defaults

    Code
      print(coll)
    Message
      A <survey_collection> with 1 survey:
      id: ".survey"
      if_missing_var: "error"
      "a": survey_taylor, 40 rows, 8 variables

# print() renders id: and if_missing_var: at non-default values

    Code
      print(coll)
    Message
      A <survey_collection> with 2 surveys:
      id: "wave"
      if_missing_var: "skip"
      "y2018": survey_taylor, 40 rows, 8 variables
      "y2020": survey_taylor, 60 rows, 8 variables

# print() renders id and if_missing_var lines exactly once on multi-member

    Code
      print(coll)
    Message
      A <survey_collection> with 3 surveys:
      id: ".survey"
      if_missing_var: "error"
      "a": survey_taylor, 40 rows, 8 variables
      "b": survey_taylor, 60 rows, 8 variables
      "c": survey_taylor, 80 rows, 8 variables

