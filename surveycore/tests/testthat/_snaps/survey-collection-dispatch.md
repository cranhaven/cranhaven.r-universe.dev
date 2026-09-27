# C5: .if_missing_var = 'error' aborts when one survey lacks the variable

    Code
      get_means(coll, focal, .if_missing_var = "error")
    Condition
      Error in `.dispatch_over_collection()`:
      x Survey "w2" in the collection is missing a required variable.
      i Original error: x Variable "focal" not found in survey data. i Available: "psu", "strata", "fpc", "wt", "y1", "y2", "y3", and "group".
      v Set `.if_missing_var = "skip"` to drop surveys missing the variable.
      Caused by error in `value[[3L]]()`:
      x Variable "focal" not found in survey data.
      i Available: "psu", "strata", "fpc", "wt", "y1", "y2", "y3", and "group".

# C6: .if_missing_var = 'skip' with all surveys missing aborts

    Code
      get_means(coll, focal, .if_missing_var = "skip")
    Message
      i Skipped 2 surveys missing the requested variable: "w1" and "w2".
    Condition
      Error in `.dispatch_over_collection()`:
      x No surveys in the collection contained the requested variable.

# C7: .id collision with an existing result column aborts

    Code
      get_means(coll, y1, .id = "mean")
    Condition
      Error in `.dispatch_over_collection()`:
      x `.id` value "mean" conflicts with a column produced by the analysis function.
      v Pass a different `.id`, e.g. `.id = "wave"`.

# C12: survey_glm() refuses survey_collection input

    Code
      survey_glm(coll, y1 ~ y2)
    Condition
      Error in `survey_glm()`:
      x `survey_glm()` does not yet support <survey_collection> inputs.
      i Run `survey_glm()` on each survey individually, or see `?survey_collection` for the current dispatch coverage.

# C12: get_anova() refuses survey_collection input

    Code
      get_anova(coll, y1 ~ grp2)
    Condition
      Error in `get_anova()`:
      x `get_anova()` does not yet support <survey_collection> inputs.
      i Run `get_anova()` on each survey individually, or see `?survey_collection` for the current dispatch coverage.

# C13: .id rejects empty, NA, non-char, wrong length

    Code
      get_means(coll, y1, .id = NA_character_)
    Condition
      Error in `.validate_collection_id()`:
      x `.id` must be a single non-empty, non-NA character string.
      i Got <character> of length 1: NA.

# C14: bind-type mismatch across surveys aborts with surveycore class

    Code
      get_means(coll, y1, group = grp)
    Condition
      Warning:
      ! 1 cell has fewer than 30 unweighted observations. Estimates in these cells may be unreliable for public reporting (AAPOR guidance).
      Warning:
      ! 1 cell has fewer than 30 unweighted observations. Estimates in these cells may be unreliable for public reporting (AAPOR guidance).
      Error in `tryCatchOne()`:
      x Cannot combine per-survey results: column type mismatch. Each survey must produce compatible types for shared columns.
      i Original error: Can't combine `..1$grp` <integer> and `..2$grp` <factor<38051>>.
      Caused by error in `dplyr::bind_rows()`:
      ! Can't combine `..1$grp` <integer> and `..2$grp` <factor<38051>>.

# C10: tidy-selected variable absent raises surveycore_error_variable_not_found

    Code
      get_means(d, nonexistent_variable)
    Condition
      Error in `value[[3L]]()`:
      x Variable "nonexistent_variable" not found in survey data.
      i Available: "psu", "strata", "fpc", "wt", "y1", "y2", "y3", and "group".

# C11: diverging value_labels across surveys emits divergence warning

    Code
      res <- get_means(coll, g)
    Condition
      Warning:
      ! Per-survey metadata diverges for 1 variable: g.
      i The top-level `.meta` reflects only the first survey. Per-survey metadata is preserved under `attr(result, ".meta")$per_survey`.
      i Downstream helpers (e.g., `clean()`, `gt()`) should consult `$per_survey` for accurate per-row labeling.
    Code
      res
    Output
      # A tibble: 2 x 5
        .survey  mean ci_low ci_high     n
        <chr>   <dbl>  <dbl>   <dbl> <int>
      1 w1          1      1       1    60
      2 w2          1      1       1    60

# id_collision hint mentions set_collection_id() under stored .id

    Code
      get_means(coll, y1, .id = NULL)
    Condition
      Error in `.dispatch_over_collection()`:
      x `.id` value "mean" conflicts with a column produced by the analysis function.
      v Pass a different `.id` to override (e.g., `.id = "wave"`) or update the stored property via `set_collection_id()`.

