# C5: .if_missing_var = 'error' aborts when one survey lacks the variable

    Code
      get_variance(coll, focal, .if_missing_var = "error")
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
      get_variance(coll, focal, .if_missing_var = "skip")
    Message
      i Skipped 2 surveys missing the requested variable: "w1" and "w2".
    Condition
      Error in `.dispatch_over_collection()`:
      x No surveys in the collection contained the requested variable.

# C7: .id collision with an existing result column aborts

    Code
      get_variance(coll, y1, .id = "variance")
    Condition
      Error in `.dispatch_over_collection()`:
      x `.id` value "variance" conflicts with a column produced by the analysis function.
      v Pass a different `.id`, e.g. `.id = "wave"`.

# C13: .id rejects empty, NA, non-char, wrong length

    Code
      get_variance(coll, y1, .id = NA_character_)
    Condition
      Error in `.validate_collection_id()`:
      x `.id` must be a single non-empty, non-NA character string.
      i Got <character> of length 1: NA.

