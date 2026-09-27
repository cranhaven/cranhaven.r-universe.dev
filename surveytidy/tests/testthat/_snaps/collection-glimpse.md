# glimpse.survey_collection raises on id-collision before binding

    Code
      dplyr::glimpse(coll)
    Condition
      Error in `dplyr::glimpse()`:
      x `glimpse()` on a <survey_collection> would collide on column .survey.
      i Members already containing a .survey column: m2. The prepended id from `coll@id` would clash on `bind_rows()`.
      v Rename the colliding column with `rename()`, or set a different `coll@id` via `surveycore::set_collection_id()` before `glimpse()`.

