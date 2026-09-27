# group_by.survey_collection .if_missing_var = 'error' raises typed

    Code
      dplyr::group_by(coll, region)
    Condition
      Error in `.dispatch_verb_over_collection()`:
      x `group_by()` failed on survey "m1": missing referenced variable region.
      Caused by error:
      ! Survey 'm1' is missing referenced variable: region

