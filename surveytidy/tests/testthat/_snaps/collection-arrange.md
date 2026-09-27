# arrange.survey_collection .if_missing_var = 'error' raises typed

    Code
      dplyr::arrange(coll, region)
    Condition
      Error in `.dispatch_verb_over_collection()`:
      x `arrange()` failed on survey "m1": missing referenced variable region.
      Caused by error:
      ! Survey 'm1' is missing referenced variable: region

