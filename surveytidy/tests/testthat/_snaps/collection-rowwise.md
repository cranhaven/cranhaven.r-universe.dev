# rowwise.survey_collection .if_missing_var = 'error' raises typed

    Code
      dplyr::rowwise(coll, tidyselect::all_of("region"))
    Condition
      Error in `.handle_class_catch()`:
      x `rowwise()` failed on survey "m1": referenced column not found.
      Caused by error in `tidyselect::all_of()`:
      ! Can't subset elements that don't exist.
      x Element `region` doesn't exist.

