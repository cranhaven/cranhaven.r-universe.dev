# pull.survey_collection .if_missing_var = 'error' re-raises with vctrs parent

    Code
      dplyr::pull(coll, tidyselect::all_of("region"))
    Condition
      Error in `.handle_class_catch()`:
      x `pull()` failed on survey "m1": referenced column not found.
      Caused by error in `tidyselect::all_of()`:
      ! Can't subset elements that don't exist.
      x Element `region` doesn't exist.

# pull.survey_collection raises typed error on vec_c type clash

    Code
      dplyr::pull(coll, flag)
    Condition
      Error in `dplyr::pull()`:
      x `pull()` cannot combine "flag": incompatible types across surveys.
      i Surveys involved: "m1" and "m2".
      v Coerce the column to a common type with `mutate()` before `pull()`, or pull each survey individually.
      Caused by error in `vctrs::vec_c()`:
      ! Can't combine `..1` <character> and `..2` <integer>.

