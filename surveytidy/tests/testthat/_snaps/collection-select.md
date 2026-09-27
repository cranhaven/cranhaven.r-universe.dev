# select.survey_collection raises group_removed when group col is excluded

    Code
      dplyr::select(coll_g, y1)
    Condition
      Error in `.check_select_group_removal()`:
      x `select()` would remove group column group from the <survey_collection>.
      i Group columns must remain in every member's `@data`; the surveycore class validator (G1b) requires this.
      v Include the group column in the selection, or call `ungroup()` on the collection first.

# select.survey_collection .if_missing_var = 'error' raises typed

    Code
      dplyr::select(coll, tidyselect::all_of("region"))
    Condition
      Error in `.handle_class_catch()`:
      x `select()` failed on survey "m1": referenced column not found.
      Caused by error in `tidyselect::all_of()`:
      ! Can't subset elements that don't exist.
      x Element `region` doesn't exist.

