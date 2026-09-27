# rename.survey_collection raises group_partial when group col missing from a member

    Code
      dplyr::rename(coll_g, demo = group)
    Condition
      Error in `.check_group_rename_coverage()`:
      x `rename()` would partially rename group column group on the <survey_collection>.
      i Member "replicate" would not rename group, leaving `@groups` inconsistent across the collection.
      v Either include group in the rename for every member, or call `ungroup()` on the collection first.

# rename.survey_collection .if_missing_var = 'error' raises typed

    Code
      dplyr::rename(coll, area = region)
    Condition
      Error in `.handle_class_catch()`:
      x `rename()` failed on survey "m1": referenced column not found.
      Caused by error in `fn()`:
      ! Can't rename columns that don't exist.
      x Column `region` doesn't exist.

