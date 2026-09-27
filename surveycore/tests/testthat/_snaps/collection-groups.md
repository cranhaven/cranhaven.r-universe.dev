# snapshot: G8 override warning

    Code
      invisible(as_survey_collection(wave1 = d1, group = strata))
    Condition
      Warning:
      ! Member "wave1" had @groups "psu"; overriding with "strata".
      i The `group` argument to `as_survey_collection()` takes precedence over pre-existing member grouping.
      i If this was unintentional, call `surveytidy::ungroup()` on the member first, or omit `group` to adopt from members.

# divergent member @groups with no group = errors G2

    Code
      as_survey_collection(wave1 = d1, wave2 = d2)
    Condition
      Error in `as_survey_collection()`:
      x Member surveys have different @groups, and no `group` was supplied.
      i Found: "wave1: strata" and "wave2: psu".
      v Supply `group` explicitly, or ungroup members first via `surveytidy::ungroup()`.

# group = naming a column missing from a member errors G3

    Code
      as_survey_collection(wave1 = d1, wave2 = d2, group = region)
    Condition
      Error in `as_survey_collection()`:
      x Column region (from `group`) not found in member "wave2".
      i Members: "wave1" and "wave2".

# ungrouped coll + grouped new: errors G4

    Code
      add_survey(coll, wave2 = d2_grouped)
    Condition
      Error in `add_survey()`:
      x Cannot add survey "wave2": `@groups` differs from collection.
      i Collection: ; survey: "strata".
      v Ungroup the survey via `surveytidy::ungroup()` before adding, or group the collection to match.

# grouped collection print includes Groups: line

    Code
      print(coll)
    Message
      A <survey_collection> with 2 surveys:
      Groups: strata
      id: ".survey"
      if_missing_var: "error"
      "wave1": survey_taylor, 40 rows, 8 variables
      "wave2": survey_taylor, 40 rows, 8 variables

# ungrouped collection print omits Groups: line

    Code
      print(coll)
    Message
      A <survey_collection> with 2 surveys:
      id: ".survey"
      if_missing_var: "error"
      "wave1": survey_taylor, 40 rows, 8 variables
      "wave2": survey_taylor, 40 rows, 8 variables

