# classify_question_type() errors when no variables provided

    Code
      classify_question_type(d)
    Condition
      Error:
      x `classify_question_type()` requires at least one variable name.

# classify_question_type() errors when both ... and variable provided

    Code
      classify_question_type(d, riagendr, variable = "ridageyr")
    Condition
      Error:
      x Provide variable names via `...` or via `variable`, not both.

# classify_question_type() errors on non-survey non-data-frame input

    Code
      classify_question_type(list(a = 1), a)
    Condition
      Error:
      x `x` must be a survey design object or a data frame, not <list>.
      v Create a survey object with `as_survey()`, `as_survey_replicate()`, or `as_survey_twophase()`.

