# R/recode.R
#
# This file is intentionally empty.
#
# The six survey-aware recode functions and their shared internal helpers have
# been split into individual files:
#
#   R/case-when.R       — case_when()
#   R/replace-when.R    — replace_when()
#   R/if-else.R         — if_else()
#   R/na-if.R           — na_if()
#   R/recode-values.R   — recode_values()
#   R/replace-values.R  — replace_values()
#
# Internal helpers (.validate_label_args, .wrap_labelled, .factor_from_result,
# .merge_value_labels) live in R/utils.R because they are used by 2+ files.
