utils::globalVariables(
  c(
    ".",
    ":=",
    "pk_field_name",
    "variable_name",
    "label",
    "type",
    "n"
  )
)


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.rcdf <- list(
    rcdf.options = list(
      verbose = TRUE,
      survey_round = "2024",
      records = list(
        "2024" = c(
          "cbms_interview_record",
          "cbms_person_record",
          "cbms_person_record_tvet",
          "cbms_household_record",
          "cbms_household_record_child_mortality",
          "cbms_barangay_record",
          "cbms_barangay_record_list"
        )
      )
    )
  )

  to_set <- !(names(op.rcdf) %in% names(op))
  if (any(to_set)) options(op.rcdf[to_set])

  invisible()
}
