check_param_names <- function(param_names, ref_names, pattern_tag = "",
                              err_stop = TRUE) {

  # Checking parameter names from param_table against xml ones
  # param_names <- unique(gsub(pattern =
  # paste0("\\_(",pattern_tag,"){0,1}[1-9]+"), x = tolower(param_names), ""))
  param_names <- unique(
    gsub(
      pattern = paste0("\\_(", pattern_tag, "){0,1}[1-9]+"),
      x = param_names, ""
    )
  )

  diff_names <- setdiff(param_names, ref_names)

  if (length(diff_names)) {
    stop("Unknown parameter(s) name(s): \n", paste(diff_names, collapse = ", "))
  }
}
