
## Specific function for tidying reference functions.
ref_tidy <- function(df, tidy_style) {
  names(df) <- gsub(".*\\.", "", names(df))

  names(df) <- gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", names(df))

  names(df) <- tolower(names(df))

  if (tidy_style == "camelCase") {
    names(df) <- gsub(
      "(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2",
      names(df),
      perl = TRUE
    )

    substr(names(df), 1, 1) <- tolower(substr(names(df), 1, 1))
  } else if (tidy_style == "period.case") {
    names(df) <- gsub("_", ".", names(df))
  }

  df
}
