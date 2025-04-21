
mnis_reference_utils <- function(type, tidy, tidy_style) {

  got <- mnis_query(query = paste0(base_url, "ReferenceData/", type, "/"))

  x <- tibble::as_tibble(as.data.frame(got[[type]]))

  if (tidy == TRUE) {
    x <- mnis::ref_tidy(x, tidy_style)
  }
  x
}
