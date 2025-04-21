
mnis_additional_utility <- function(query) {
  got <- mnis_query(query)

  mem <- purrr::discard(got$Members$Member, is.null)

  names(mem) <- janitor::make_clean_names(names(mem))

  mem
}
