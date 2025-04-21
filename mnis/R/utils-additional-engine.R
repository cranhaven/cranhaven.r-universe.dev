

## powers all the mnis_additional functions
mnis_additional_engine <- function(ID, ref_dods, tidy, tidy_style, type) {
  if (missing(ID)) {
    rep <- mnis_all_members()
  } else {
    ID <- as.character(ID)

    id_type <- mnis_id_type_util(ref_dods)

    query <- paste0(base_url, "/members/query/", id_type, ID, type)

    rep <- mnis_additional_utility(query)
  }

  rep
}
