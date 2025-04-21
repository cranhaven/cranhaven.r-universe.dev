
## additional function to reduce total amount of code in package and make
# maintenance easier used in: - mnis_additional - mnis_full_biog

get_additional <- function(query, tidy, tidy_style) {
  got <- mnis_query(query)

  df <- got$Members$Member

  if (tidy == TRUE) {
    df <- mnis_tidy(df, tidy_style)
  }

  df
}


## generic to construct mnis_additional queries to reduce amount of code
# and keep changes in one place
additional_generic <- function(ID, ref_dods, tidy, tidy_style, query_type) {
  baseurl <- "http://data.parliament.uk/membersdataplatform/services/mnis/members/query/"

  if (ref_dods == TRUE) {
    ID_Type <- "refDods="
  } else {
    ID_Type <- "id="
  }

  query <- paste0(baseurl, ID_Type, ID, query_type)

  df <- get_additional(query, tidy, tidy_style)

  df
}
