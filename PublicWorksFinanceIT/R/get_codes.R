#' Download ISTAT codes for italian regions, provinces and municipalities.
#'
#' get_codes allows to retrieve codes for regions, provinces, and municipality, filtering for the type of codes needed.
#'
#' @param type character. The argument can be set to \code{region}, \code{province}, or \code{municipality} according to which codes are needed.
#' @returns a data.frame object
#' @import magrittr
#' @author Lorena Ricciotti
#' @examples
#' data <- get_codes("region")
#'
#'@export
get_codes <- function(type) {
  endpoint <- "https://dati.isprambiente.it/sparql"
  if(type == "region"){
    init <- 'select distinct str(?reg) AS ?DEN_REG str(?istat_reg) AS ?COD_REG
where'
  }
  if(type == "province"){
    init <- 'select distinct str(?reg) AS ?DEN_REG str(?istat_reg) AS ?COD_REG str(?prov) AS ?DEN_PROV str(?istat_prov) AS ?COD_PROV
where'}
  if(type == "municipality"){
    init <- 'select distinct str(?reg) AS ?DEN_REG str(?istat_reg) AS ?COD_REG str(?prov) AS ?DEN_PROV str(?istat_prov) AS ?COD_PROV str(?com) AS ?DEN_MUN str(?istat) AS ?COD_MUN
where'}
  sparql_query_geo <- paste0(init,
                             '{?m a <https://w3id.org/italia/env/onto/place/Municipality>;
<http://www.w3.org/2000/01/rdf-schema#label> ?com;
<https://w3id.org/italia/env/onto/place/hasDirectHigherRank> ?hrank;
<https://w3id.org/italia/env/onto/place/istat> ?istat.
?hrank<http://www.w3.org/2000/01/rdf-schema#label> ?prov;
<https://w3id.org/italia/env/onto/place/hasRegion> ?hhrank;
<https://w3id.org/italia/env/onto/place/istat> ?istat_prov.
?hhrank <http://www.w3.org/2000/01/rdf-schema#label> ?reg;
<https://w3id.org/italia/env/onto/place/istat> ?istat_reg.
}')
  # Make the SPARQL query for georeferenced data
  response_geo <- httr::POST(
    url = endpoint,
    httr::add_headers("Content-Type" = "application/x-www-form-urlencoded"),
    body = list(query = sparql_query_geo),
    encode = "form"
  )
  # Get the content of the response
  data_geo <- httr::content(response_geo, as = "parsed")
  head_vars_geo <- data_geo$head$vars
  results_geo <- data_geo$results$bindings
  # Function to extract a single row from the results
  extract_row_geo <- function(result) {
    values <- sapply(head_vars_geo, function(var) {
      if (length(result[[var]]) == 0) {
        NA
      } else {
        result[[var]]$value
      }
    })
    data.frame(t(values), stringsAsFactors = FALSE)
  }
  # Apply the function to each result
  rows_geo <- lapply(results_geo, extract_row_geo)
  # Combining the rows into a data frame
  loc_current <- do.call(rbind, rows_geo)
  # Renaming the columns for georeferenced data
  colnames(loc_current) <- head_vars_geo
  return(loc_current)
}
