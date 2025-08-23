#' Retrieve information about soil defense type of the ReNDiS database
#'
#' The \code{get_type_RENDIS} function returns the list of type of interventions for soil defense contained in the ReNDiS database.
#'
#' @returns Return an object of class \code{data.frame}
#'
#' Types:
#'
#'   \itemize{
#'   \item{Frana} \strong{=>} Landslide
#'   \item{Non definito} \strong{=>} Not defined
#'   \item{Alluvione} \strong{=>} Flooding
#'   \item{Misto} \strong{=>} Mixed
#'   \item{Valanga} \strong{=>} Avalanche
#'   \item{Incendio} \strong{=>} Wildfire
#'   \item{Costiero} \strong{=>} Coastal
#'   }
#'
#'@references \href{http://www.rendis.isprambiente.it/rendisweb/}{ReNDiS}
#'
#'@author Lorena Ricciotti
#'
#'@examples
#'get_type_RENDIS()
#'
#'@export
get_type_RENDIS <- function(){
  endpoint <- "https://dati.isprambiente.it/sparql"

  sparql_query <- "SELECT DISTINCT ?Type
WHERE {
  ?i a <https://w3id.org/italia/env/onto/core#Intervention>;
  <https://w3id.org/italia/env/onto/core#officialInstabilityType> ?Type.
}"


  # Make the SPARQL query
  response <- httr::POST(
    url = endpoint,
    httr::add_headers("Content-Type" = "application/x-www-form-urlencoded"),
    body = list(query = sparql_query),
    encode = "form"
  )

  # Get the content of the response
  data <- httr::content(response, "parsed")

  head_vars <- data$head$vars
  results <- data$results$bindings

  extract_row <- function(result) {
    values <- sapply(head_vars, function(var) {
      if (length(result[[var]]) == 0) {
        NA
      } else {
        result[[var]]$value
      }
    })
    data.frame(t(values), stringsAsFactors = FALSE)
  }

  rows <- lapply(results, extract_row)
  df <- do.call(rbind, rows)
  colnames(df) <- head_vars
  return(df)
}


