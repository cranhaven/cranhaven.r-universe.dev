#' @title Fetch OAI-PMH Identifiers with setSpec
#' @description Fetches OAI-PMH record identifiers along with their setSpec (collection name).
#' @param base_url A string. The base URL of the OAI-PMH server.
#' @param metadata_prefix A string. The metadata format (e.g., "marc21").
#' @param set A string. Optional. A set specifier.
#' @param user_agent A string. Custom User-Agent. Default is "OAIHarvester/1.0".
#' @return A tibble with identifier and setSpec.
#'
#' @importFrom httr GET content user_agent
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom tibble tibble
#' @export

fetch_identifiers_with_sets <- function(base_url, metadata_prefix, set = NULL, user_agent = "OAIHarvester/1.0") {
  url <- paste0(base_url, "?verb=ListIdentifiers")
  params <- list(metadataPrefix = metadata_prefix)
  if (!is.null(set)) {
    params$set <- set
  }

  response <- tryCatch({
    httr::GET(url, query = params, httr::user_agent(user_agent))
  }, error = function(e) {
    warning("Failed to fetch identifiers: ", e$message)
    return(NULL)
  })

  if (is.null(response) || response$status_code != 200) {
    stop("Failed to fetch identifiers. HTTP status code: ", response$status_code)
  }

  raw_content <- httr::content(response, as = "text", encoding = "UTF-8")
  xml <- read_xml(raw_content)
  xml <- strip_namespaces(xml)

  records <- xml_find_all(xml, "//header")
  identifiers <- xml_text(xml_find_all(records, "identifier"))
  set_specs <- lapply(records, function(record) {
    set_nodes <- xml_find_all(record, "setSpec")
    if (length(set_nodes) > 0) paste(xml_text(set_nodes), collapse = "|") else NA
  })

  tibble::tibble(identifier = identifiers, setSpec = unlist(set_specs))
}
