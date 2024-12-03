#' Search for Gene Information Based on a Query
#'
#' @description
#' This function allows users to search for gene information by sending a POST request
#' to the 'Genular' API. It accepts various search parameters and returns information
#' about genes that match the search criteria.
#'
#' @param queryFields A character vector specifying the fields to search within the gene data.
#' @param queryValues A numeric vector representing the values to search for within the specified fields.
#' @param fieldsFilter An optional character vector specifying which fields to include in the response.
#' @param excludeFields If fieldsFilter is not provided (empty), all fields are returned, here you can specify the fields you want to exclude.
#' @param page An integer specifying the page number of the search results to retrieve.
#' @param limit An integer specifying the maximum number of results to return per page.
#' @param searchType A character string indicating whether to use 'and' or 'or' logic for multiple search conditions.
#' @param orderBy A character string specifying which field to sort the results by.
#' @param sortDirection A character string indicating the sort direction ('asc' or 'desc').
#' @param responseType A character string indicating the type of response to expect ('json' or 'csv').
#' @param matchType A character string indicating the type of match to perform ('exact' or 'regex').
#' @param organismType A list of organism type IDs to filter the search results.
#' @param ontologyCategories A list of ontology category IDs to filter the search results.
#' @param debug An integer value indicating whether to enable debug mode (1) or not (0).
#' @param options A list of additional options for the API request, including endpoint, api_key, timeout, and user-agent.
#'
#' @return Depending on the responseType parameter, this function returns a list with different elements:
#' If responseType is 'json', the function returns a list containing the HTTP status code ('status_code'), 
#' the parsed JSON content ('content') representing gene information matching the search criteria, 
#' and the original request body sent to the API ('request_body').
#' If responseType is 'csv', the function returns a list containing the HTTP status code ('status_code'), 
#' a data frame ('content') constructed from the CSV response representing gene information, 
#' and the original request body sent to the API ('request_body').
#' In case of an HTTP status code different from 200, the content part of the return value provides the received error message or data.
#' @examples
#' \donttest{
#' # Define search parameters
#' queryFields <- list(c("geneID")) # or c("geneID", "symbol" ...)
#' queryValues <- c(1, 56, 70)
#' searchType <- "or"
#' fieldsFilter <- c("geneID", "symbol", "crossReference.enseGeneID")
#'
#' # Execute the search
#' gene_search_results <- gene_search(queryFields, queryValues, 
#'                                      fieldsFilter, searchType = searchType, 
#'                                      page = 1, limit = 10)
#' 
#' # Print the results
#' print(gene_search_results)
#' }
#' @export
#' @importFrom utils read.csv
#' @importFrom utils modifyList
#' @importFrom jsonlite toJSON
gene_search <-
    function(queryFields,
             queryValues,
             fieldsFilter = c("geneID", "symbol", "crossReference.enseGeneID"),
             excludeFields = NULL,
             page = 1,
             limit = 10,
             searchType = "and",
             orderBy = "geneID",
             sortDirection = "asc",
             responseType = "json",
             matchType = "exact",
             organismType =  list(c("9606")),
             ontologyCategories = list(),
             debug = 0,
             options = list()) {
        
    # Predefined options
    defaultOptions <- list(
        endpoint = "https://genular.atomic-lab.org/api/v1/gene/search",
        api_key = "3147dda5fa023c9763d38bddb472dd28",
        timeout = 10000,
        user_agent = "GenularRClient/1.0"
    )
    # Merge user-provided options with predefined options
    # User options can override predefined ones
    effectiveOptions <- modifyList(defaultOptions, options)
    
    # Validate required parameters
    if (is.null(effectiveOptions$api_key)) {
        stop("API key is required.")
    }
    
    if (missing(queryFields) || missing(queryValues)) {
        stop("queryFields and queryValues are required and cannot be NULL or missing.")
    }

    # Apply sorting to each input
    fieldsFilter <- sort_input(fieldsFilter)
    queryFields <- sort_input(queryFields)
    queryValues <- sort_input(queryValues)
    organismType <- sort_input(organismType)
    ontologyCategories <- sort_input(ontologyCategories)

    # Initialize body list without excludeFields
    bodyList <- list(
        queryFields = queryFields,
        queryValues = queryValues,
        fieldsFilter = fieldsFilter,
        searchType = searchType,
        orderBy = orderBy,
        sortDirection = sortDirection,
        page = page,
        limit = limit,
        responseType = responseType,
        matchType = matchType,
        organismType =  organismType,
        ontologyCategories = ontologyCategories,
        debug = debug
    )
    
    # Add excludeFields only if it's a non-null character vector
    if (!is.null(excludeFields) && is.character(excludeFields)) {
        bodyList$excludeFields <- excludeFields
    }
    
    # Convert the body list to JSON
    body <- toJSON(bodyList, auto_unbox = TRUE)
    
    # Construct the API URL
    apiUrl <- paste0(effectiveOptions$endpoint, "?api_key=", effectiveOptions$api_key)
    
    # Send the POST request to the API
    response <-
        httr::POST(
            url = apiUrl,
            body = body,
            encode = "json",
            httr::add_headers(`Content-Type` = "application/json"),
            httr::timeout(effectiveOptions$timeout),
            httr::user_agent(effectiveOptions$user_agent)
        )
    
    # Handle response based on the responseType
    if (responseType == "json") {
        content <- httr::content(response, "parsed", type = "application/json")
        result <- list(status_code = response$status_code, content = content)

        if (debug == 1) {
            result$request_body <- body
        }

        if (response$status_code != 200) {
            if (debug == 1) {
                # Convert and store relevant response parts as a text string for debugging
                response_summary <- list(
                    status = response$status_code,
                    headers = as.character(response$headers),
                    content = httr::content(response, "text", encoding = "UTF-8")
                )
                result$request_response <- jsonlite::toJSON(response_summary, pretty = TRUE)
            }
            return(result)
        }

        return(result)

    } else if (responseType == "csv") {
        # Assuming the CSV content is returned as text
        csvContent <- httr::content(response, "text", encoding = "UTF-8")
        result <- list(status_code = response$status_code, content = csvContent)

        if (debug == 1) {
            # Ensure that body is converted to a JSON string
            result$request_body <- jsonlite::toJSON(bodyList, auto_unbox = TRUE, pretty = debug == 1)
        }

        if (response$status_code != 200 || nchar(csvContent) == 0) {
            if (debug == 1) {
                # Convert and store relevant response parts as a text string for debugging
                response_summary <- list(
                    status = response$status_code,
                    headers = as.character(response$headers),
                    content = substring(csvContent, 1, 1000)  # Limit content size for readability
                )
                result$request_response <- jsonlite::toJSON(response_summary, pretty = TRUE)
            }
            return(result)
        } else {
            # Proceed to process the CSV content
            tempFile <- tempfile(fileext = ".csv")
            writeLines(csvContent, tempFile)

            if (file.info(tempFile)$size == 0) {
                # Handle the case where the CSV file is empty
                result$content <- data.frame()  # Return an empty data frame if the CSV is empty
            } else {
                result$content <- read.csv(tempFile)
            }
            return(result)
        }
    }
}

#' Fetch All Pages of Gene Search Results
#'
#' @description
#' This function iteratively calls the `gene_search` function to retrieve all available
#' search results across pages for a given query.
#'
#' @param queryFields A character vector specifying the fields to search within the gene data.
#' @param queryValues A numeric/character vector representing the values to search for within the specified fields.
#' @param fieldsFilter A vector specifying which fields to include in the response.
#' @param searchType Indicates whether to use 'and' or 'or' logic for multiple search conditions.
#' @param orderBy Specifies which field to sort the results by.
#' @param sortDirection Indicates the sort direction ('asc' or 'desc').
#' @param responseType Indicates the type of response to expect ('json' or 'csv').
#' @param matchType Indicates the type of match to perform ('exact' or 'regex').
#' @param organismType A list of organism type IDs to filter the search results.
#' @param ontologyCategories A list of ontology category IDs to filter the search results.
#' @param limit The maximum number of results to return per page.
#' @param debug An integer value indicating whether to enable debug mode (1) or not (0).
#' @param options A list of additional options for the API request, including endpoint, api_key, timeout, and user-agent.
#'
#' @return A list of gene search results aggregated from all retrieved pages.
#' @examples
#' \donttest{
#' all_gene_results <- fetch_all_gene_search_results(
#'   queryFields = list(c("symbol")),
#'   queryValues = c("A1CF", "A2M", "A4GALT", "A4GNT"),
#'   fieldsFilter = c("geneID", "symbol", "crossReference.enseGeneID", 
#'                      "ontology.id", "ontology.term", "ontology.cat"),
#'   searchType = "or",
#'   orderBy = "geneID",
#'   sortDirection = "asc",
#'   responseType = "json",
#'   matchType = "exact",
#'   organismType = list(c(9606)),
#'   ontologyCategories = list(),
#'   limit = 5
#' )
#' }
#' @export
fetch_all_gene_search_results <- function(
  queryFields, 
  queryValues, 
  fieldsFilter = c("geneID", "symbol", "crossReference.enseGeneID", 
                   "ontology.id", "ontology.term", "ontology.cat"), 
  searchType = "or", 
  orderBy = "geneID", 
  sortDirection = "asc", 
  responseType = "json", 
  matchType = "exact", 
  organismType = list(c(9606)), 
  ontologyCategories = list(), 
  limit = 5, 
  debug = 0,
  options = list()
) {
    all_results <- list()
    current_page <- 1
    total_pages <- 1
    
    while (current_page <= total_pages) {
        gene_search_results <- gene_search(
            queryFields = queryFields,
            queryValues = queryValues,
            fieldsFilter = fieldsFilter,
            searchType = searchType,
            orderBy = orderBy,
            sortDirection = sortDirection,
            responseType = responseType,
            matchType = matchType,
            organismType = organismType,
            ontologyCategories = ontologyCategories,
            page = current_page,
            limit = limit,
            debug = debug,
            options = options
        )
        
        # Check if the HTTP status code is 200 (OK)
        if (gene_search_results$status_code == 200) {
            # Update total_pages after fetching the first page
            if (current_page == 1) {
              total_pages <- gene_search_results$content$totalPages
            }

            # Concatenate the results
            all_results <- c(all_results, gene_search_results$content$results)

            # Progress message
            message(sprintf("Fetching page %d out of %d...", current_page, total_pages))
        } else {
            warning(sprintf("Failed to fetch page %d. HTTP status code: %s", current_page, gene_search_results$status_code))
            if (debug == 1) {
                print(gene_search_results$request_response)
            }
        }
        
        # Update the current page counter
        current_page <- current_page + 1
    }
    
    return(all_results)
}
