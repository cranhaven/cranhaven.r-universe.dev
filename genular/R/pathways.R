#' Suggest Pathway Matches Based on Query Values
#'
#' @description
#' This function queries the Genular API to suggest pathway matches based on an array
#' of query values. It is useful for identifying pathways related to specific terms
#' or concepts provided in the query.
#'
#' @param queryValues A character vector representing the search terms or values to find
#'        corresponding pathways.
#' @param options A list of options to customize the API request, including the API
#'        endpoint URL, api_key, timeout duration, and user-agent string, with sensible defaults
#'        set for querying the Genular pathways suggestion endpoint.
#'
#' @return A list containing suggested pathway matches including their identifiers and
#'         other relevant details based on the provided query values.
#' @examples
#' queryValues <- c("apoptosis", "signal transduction")
#' pathway_suggest_results <- pathways_suggest(queryValues)
#' print(pathway_suggest_results)
#' @export
pathways_suggest <-
    function(queryValues,
             options = list()) {  # Change here: set default to an empty list
        
        # Predefined options
        defaultOptions <- list(
            endpoint = "https://genular.atomic-lab.org/api/v1/pathways/suggest",
            api_key = "3147dda5fa023c9763d38bddb472dd28",
            timeout = 10000,
            user_agent = "GenularRClient/1.0"
        )
        
        # Merge user-provided options with predefined options
        # User options can override predefined ones
        effectiveOptions <- modifyList(defaultOptions, options)

        # Check for necessary input
        if (is.null(effectiveOptions$api_key)) {
            stop("API key is required.")
        }
        
        if (missing(queryValues)) {
            stop("queryValues argument is required and cannot be empty.")
        }
        
        if (!is.character(queryValues)) {
            stop("queryValues should be a character vector.")
        }
        
        # Construct the API endpoint URL with the provided API key
        apiUrl <- paste0(effectiveOptions$endpoint, "?api_key=", effectiveOptions$api_key)
        

        queryValues <- sort_input(queryValues)
        
        # Prepare the request payload
        body <-
            jsonlite::toJSON(list(queryValues = queryValues), auto_unbox = FALSE)
            
        response <- httr::POST(
            apiUrl,
            body = body,
            encode = "json",
            httr::add_headers(`Content-Type` = "application/json"),
            httr::timeout(effectiveOptions$timeout),
            httr::user_agent(effectiveOptions$user_agent)
        )
        
        # Extract content regardless of the response status
        content <- httr::content(response, "parsed", type = "application/json")

        # Verify the response and extract content
        if (response$status_code != 200) {
            message("Received HTTP status code: ", response$status_code)
            return(list(status_code = response$status_code, content = content, request_body = body))
        }
        
        return(content)
    }
