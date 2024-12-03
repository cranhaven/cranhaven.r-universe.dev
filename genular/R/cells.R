#' Suggest Cell Matches Based on Query Values
#'
#' @description
#' This function communicates with the 'Genular' API to suggest cell matches
#' based on an array of query values. It sends a POST request with the query
#' values and retrieves suggested cell matches, including details and scores.
#'
#' @param queryValues A character vector of cell names or identifiers to find matches for.
#' @param responseType A character string indicating the type of response to expect ('json' or 'csv').
#' @param debug An integer value indicating whether to enable debug mode (1) or not (0).
#' @param options A list that specifies the API endpoint, api_key, timeout duration, and user agent string,
#'        with default values preset for the 'Genular' API cell suggestion endpoint.
#'
#' @return A list containing suggested cell matches, each with associated details like keys,
#'         values, search scores, and expression marker scores.
#' @examples
#' queryValues <- c("endothelial cell", "T cell")
#' cell_suggest_results <- cells_suggest(queryValues)
#' print(cell_suggest_results)
#' @export
cells_suggest <-
    function(queryValues,
             responseType = "json",
             debug = 0,
             options = list()) {
    
    # Predefined options
    defaultOptions <- list(
        endpoint = "https://genular.atomic-lab.org/api/v1/cells/suggest",
        api_key = "3147dda5fa023c9763d38bddb472dd28",
        timeout = 10000,
        user_agent = "GenularRClient/1.0"
    )
    
    # Merge user-provided options with predefined options
    # User options can override predefined ones
    effectiveOptions <- modifyList(defaultOptions, options)

    # Validate the input
    if (is.null(effectiveOptions$api_key)) {
        stop("API key is required.")
    }
    
    if (missing(queryValues)) {
        stop("queryValues argument is required and cannot be empty.")
    }
    
    if (!is.character(queryValues)) {
        stop("queryValues should be a character vector.")
    }
    
    queryValues <- sort_input(queryValues)

    # Prepare the API URL
    apiUrl <- paste0(effectiveOptions$endpoint, "?api_key=", effectiveOptions$api_key)
    
    # Prepare the data payload as JSON
    body <-
        jsonlite::toJSON(list(queryValues = queryValues, responseType = responseType), auto_unbox = FALSE)
    
    # Execute the POST request
    response <- httr::POST(
        apiUrl,
        body = body,
        encode = "json",
        httr::add_headers(`Content-Type` = "application/json"),
        httr::timeout(effectiveOptions$timeout),
        httr::user_agent(effectiveOptions$user_agent)
    )
    
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
            result$request_body <- jsonlite::toJSON(body, auto_unbox = TRUE, pretty = debug == 1)
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

#' Search Cell Information Based on Query Conditions
#'
#' @description
#' This function interacts with the 'Genular' API to search for cell information based on specific
#' query conditions related to cell IDs and expression marker scores. It sends a POST request
#' with these conditions and retrieves matching cell information.
#'
#' @param queryValues A named list where keys are cell IDs and values are condition strings
#'        indicating the expression score criteria.
#' @param fieldsFilter A character vector specifying which fields to include in the response.
#' @param excludeFields If fieldsFilter is not provided (empty), all fields are returned, here you can specify the fields you want to exclude.
#' @param orderBy The field name by which to order the returned results.
#' @param sortDirection The direction of sorting, which can be either "asc" for ascending or "desc" for descending.
#' @param page An integer specifying the page number for pagination of results.
#' @param limit An integer specifying the maximum number of results to return per page.
#' @param searchType A character string indicating whether to use 'and' or 'or' logic for multiple search conditions.
#' @param responseType A character string indicating the type of response to expect ('json' or 'csv').
#' @param matchType A character string indicating the type of match to perform ('exact' or 'regex').
#' @param organismType A list of organism type IDs to filter the search results.
#' @param debug An integer value indicating whether to enable debug mode (1) or not (0).
#' @param options A list containing additional options for the API request, including the endpoint, api_key, timeout, and user-agent.
#'
#' @return A list containing detailed information about the cells that meet the search criteria, including the requested fields.
#' @examples
#' \donttest{
#' queryValues <- list("CL0001082" = ">= 250")
#' fieldsFilter <- c("geneID", "symbol")
#' cell_search_results <- cells_search(queryValues, fieldsFilter)
#' print(cell_search_results)
#' }
#' @export
cells_search <-
    function(queryValues,
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
             debug = 0,
             options = list()) {
        
    # Predefined options
    defaultOptions <- list(
        endpoint = "https://genular.atomic-lab.org/api/v1/cells/search",
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
    
    if (!is.list(queryValues)) {
        stop("queryValues argument must be a list.")
    }
    
    if (!is.character(fieldsFilter)) {
        stop("fieldsFilter argument must be a character vector.")
    }

    fieldsFilter <- sort_input(fieldsFilter)
    ## queryValues <- sort_input(queryValues)
    organismType <- sort_input(organismType)
    

    # Initialize body list without excludeFields
    bodyList <- list(
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
    
    # Execute the POST request
    response <- httr::POST(
        apiUrl,
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
#' Format Cell Search Results for Query Based on Lineage
#'
#' @description
#' This function formats the results from cell search queries into a structured list
#' based on specified lineage criteria. It allows subsetting of the cell data to include
#' parent cells, child cells, or both in the output, and converts them into a named list
#' where each key is a `cell_id` and each value is a string representing a condition on
#' the `marker_score`.
#'
#' @param cell_list A list of cells, each including details such as `cell_id` and `marker_score`,
#'        and optionally containing nested lists of child cells.
#' @param cells_lineages A character string specifying the lineage subset to include in the output.
#'        Options are "parent" for only parent cells, "childs" for only child cells, and "both"
#'        for including both parent and child cells. Defaults to "both".
#'
#' @return A named list where keys are `cell_id`s and values are strings formatted as conditions
#'         on the `marker_score`. This list can be used for constructing query conditions
#'         in further API requests.
#'
#' @examples
#' cells <- list(
#'   list(cell_id = "CL0000235", cell_name = "macrophage", 
#'   marker_score = 1112.325, childs = list()),
#'   list(cell_id = "CL0000784", cell_name = "plasmacytoid dendritic cell", 
#'   marker_score = 537.7737, childs = list(
#'     list(cell_id = "CL0001058", cell_name = "plasmacytoid dendritic cell, human", 
#'   marker_score = 262.985)
#'   ))
#' )
#' formatted_query_values <- cells_search_format(cells, cells_lineages = "both")
#' print(formatted_query_values)
#' @export
cells_search_format <- function(cell_list, cells_lineages = "both") {
    result <- list()
    extract_and_format <- function(cell, lineage) {
        temp_result <- list()
        include_parent <- lineage %in% c("parent", "both")
        include_childs <- lineage %in% c("childs", "both")

        if (include_parent) {
            # Formatting cell_id with marker_score for parent
            temp_result[[cell$cell_id]] <- paste0(">= ", cell$marker_score)
        }
        
        if (include_childs && length(cell$childs) > 0) {
            # Recurse for child cells if they exist and are to be included
            for (child in cell$childs) {
                child_results <- extract_and_format(child, "both")  # Child cells always process both
                temp_result <- c(temp_result, child_results)
            }
        }
        return(temp_result)
    }
    for (cell in cell_list) {
        result <- c(result, extract_and_format(cell, cells_lineages))
    }
    return(result)
}
