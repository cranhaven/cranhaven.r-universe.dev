#' UCSF Industry Documents Library Solr API
#' @import R6
#' @import arrow
#' @import jsonlite
#' @import stringr
#' @import data.table
#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%"
#' @export
IndustryDocsSearch <- R6::R6Class(
  "IndustryDocsSearch",
  private = list(
    base_url = "https://metadata.idl.ucsf.edu/solr/ltdl3/",
    create_query = function(...) {
      params <- list(...)

      if (!is.null(params$q)) {
        query <- sprintf("%squery?q=(%s)&wt=%s&cursorMark=%s&sort=%s",
                         private$base_url,
                         params$q,
                         params$wt,
                         params$cursorMark,
                         params$sort)
      } else {
        query_parts <- vapply(names(params), function(k) {
          v <- params[[k]]
          if (!is.null(v) && !(k %in% c("wt", "cursorMark", "sort", "n"))) {
            sprintf("%s:%s", k, v)
          } else {
            NA_character_
          }
        }, character(1))

        query_parts <- query_parts[!is.na(query_parts)]
        query <- sprintf("%squery?q=(%s)&wt=%s&cursorMark=%s&sort=%s",
                         private$base_url,
                         paste(query_parts, collapse = " AND "),
                         params$wt, params$cursorMark, params$sort)
      }
      cleaned_query <- clean_query_text(query)
      return(cleaned_query)
    },
    update_cursormark = function(query, cursor_mark) {
      updated_query <- stringr::str_replace(query, "(?<=cursorMark=)[A-Za-z0-9*=]+(?=&)", cursor_mark)
      return(updated_query)
    },
    loop_results = function(query, n) {
      next_cursor <- NULL
      current_cursor <- "*" # initial cursor mark

      initial_response <- jsonlite::fromJSON(httr::content(httr::GET(query), "text"))
      total_available <- initial_response$response$numFound

      if (n == -1) n <- Inf
      if (n > total_available) {
        message(sprintf("Warning: Only %f documents available, which is less than the %f requested", total_available, n))
        n <- total_available
      }

      while ((!identical(next_cursor, current_cursor)) && ((is.null(self$results)) || (nrow(self$results) < n))) {
        if (!is.null(next_cursor)) {
          current_cursor <- next_cursor
          query <- private$update_cursormark(query, current_cursor)
        }

        response <- jsonlite::fromJSON(httr::content(httr::GET(query), "text"))
        docs <- data.table::data.table(response$response$docs)
        if (is.null(self$results)) {
          if (n <= nrow(docs)) {
            self$results <- docs[1:n,]
          }
          else {
            self$results <- docs
          }
        }

        else {
          if (n < nrow(docs)) {
            self$results <- rbind(self$results, docs[1:n,], fill=TRUE)
          } else if (n < (nrow(self$results) + nrow(docs))) {
            self$results <- rbind(self$results, docs[1:(n - nrow(self$results)),], fill=TRUE)
          } else {
            self$results <- rbind(self$results, docs, fill=TRUE)
          }
        }

        next_cursor <- response$nextCursorMark
        message(sprintf("%d/%d documents collected", nrow(self$results), n))
      }
    },
    create_links = function(industry) {
      self$results <- self$results %>%
        dplyr::mutate(url = paste0("https://www.industrydocuments.ucsf.edu/", industry, "/docs/#id=", id))
    }
  ),
  public = list(
    #' @field results placeholder for storing query results
    results = NULL,
    #' @description Create a new IndustryDocsSearch instance
    #' @param NONE No parameters for initialization
    initialize = function() {
      # initialize empty data.table
      self$results <- NULL
    },

    #' @description Query the UCSF Industry Documents Solr Library
    #' @param q The query text that may incoporate the rest of the parameters. The function will not use the rest of the parameters if `q` is not NULL.
    #' @param case The case the collection is related to.
    #' @param collection The collection the results are found in.
    #' @param doc_type The document type(s) to filter the results.
    #' @param industry The industry the documents are located within.
    #' @param brand The brand the documents are related to.
    #' @param availability The availability status of the documents.
    #' @param date The date of the documents.
    #' @param id The id of the document(s).
    #' @param author The author or originator of the contents of the document(s).
    #' @param source The source of the document(s); usually the institution that deposited the documents.
    #' @param bates The bates number(s) of the document(s) to be retrieved.
    #' @param box The box id of the document(s) to be retrieved.
    #' @param originalformat The original format of the document(s) to be retrieved.
    #' @param wt The format the results should come in. Defaults to json. Functions depend on the results being returned as a JSON object.
    #' @param cursor_mark Initial placeholder for cursormark within the API URL
    #' @param sort The results will be sorted by ID in ascending order.
    #' @param n The number of results we want to capture. Defaults to 1000. If `n` is set to `-1` then all documents available related to the query will be retrieved.
    #' @example
        #' ids = IndustryDocsSearch$new()
        #' ids$query(
        #'   industry='tobacco',
        #'   case='State of North Carolina',
        #'   collection='JUUL labs Collection',
        #'   n=100)
    query = function(q = NULL,
                     case = NULL,
                     collection = NULL,
                     doc_type = NULL,
                     industry = NULL,
                     brand = NULL,
                     availability = NULL,
                     date = NULL,
                     id = NULL,
                     author = NULL,
                     source = NULL,
                     bates = NULL,
                     box = NULL,
                     originalformat = NULL,
                     wt = "json",
                     cursor_mark = "*",
                     sort = "id%20asc",
                     n = 1000) {

      query <- private$create_query(
        q = q,
        case = case,
        collection = collection,
        type = doc_type,
        industry = industry,
        brand = brand,
        availability = availability,
        documentdate = date,
        id = id,
        author = author,
        source = source,
        batesexpanded = bates,
        box = box,
        originalformat = originalformat,
        wt = wt, cursorMark = cursor_mark, sort = sort, n = n
      )

      industry_match <- regmatches(query,
                                   regexpr("(?<=industry:)\\w+(?=\\s)",
                                           query, perl = TRUE))

      private$loop_results(query, n)

      if (length(industry_match) > 0) {
        private$create_links(industry_match[1])
      }
    },
    #' @description Save results to file
    #' @param filename Output filename
    #' @param format Output format ('parquet' or 'json' or 'csv')
    #' @examples
        #' ids = IndustryDocsSearch$new()
        #' ids$query(
        #'   industry='tobacco',
        #'   case='State of North Carolina',
        #'   collection='JUUL labs Collection',
        #'   n=100)
        #' ids$save('query_results.csv', format='csv')
        #' file.remove('query_results.csv')

    save = function(filename, format) {
      df <- data.table::rbindlist(list(self$results), fill = TRUE)

      switch(format,
             "parquet" = arrow::write_parquet(df, filename),
             "json" = jsonlite::write_json(df, filename),
             'csv'= df %>%
               dplyr::mutate(across(where(~ class(.) %in% c("list", "data.frame", "vector", "matrix")),
                                    ~ sapply(., function(x) paste(capture.output(print(x)), collapse = " ")))) %>%
               data.table::fwrite(., filename),
             stop("Only parquet,json, and csv formats supported currently."))
    }
  )
)
