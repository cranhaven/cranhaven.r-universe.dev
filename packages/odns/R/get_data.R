#' Get data from a resource.
#'
#' @description Get data from a resource in tabular format with the option to
#'  select fields and perform basic filtering. Where multiple data sets are
#'  required from a package and/or no field selection and filtering is required
#'  the \code{get_resource} function can be used.
#'
#' @param resource A character string containing the resource id of the data set
#'  to be returned.
#' @param fields A character vector containing the names of fields to be 
#'  included in the returned table. The input is checked to ensure the specified
#'  fields exist in the chosen resource.
#' @param limit An integer specifying the maximum number of records to be
#'  returned, the default NULL value returns all records.
#' @param where A character string containing the 'WHERE' element of a simple
#'  SQL SELECT style query. Field names must be double quoted ",
#'  non-numeric values must be single quoted ', and both single and
#'  double quotes must be delimited. Example; \code{where = "\"AgeGroup\" = 
#'  \'45-49 years\'"}.
#' @param page_size An integer specifying the maximum number of records to be
#'  returned per query. Setting a value causes the use of offset pagination,
#'  multiple queries will be sent to return subsets of the available data.
#'  Subsets are joined before being returned. The default NULL value will always
#'  attempt to return all rows with a single query.
#'
#' @return A data.frame.
#'
#' @examples
#' \dontrun{
#' get_data(resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69")
#' 
#' get_data(
#'   resource = "edee9731-daf7-4e0d-b525-e4c1469b8f69",
#'   fields = c("AgeGroup", "EuropeanStandardPopulation"),
#'   where = "\"AgeGroup\" = \'45-49 years\'"
#' )
#' }
#'
#' @export
get_data <- function(resource, fields = NULL, limit = NULL, where = NULL,
                     page_size = NULL) {
  
  stopifnot(
    "Only one resource can be specified" = length(resource) == 1,
    "Check resource argument, a valid ID is exactly 36 characters long" = valid_id(resource),
    "limit must be NULL or numeric" = is.null(limit) || is.numeric(limit),
    "page_size must be NULL or numeric" = is.null(page_size) ||is.numeric(page_size)
  )
  
  meta <- resource_data_items(resource)$id
  
  stopifnot(
    "fields must only contain column names present in the target resource." = all(fields %in% meta)
  )
  
  n_rows <- nrow_resource(resource)
  
  limit <- as.integer(if(is.null(limit)) n_rows else limit)
  
  page_size <- as.integer(if(is.null(page_size)) n_rows else page_size)
  
  catch <- vector("list", ceiling(min(limit, n_rows) / page_size))
  
  page <- 1
  
  while (page <= length(catch)) {
    
    p_offset = (page - 1) * page_size
    
    p_limit = as.integer(min(limit - p_offset, page_size))
    
    query <- if (is.null(where)) {
      
      prep_nosql_query(resource = resource, fields = fields, limit = p_limit, 
                       offset = p_offset)
      
    } else {
      
      prep_sql_query(resource = resource, fields = fields, limit = p_limit,
                     offset = p_offset, where = where)
    }
    
    res <- httr::RETRY(
      verb = "GET",
      url = query,
      times = 3,
      quiet = TRUE,
      terminate_on = c(404)
    )
    
    detect_error(res)
    
    catch[[page]] <- httr::content(res)
    
    catch[[page]] <- lapply(catch[[page]]$result$records, function(x) {
      as.list(sapply(x, function(y) ifelse(is.null(y), NA, y)))
    })
    
    catch[[page]] <- data.table::setDF(
      data.table::rbindlist(catch[[page]], use.names = TRUE, fill = TRUE)
    )
    
    page <- page + 1
  }
  
  out <- do.call(rbind, catch)
  
  out <- utils::type.convert(out, as.is = TRUE)
  
  if (any(names(out) %in% c("_full_text", "_id"))) {
    out <- out[ , !names(out) %in% c("_full_text", "_id")]
  } 
  
  data.table::setcolorder(out, meta[meta %in% names(out)])
  
  return(out)
}
