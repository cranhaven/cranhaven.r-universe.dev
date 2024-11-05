#' Get data columns
#' 
#' Get columns from a data table by specifying the index, column name as a string, or unquoted column name. Returns the column names or indices.
#'
#' @param data the existing tbl
#' @param ... Columns to get
#' @param as_index return the column indices (defaults to name) 
#'
#' @return vector of column names or indices
#' @export
#'
#' @examples
#' getcols(mtcars, 1, cyl, "disp", 5:7)
getcols <- function(data, ..., as_index = FALSE) {
  cols <- sapply(rlang::enexprs(...), function(v) {
    switch(
      typeof(v),
      symbol = rlang::as_string(v),
      character = v,
      language = names(data)[eval(v)], # usually e.g., 1:3
      names(data)[v]
    )
  }) %>% unlist() %>% as.vector()
  
  # make sure all columns are in the data 
  not_in_data <- setdiff(cols, names(data))
  if (length(not_in_data) > 0) {
    stop("Some columns were not in the data table: ", 
         paste(not_in_data, collapse = ", "))
  }
  
  if (as_index) {
    cols <- which(names(data) %in% cols)
  }
  
  unname(cols)
}
