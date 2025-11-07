
#' @noRd
check_input_data <- function(data,
                             id,
                             value,
                             plotlevel) {
  
  if(is.null(data)) {
    message("No data provided, showing empty plot.")
    return(invisible())
  }
  
  if(is.data.frame(data) && is.null(id)) {
    stop("When data is provided an id column must also be provided.")
  }
  
  if(is.data.frame(data) && is.null(value)) {
    stop("When data is provided a value column must also be provided.")
  }
  
  if (!is.data.frame(data)) {
    stop("Inputdata must be a data.frame or similar.")
  }
  
  if(any(duplicated(data[[id]]))) {
    stop("Duplicates in provided id-variabel. Data must contain only one row per id.")
  }
  
  if(!is.null(id) && !is.character(id)) {
    stop("Argument 'id' must be a character specifying a column in data.")
  }
  
  if(!id %in% names(data)) {
    stop("The provided 'id' name is not present in data.")
  }
  
  if(!is.null(value) && !is.character(value)) {
    stop("Argument 'value' must be a character specifying a column in data.")
  }
  
  
  if(!value %in% names(data)) {
    stop("The provided 'value' name is not present in data.")
  }
  
  stopifnot(
    class(data[[value]]) %in% c("numeric", "integer", "character", "factor")
  )
  
  check_ids(
    data = data,
    id = id,
    plotlevel = plotlevel
  )
  
  return(invisible())
  
}

#' @noRd
check_ids <- function(data,
                      id,
                      plotlevel) {
  
  
  valid_ids <- switch(
    plotlevel,
    "region" = plotDK::region_info,
    "province" = plotDK::province_info,
    "municipality" = plotDK::municipality_info,
    "zipcode" = plotDK::zipcode_info
  )
  
  if(is.numeric(data[[id]])) {
    id_var <- paste0(plotlevel,"_numbers")
  } else if (is.character(data[[id]])) {
    id_var <- paste0(plotlevel, "_names")
  } else{
    stop("The provided 'id'-column must be either a numeric or a character")
  }
  
  
  all_mismatches <- all(!data[[id]] %in% valid_ids[[id_var]])
  
  any_mismatches <-  any(!data[[id]] %in% valid_ids[[id_var]])
  
  if(all_mismatches) {
    stop(
      "No valid id's provided. See valid id's in ?plotDK::",
      plotlevel,
      "_info."
    )
  }
  
  if(any_mismatches) {
    warning(
      "The following provided id's are not recognized as valid and will not be plotted: ",
      paste0(
        setdiff(
          unique(data[[id]]),
          valid_ids[[id_var]]
        ),
        collapse = ", "
      )
    )
  }
  
}