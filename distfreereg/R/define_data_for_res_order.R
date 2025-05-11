# Calculate res_order, if that requires columns, which should refer to data
# argument rather than X matrix formed by lm(). This is done by sneaking a
# "data" element to distfreereg.function via the control argument.
define_data_for_res_order <- function(test_mean, data, ordering, override, control){
  if(is.null(override[["res_order"]]) && (identical(ordering, "natural") || is.list(ordering))){
    if(is.null(control)){
      control <- list()
    } else {
      if(!is.list(control)) stop("'control' must be NULL or a list")
    }
    data_for_ordering <- data
    data_for_ordering[[get_response(test_mean)]] <- NULL
    data_for_ordering[["(weights)"]] <- NULL
    control[["data"]] <- data_for_ordering
  }
  return(control)
}
