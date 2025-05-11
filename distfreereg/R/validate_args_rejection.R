validate_args_rejection <- function(object, alpha, stat){
  if(!(is(object, "compare")))
    stop("\"object\" must have class \"compare\"")
  validate_numeric(alpha, min_len = 1, min_val = 0, max_val = 1)
  if(isTRUE(!all(stat %in% names(object[["observed_stats"]]))))
    stop("Some specified statistic(s) not found in ",
         deparse1(substitute(object)))
}