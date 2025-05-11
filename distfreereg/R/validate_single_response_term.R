validate_single_response_term <- function(form){
  if(length(get_response(form)) != 1)
    stop("model formula must have a single response term")
}