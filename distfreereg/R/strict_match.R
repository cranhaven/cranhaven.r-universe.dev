strict_match <-
function(arg, choices){
  # This is intended to be used in place of match.arg() when partial matching is
  # not desired/allowed.
  if(is(choices, "distfreereg") || is(choices, "compare")){
    # When choices is a distfreereg or compare object, automatically set
	# choices to the vector of stat names from object.
    choices_internal <- names(choices[["observed_stats"]])
  } else {
    choices_internal <- choices
  }
  match_ind <- sapply(choices_internal, function(x) identical(arg, x))
  if(isFALSE(length(which(match_ind)) == 1)){
    if(is(choices, "compare")){
      stop("\"", arg, "\" not found among statistics in compare object")
    } else {
      stop(deparse1(substitute(arg)), " must be exactly one of ",
           paste0("\"", choices_internal, "\"", collapse = ", "),
           "; it cannot be \"", arg, "\"")
    }
  }
}