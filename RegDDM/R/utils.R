#' Somehow stan does not support : in variable names thus we have to replace it with _.
#' @keywords internal
#' @noRd
replace_colon = function(var){
  return(stringr::str_replace_all(var, ":", "_"))
}

