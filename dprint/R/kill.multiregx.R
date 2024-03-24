#' Kill Multiple Regular Expression
#'
#' Compacting some regular expression logic to remove regular expressions from vector
#'
#' @param string Input string which will have regular expressions removed
#' @param regx Perl Regular expression
#' @export
kill.multiregx <-
function(string, # string
                           regx # Perl Regular expression
                           )
{

  string <- sub(regx, '', string, perl = TRUE)
  g.spc <- grep(regx, string)
  if(length(g.spc) > 0)   { string <- kill.multiregx(string, regx) }
  return(string)
}

