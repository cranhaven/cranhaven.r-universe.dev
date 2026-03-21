
#' coerce_safe
#' 
#' Coerce values in a safe, non-destructive and consistent way.
#'
#' @param object to be coerced
#' @param class character; class to which `object` should be coerced.
#' @param alert function to use to raise exceptions: (Default: [base::stop()])
#' @param ... unused
#' @param alert_irreversible function to raise alert when coercion is not 
#'        reversible. See Details.
#' @param alert_na function to raise when `NAs` are produced.
#' 
#' `coerce_safe` transform the `object` to `class` in a safe, consistent, 
#' non-destructive way. 
#' 
#' Safe means that coercison:
#'  1. is non-destructive (i.e information is not lost in the transformation ) 
#'  2. is reversible: \deqn{ f^{-1}( f(x) ) == x }
#'  3. does not introduce (additional) missing values (`NA`) 
#'  
#' By default, `corece_safe` raises an alert (message|warning|error) 
#' when the attempted coercion violates these constraints.  The `alert` 
#' argument (and `alert_irreversible` or `alert_na`) can be used
#' to fleixble change the response.  Valid values for these are 
#' [base::message()], [base::warning()] and [base::stop] among others. 
#' 
#' @note
#' 
#' There must be a `as` method to the reverse coercion for this function to work.
#' 
#' @return 
#'   `object` coerced to `class` but ensured that there has been no loss in data
#' and no additional Missonve values introduced.
#' 
#' @seealso
#'   [methods::as] 
#'   `coercion::try_as()``
#'   
#' @examples 
#' 
#' \dontrun{
#'   # Error
#'   coerce_safe(1.01, "integer")  # 1.01 != 1
#'   coerce_safe( c("1","2","a"), "integer" )
#' }
#' 
#' @md
#' @import methods
#' @export 

coerce_safe <- function(object, class, alert=stop, ..., alert_irreversible=alert, alert_na=alert ) {
  
  if( is(object, class) ) {
    res <- object 
  } else { 
    suppressWarnings( res <- as(object, class) )  # Use coercion::try_as 
    # Test Reversibleility    if( any(res != object, na.rm=TRUE ) ) stop( paste0( "Coercion to '", class, "' altered value(s)." ) )  
    
    # Test NAs introduced
    if( any( is.na(object) != is.na(res) ) )
      stop( "Coercion of '", object, "' to class '", class, "' is not possible.")
      # stop( "Coercion to '", class, "' introduced ", na.n(res) - na.n(object), " additional missing value(s) (NA)." )
    
  }
  
  res
    
}
