#' na.unreplace
#' 
#' Change values to NAs, ie make explicit `NAs` back to `NA`
#' 
#' @param x object
#' @param values values that are (or can be coerced to) `class(x)` that are to 
#'        be set to `NA`.
#' 
#' @details 
#' 
#' `na.unreplace` replaces `values` by `NA`. It is meant to be nearly inverse 
#'  operation to `na_replace` (and `na_explicit`). It can be used on both atomic 
#'  and recursive objects. Unlike  `na.replace` however, `values ` express the 
#'  values that if matched are set to `NA`.  It is basically:
#'  
#'  ```
#'  x[ x %in% values ] <- NA
#'  ````
#'  
#'  `na.unreplace` is a S3 method that can be used to defince additional 
#'  methods for other objects.
#' 
#' @seealso 
#' 
#'  * [na.replace()]
#'  
#' @examples 
#' 
#'  na.unreplace( c(1,2,3,4), 3 )
#'  na.unreplace( c("A", "(NA)", "B", "C") )
#'  na.unreplace( c("A", NA_explicit_, "B", "C") )
#'  
#'  df <- data.frame( char=c('A', 'NA', 'C', NA_explicit_), num=1:4 ) 
#'  na.unreplace(df)
#'  
#'  
#' @export 

na.unreplace <- function( x, values ) 
  UseMethod('na.unreplace')

#' @rdname na.unreplace
#' @export
na.unreplace.default <- function(x, values=NULL) 
  if( is.recursive(x) ) .na.unreplace.recursive(x, values ) else
  .na.unreplace.atomic( x, values )

.na.unreplace.recursive <- function(x,values=NULL) {
  for( i in 1:length(x) ) 
    x[[i]] <- na.unreplace( x[[i]], values )  
  x
}

.na.unreplace.atomic <- function(x, values=NULL ) {
  x[ x %in% values ] <- NA
  x
}


#' @rdname na.unreplace
#' @export
na.unreplace.character <- function( x, values=c("NA", NA_explicit_) ) {
  if( is.null(values) )
    values <- c("NA", NA_explicit_) 
  
  x[ x %in% values ] <- NA
  x
}

#' @rdname na.unreplace
#' @export
na.unreplace.factor <- function( x, values=c("NA", NA_explicit_) ) {
  if( is.null(values) )
    values <- c("NA", NA_explicit_) 
  
  x[ x %in% values ] <- NA
  x
}

#' @rdname na.unreplace
#' @export
na.implicit <- na.unreplace
