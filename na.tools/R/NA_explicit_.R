options( NA_explicit_ = "(NA)" ) 

#' NA_explicit_
#'
#' Default replacement for missing values in categorical vectors.
#' 
#' @details 
#' 
#' `NA_explicit_` is used as a default replacement for categorical vectors. 
#' 
#' It is an active binding to `getOptions('NA_explicit_')` and is exported
#' to the callers namespace. 
#' 
#' To change the value of `NA_explicit` use:
#' ```
#' options( NA_explicit = new_value )
#' ``` 
#' `NA_explicit_` cannot be directly set.
#' 
#' @seealso 
#' 
#'  [na.replace()] 
#' 
#' @md
#' @export

NA_explicit_ <- 
  makeActiveBinding(
      "NA_explicit_"
    , function(x) {
        val <- getOption("NA_explicit_")
        # val <- structure(val, class = c("NA_explicit_", class(val)))
        val
      }
    , sys.frame( sys.nframe() )
  )


#' #' @rdname NA_explicit_
#' #' @export
#' print.NA_explicit_ <- function(object) print( as.character( object ) )
#' 
#' #' @rdname NA_explicit_
#' #' @export
#' show.NA_explicit_ <- function(object) show( as.character( object ) )
