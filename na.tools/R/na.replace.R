#' Replace Missing Values   
#'
#' Replaces `NA` values with explicit values.
#'
#' @param x vector in which `NA` values are to be replaced. 
#' @param .na scalar, length(x)-vector or function used to replace `NA`. See #Details. 
#' @param ... additional arguments passed to `.na` when it is a function.
#'    
#' 
#' @details 
#' 
#' `na.replace` replaces missing values in `x` by `.na` if possible.
#' 
#' In R, replacement of values can cause a change in the class/type of an object. 
#' This is not often desired. `na.replace` is class/type-safe and length-safe. 
#' It replaces missing values without changing the `x`'s class or length 
#' regardless of the value provided by `.na`. 
#' 
#' **Param: `x`** 
#' 
#' If `x` is **categorical** (e.g. character or factor), `.na` is optional. 
#' The default is "(NA)" and can be set with 
#' `options( NA_explicit_ = new_value )`. It can also be 
#' referenced directly with [NA_explicit_].
#'  
#' If `x` is a **factor**, unique values of `.na` not in already present in 
#' `levels(x)` will be added. They are appended silently unless 
#' `getOption('verbose')==TRUE` in which a message reports the added levels.
#' 
#' 
#' **Param: `.na`**
#' 
#' `.na` can be either a scalar, vector or function.
#' 
#' If a **scalar**, each missing value of `x` is replaced by `na`.
#' 
#' If a **vector**, `.na` must have length(x)`. Missing values of `x` are replaced 
#' by corresponding elements of `.na`.  Recycling values of `.na` is not 
#' allowed. An error will be thrown in the event that `length(.na)` is not `1`
#' or `length(x)`.`  
#'    
#' If a **function**, `x` is transformed by .na` with:
#' ````
#'      .na(x, ...)
#' ````    
#' then preceding with normal operations.          
#'    
#'    
#' `na.explicit` is an alias for na.replace that uses [NA_explicit_] for `.na``; 
#' it returns x unchanged if it cannot change the value.  
#' 
#' @return 
#' A vector with the class and length of `x`.  
#' `NA`s in `x` will be replaced by `.na`. `.na` is coerced as necessary.
#' 
#' @seealso 
#' 
#'  - [base::ifelse()], [base::replace()] 
#'  - `forcats::fct_explicit_na` - which only handles factors
#'   
#' @examples 
#' 
#'   # Integers and numerics
#'   na.replace( c(1,NA,3,NA), 2 )    # 1 2 3 2   
#'   na.replace( c(1,NA,3,NA), 1:4 )  # 1 2 3 4
#' 
#'   # This produces an error because it would change the type
#'   \dontrun{
#'   na.replace( c(1,NA,3,NA), letters[1:4] )  # "1" "b" "3" "d"
#'   }
#'   
#'   # Characters 
#'   lets <- letters[1:5]
#'   lets[ c(2,4) ] <- NA
#'   na.replace(lets)  # replace with NA_explicit_
#' 
#'   # Factors 
#'   fct <- as.factor( c( NA, letters[2:4], NA) )
#'   fct
#'   na.replace(fct, "z")  # z b c d z  -- level z added
#'   na.replace(fct, letters[1:5] )
#'   na.replace(fct)
#'      
#'  \dontrun{
#'    na.replace( rep(NA,3), rep(NA,3) )
#'  }
#'      
#' @md
#' @rdname na.replace
# @include utils
#' @export


na.replace <- function(x, .na, ...) { 
  
 if( is.recursive(x) ) 
   stop( call.=TRUE
         , "'na.replace' does not work for tables or lists. "
         , "Use 'na_replace' from the 'tidyimpute' package instead."
   )

 # A function for .na will choke when all_na(x) == TRUE  
 if( !missing(.na) )
   if( is.function(.na) && all(is.na(x) ) ) 
   warning( "All values of 'x' are missing (NA).")
  
 # CHECK .na length == 1 or length(x)
  if( ! missing(.na) )
    if( ! length(.na) %in% c(1,length(x)) )  
      stop( "length(.na) is not 1 or length(x); recycling of .na is not allowed.")
  
  UseMethod("na.replace")
}


#' @export
na.replace.default <- function(x, .na, ...) {
  
  wh <- which_na(x)
  if( length(wh) == 0 ) return(x)  # No need to evaluate without missing values.
  
  # 1. `.na` function(s) need to be evaluated for before used in replacemnet
  # NB. For row-based imputes, only  missing rows needed be calculate imputed value
  if( is.function(.na) ) .na <- .na(x, ...)
  
  # 2. This makes the results type/class-safe.
  .na <- coerce_safe( .na, class(x) )
  
  # 3. Specially handle cases in which replacement values are missing
  #    - Replacement values should not be missing
  #    - When .na is a function, compute it on `x`, use `...` as additional args.
  
  # 4. scalar AND vector .na results need separate treatment.
  # warnings indicate not all coercions are possible.
  if( length(.na) == 1 ) {  # SCALAR
    if( is.na(.na) ) { 
      warning("Replacement value is 'NA'. Returning values unchanged.")
      return(x)
    }
    x[ wh ] <- .na
    
  } else {                 # VECTOR   
    if( all_na( .na[wh] ) ) {
      warning( "Replacement values are all 'NA'.")
      return(x)
    } else if( any_na(.na[wh]) ) {  
      warning( "Replacement values contain missing values 'NA'." )  
    }
    x[wh] <- .na[wh]
  }
  
  x  
  
}



#' @export
# Replacement with factors requires managing new-levels  
na.replace.factor <- function( x, .na=NA_explicit_, ... ) { 

  # When .na is a function, we don't know what the replacement values are ...
  # We must call that function to get the replacement values. 
  if( is.function(.na) ) .na <- .na(x, ...)
    
  # If there are new levels, generate them
  new_levels <- setdiff( unique(.na), levels(x) ) 
  
  if( length(new_levels) > 0 ) { 
    if( getOption('verbose') ) 
      warning( "Adding levels to factor: ", paste( new_levels, sep=", "))
    levels(x) <- c( levels(x), new_levels )
  }
    
  # While this is virtually identical to 
  if( length(.na) == 1 )
    x[ is.na(x) ] <- .na else
    x[ is.na(x) ] <- .na[ is.na(x) ]

  x  
    
}


#' @export 
na.replace.character <- function( x, .na=NA_explicit_, ... ) 
  na.replace.default(x, .na )


  
#' @rdname na.replace
#' @export

na.explicit <- function(x) {
  na.replace(x, .na=NA_explicit_ )
}
