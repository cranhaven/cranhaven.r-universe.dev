#' extract_algebra_elements
#'
#' extract all variables/parameters from an mxAlgebra
#' @param mxAlgebra_formula formula embedded in mxAlgebra
#' @param extracted used in recursive function calls; don't set this manually
#' @return vector with names of variables and parameters used in the function call
#' @keywords internal
extract_algebra_elements <- function(mxAlgebra_formula, extracted = c()){

  for(i in 1:length(mxAlgebra_formula)){
    if(is(mxAlgebra_formula[[i]], "call") |
       # terms in rounded brackets are of class "(" and must be parsed further
       is(mxAlgebra_formula[[i]], "(")){
      extracted <- extract_algebra_elements(mxAlgebra_formula[[i]], extracted)
    }else if(is(mxAlgebra_formula[[i]], "name")){
      potential_element <- mxAlgebra_formula[[i]]
      if(as.character(potential_element) %in% c(OpenMx::omxSymbolTable$R.name, "(", ")"))
        next
      extracted <- c(extracted, as.character(potential_element))
    }else if(is(mxAlgebra_formula[[i]], "numeric")){
      extracted <- c(extracted, mxAlgebra_formula[[i]])
    }else{
      stop("Unknown element in Algebra: ", mxAlgebra_formula[[i]])
    }
  }

  return(extracted)

}
