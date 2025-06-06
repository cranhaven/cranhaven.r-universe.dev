#' Creates Formula for the pl function
#' 
#' This function takes the bwreg object, as used in the pl function as input. 
#' It extracts the used formula and identifies the terms using splines. These are
#' changed, so that they use the "rb" function to create the splines. The left-
#' hand side is then changed to a "Surv" Object, with the same response, but the
#' delta is set to all ones.
#' 
#' @noRd
#' 
#' @importFrom formula.tools rhs.vars lhs
#' @importFrom rlang call_args
#' @importFrom stats as.formula


pl_formula <- function(bwreg){
  
  rh <- rhs.vars(bwreg$formula)
  bwnames <- unlist(lapply(bwreg$smooth, function(x)x$term))
  txt <- ""
  for(i in seq_along(rh)){
    if(any(strsplit(rh[i], "")[[1]] == "(")){
      var <- call_args(str2lang(rh[i]))[[1]]
      txt <- paste0(txt,"rb(", var,", B_size = bwreg$smooth[[",which(bwnames==var),"]]$bs.dim-2)")
    }else{
      txt <- paste0(txt,rh[i])
    }
    if( i != length(rh)){
      txt <- paste(txt,"+")
    }
  }
  nrows <- nrow(bwreg$model)
  return(as.formula(paste("Surv(",lhs(bwreg$formula),", rep(1,",nrows,")) ~", txt)))
}
