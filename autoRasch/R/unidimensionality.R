#' Unidimensionality Check
#'
#' This function checks the unidimensionality status using the confirmatory factor analysis.
#'
#' @param x The dataset of responses.
#' @param is.polychor A boolean parameter to set whether the dataset is categorical or not.
#'
#' @return A list of the CFA output and the some of the goodness-of-fit indices (i.e., cfi, tli, rmsea, and srmr)
#'
#' @importFrom lavaan cfa
#' @importFrom lavaan fitMeasures
#'
#' @rdname unidimensionality
#' @export
check.unidim <- function(x, is.polychor = TRUE){

  defaultW <- getOption("warn")
  options(warn = -1)

  resid <- x

  if(is.null(dim(resid))){
    resid <- matrix(resid,ncol = 1,dimnames = list(seq_along(resid), c("item")))
  }
  var.name <- names(as.data.frame(resid))
  var.names <- substr(paste(paste(var.name,"+",sep = ""),collapse = ""),1,nchar(paste(paste(var.name,"+",sep = ""),collapse = ""))-1)

  model <- paste('dimension  =~',var.names)
  if(is.polychor){
    unidim <- cfa(model, data=resid, missing = "pairwise", ordered = c(var.name))
  } else {
    unidim <- cfa(model, data=resid, missing = "pairwise")
  }

  fit.unidim <- fitMeasures(unidim, c("cfi.scaled","tli.scaled","rmsea.scaled","srmr","chisq","pvalue"))


  cfa.cfi.scaled <- fit.unidim[1]
  cfa.tli.scaled <- fit.unidim[2]
  cfa.rmsea.scaled <- fit.unidim[3]
  cfa.srmr <- fit.unidim[4]
  cfa.chisq <- fit.unidim[5]
  cfa.pvalue <- fit.unidim[6]

  MLplot <- list("unidim" = unidim,"cfi.scaled" = cfa.cfi.scaled,"tli.scaled" = cfa.tli.scaled,
                 "rmsea.scaled" = cfa.rmsea.scaled,"srmr" = cfa.srmr,"chisq"=cfa.chisq,"pvalue" = cfa.pvalue)

  options(warn = defaultW)


  return(MLplot)

}
