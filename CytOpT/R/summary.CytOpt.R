#'CytOpt summary
#'
#'summary S3 method for CytOpt object
#'
#'@param object	an object of class \code{CytOpt} to summarized.
#'@param ... further arguments passed to or from other methods. Not implemented.
#'
#'@return a \code{list} object
#'
#'@method summary CytOpt
#'@export
#'
#'@examples
#'if(interactive()){
#'
#'res <- CytOpT(X_s = HIPC_Stanford_1228_1A, X_t = HIPC_Stanford_1369_1A, 
#'              Lab_source = HIPC_Stanford_1228_1A_labels,
#'              eps = 0.0001, lbd = 0.0001, n_iter = 10000, n_stoc=10,
#'              step_grad = 10, step = 5, power = 0.99, 
#'              method='minmax', monitoring=TRUE)
#'summary(res)
#'
#'}


summary.CytOpt <- function(object, ...){
  s <- list()
  s[["proportions"]] <- object$proportions

  if(!is.null(s$proportions$Gold_standard)){
    s[["KLdiv"]] <- lapply(object$monitoring, function(x){x[length(x)]})
    s[["n_iter"]] <- lapply(object$monitoring, function(x){length(x)})
  }else{
    s[["KLdiv"]] <- NULL
  }
  
  class(s) <- "summary.CytOpt"
  
  return(s)
}

#'CytOpt print summary
#'
#'print S3 method for summary.CytOpt object
#'
#'@param x	an object of class \code{summary.CytOpt} to print.
#'@param ... further arguments passed to or from other methods. Not implemented.
#'
#'@return NULL
#'
#'@method print summary.CytOpt
#'@export

print.summary.CytOpt <- function(x, ...){
  method <- gsub("MinMax", "MinMax swapping", 
                 gsub("Descent_ascent", "Descent-Ascent", colnames(x$proportions)
                 ))
  if(method[1] == "Gold_standard"){
    method <- method[-1]
  }
  
  cat("Estimation of cell proportions with", paste0(method, collapse = " and "), "from CytOpt:\n")
  
  print(x$proportions)
  
  if(length(x$KLdiv)>0){
    KLdivs <- unlist(x$KLdiv)
    names(KLdivs) <- gsub("MinMax", "MinMax swapping", 
                          gsub("Descent_ascent", "Descent-Ascent", names(KLdivs)
                          ))
    cat("\nFinal Kullback-Leibler divergences:\n")
    print(KLdivs)
    
    niter <- unlist(x$n_iter)
    names(niter) <- gsub("MinMax", "MinMax swapping", 
                          gsub("Descent_ascent", "Descent-Ascent", names(niter)
                          ))
    cat("Number of iterations:\n")
    print(unlist(niter))
  }
}