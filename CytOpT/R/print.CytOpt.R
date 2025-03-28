#'CytOpt print
#'
#'print S3 method for CytOpt object
#'
#'@param x an object of class \code{CytOpt} to print.
#'@param ... further arguments passed to or from other methods. Not implemented.
#'
#'@return the proportions \code{data.frame} from \code{x}
#'
#'@method print CytOpt
#'@export
#'@examples
#'if(interactive()){
#'
#'res <- CytOpT(X_s = HIPC_Stanford_1228_1A, X_t = HIPC_Stanford_1369_1A, 
#'              Lab_source = HIPC_Stanford_1228_1A_labels,
#'              eps = 0.0001, lbd = 0.0001, n_iter = 10000, n_stoc=10,
#'              step_grad = 10, step = 5, power = 0.99, 
#'              method='minmax')
#'print(res)
#'
#'}

print.CytOpt <- function(x, ...){
  cat("A CytOpt object\n\n")
  cat("Cell proportions:\n")
  print(x$proportions)
}
