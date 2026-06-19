#' Simulate Data
#'
#' A function to simulate data based on a multinomial vector parameter vector or
#' a list of parameter vectors.
#'
#' @param N Integer. Number of observations.
#' @param nQ Integer. Number of questions.
#' @param pi Numeric vector. Vector of probabilities adding up to 1; it is recommended that names of elements are character strings.
#' Alternatively, pi can be list of vectors as previously described with length equal to `nQ`. Notice that the list elements need not have same vector names. The order of pi vectors in the list will be reflected in the resulting simulated matrix. This alternative ideally assumes that questions are independently distributed.
#'
#' @returns N x nQ matrix with simulated categories distributed according to vector pi
#' @export
#'
#' @examples
#' Pix <- setNames(c(0.1, 0.2, 0.3, 0.4, 0), paste0('a', 1:5))
#' X <- SimData(N=10, nQ=5, Pix)
#' head(X)
#'
#' Piy <- setNames(c(0.3, 0.2, 0.4, 0, 0.1), paste0('a', 1:5))
#' Y <- SimData(N=10, nQ=3, Piy)
#' head(Y)
#'
#' PiZ <- list(x1 = Pix, x2 = Pix, y1 = Piy, y2 = Piy)
#' Z <- SimData(N=10, nQ=length(PiZ), PiZ)
#'
SimData <- function(N, nQ, pi){

   ## if(class(pi) == 'numeric'){
   if(is.numeric(pi)){
      out <- matrix(sample(names(pi), N*nQ, replace=TRUE, prob = pi),
                    nrow = N,
                    dimnames = list(paste0('s', 1:N),
                                    paste0('q', 1:nQ)))
      return(out)
   ## }else if(class(pi) == 'list' & length(pi) == nQ){
   }else if(is.list(pi) & length(pi) == nQ){
      out <-lapply(pi, function(y) sample(x = names(y), size = N, replace=TRUE, prob = y)) %>%
         do.call(what = cbind)
      rownames(out) <- paste0('s', 1:N)

      if(all( class(names(pi)) == 'character' ) )
         colnames(out) <- names(pi)
      else
         colnames(out) <- paste0('1', 1:nQ)

      return(out)
   }else{
      message('Either pi is numeric vector or a list with numeric vectors of length equal to nQ.')
   }

}
