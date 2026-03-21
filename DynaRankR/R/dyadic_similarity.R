#' Compare two rank orderings 
#' 
#' This function compares two rank orderings that have the same elements. 
#'      For an order of length \emph{n} 
#'      there are \emph{n choose 2} dyadic relationships implied by that order. 
#'      For example, the order {a, b, c} implies that a > b, a  > c, and b > c.
#'      The dyadic similarity between two orders is the proportion of implied 
#'      dyadic relationships that are shared by the two orders. 
#' 
#' @param order1 The first rank ordering to be compared. Alternatively, this can
#'               be supplied as an interaction matrix with identities as the dimension names.
#'               All identities in order1 must be in order2. 
#' @param order2 The second rank ordering to be compared. Alternatively, this can
#'               be supplied as an interaction matrix with identities as the dimension names. 
#'               All identities in order2 must be in order1.
#' 
#' @return The proportion of dyadic relationships that are shared by the two orders.
#'         This value is 1 if the orders are identical and 0 if the orders are
#'         exact opposites. 
#' 
#' @examples 
#' dyadic_similarity(letters[1:20], letters[1:20]) #identical orders
#' dyadic_similarity(letters[1:20], letters[20:1]) #opposite orders
#' dyadic_similarity(sample(letters[1:20]), sample(letters[1:20])) #random orders
#' 
#' @export
#' 
dyadic_similarity <- function(order1, order2){
  if('matrix' %in% class(order1)[1]){
    ##Make sure that matrix supplied is square with identical orders
    if(any(dimnames(order1)[[2]] != dimnames(order1)[[1]])){
      stop('matrix supplied has different individuals in rows and columns')
    }
    order1 <- make_full_matrix(dimnames(order1)[[1]])
  }else{order1 <- make_full_matrix(order1)}
  if('matrix' %in% class(order2)[1]){
    if(any(dimnames(order2)[[2]] != dimnames(order2)[[1]])){
      stop('matrix supplied has different individuals in rows and columns')
    }
    order2 <- make_full_matrix(dimnames(order2)[[1]])
  }else{order2 <- make_full_matrix(order2)}
  
  ##Make sure all individuals are shared by the two orders
  if(any(!dimnames(order1)[[1]] %in% dimnames(order2)[[1]]) | 
         any(!dimnames(order2)[[1]] %in% dimnames(order1)[[1]])){
    stop("can\'t compare orders with non-shared individuals")
  }
  
  num_same <- sum(order2[dimnames(order1)[[1]], dimnames(order1)[[1]]] == order1) - nrow(order1)
  return((num_same/(length(order1)-nrow(order1))))
}
