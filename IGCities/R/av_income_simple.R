#' Computes average income in each location, which is the weighted average of
#' the income of the people living in the location.
#'
#' @param lambda_ij_i NxN matrix - Probability of individuals in each
#'     location of working in each location.
#' @param w_tr NxS - Wages in each location in each sector.
av_income_simple = function(lambda_ij_i,
                            w_tr){
  
  y_bar = (sumDims2(array_operator(lambda_ij_i, w_tr, '*'), 2));
  return(list(y_bar=y_bar))
}