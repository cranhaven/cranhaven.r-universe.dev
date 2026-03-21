#' @title Reliability Growth Cost
#'
#' @description Define the cost function of reliabiltiy growth (RG) after the decision of the test (for binomial RDT).
#'
#' @param G A constant value reliabilty growth cost, suggest to be sufficiently larger than RDT cost.
#' @return Reliability growth cost
#' @examples
#' bcost_RG(G = 100000);
#' @seealso \code{\link{bcost_RDT}}, \code{\link{bcost_WS}}, \code{\link{bcost_expected}}
#' @export

bcost_RG <- function(G){
  return(G)
}
