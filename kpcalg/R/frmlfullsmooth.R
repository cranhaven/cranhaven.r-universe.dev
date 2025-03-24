#' Formula for GAM with crossterms
#'
#' Creates a formula for \link{gam} to be used in \link{regrXonS}. For data \eqn{X=(X_1,...X_n,X_{n+1},...,X_m)}, variable to be regressed \eqn{X_{i}}, i=1...n and variables to regress on \eqn{S={X_{n+1},...,X_m}} creates formula \eqn{X_i \sim s(X_{n+1},...,X_m}{X_i ~ s(X_{n+1},...,X_m}.
#'
#' @param target.ind integer, number for the variable to be regressed
#' @param pred.inds integer(s), number(s) for the variable(s) on which we regress
#' @param var.str name of variables used to create formula, default is "x"
#' @importFrom stats formula as.formula
#' @import methods
#' @export
#' @return formula.full.smooth() returns a formula \eqn{X_i \sim s(X_{n+1},...,X_m)}{X_i ~ s(X_{n+1},...,X_m)}
#' @author Petras Verbyla (\email{petras.verbyla@mrc-bsu.cam.ac.uk})
#' @seealso \link{regrXonS}

frml.full.smooth <- function(target.ind,pred.inds,var.str="x")
{
  as.formula(paste(c(var.str,target.ind," ~ s(",paste(paste(var.str,pred.inds,sep=""),collapse=","),")",collapse=""),collapse=""))
}
