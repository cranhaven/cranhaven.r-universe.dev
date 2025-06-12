#' @description
#' Natural strata fix a constant ratio of controls to treated units within
#' each stratum. This ratio need not be an integer. The control units are
#' chosen using randomized rounding of a linear program that balances many
#' covariates.
#' To solve the linear program, the 'Gurobi' commercial optimization software
#' is recommended, but not required. The 'gurobi' R package can be installed following
#' the instructions \href{https://www.gurobi.com/documentation/9.1/refman/ins_the_r_package.html}{here}.
#'
#' @details
#' To achieve the desired ratio of control to treated units,
#' a subset of control units are
#' chosen using by optimizing the balance of many covariates using
#' either randomized rounding of a linear program or
#' an integer program. The main function in this package is \code{\link{optimize_controls}()}.
#' To create the input constraints for this function, you should use
#' \code{\link{generate_constraints}()}.
#'
#' @keywords internal
"_PACKAGE"
