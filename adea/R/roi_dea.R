#' Build (standard) DEA problem using ROI infraestructure
#'
#' This function works like dea but without strict parameter check.
#'
#' Unlike the dea function, this function expects input and output to be arrays or data.frames, not vectors.
#'
#' Note: As this function is mainly for internal use, to avoid unnecessary overload, it does not do an extensive check of the input parameters. Use the higher level dea function instead.
#' 
#' @name roi_dea
#' @aliases roi_dea
#' @keywords internal
#' @inheritParams adea
#' @param solver a solver to be used by ROI to solve the DEA optimization problem. Use <code>ROI_installed_solvers()</code> to list them.
#' @return list(lp, status, solver, ux, vy, eff)
roi_dea <- function(input, output, orientation, solver)
{
    ## Check input and output
    ## err <- adea_check(input, output)
    ## if (err != TRUE) stop(err)
    orientation <- match.arg(orientation, c('input', 'output'))

    ## Build lp problem
    lp <- roi_build_dea(input = input, output = output, orientation = orientation)

    ## Solve it
    .dea <- roi_solve_dea(input = input, output = output, orientation = orientation, lp, solver = solver)
    
    ## Compute scores
    .dea$eff <- eff_dea(input = input, output = output, orientation = orientation, ux = .dea$ux, vy = .dea$vy)

    ## Return lp
    .dea$lp <- lp

    ## Return the list values
    ##.dea <- list(lp = lp, orientation = orientation, status = status, solver = solver, ux = ux, vy = vy, eff = eff)
    class(.dea) <- 'dea'
    .dea
}
