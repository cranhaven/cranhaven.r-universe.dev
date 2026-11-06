#' Build adea problem
#'
#' For the given input and output build the adea problem and return it.
#'
#' Note: As this function is mainly for internal use, to avoid unnecessary overload, it does not do an extensive check of the input parameters. Use the higher level adea function instead.
#' 
#' @name roi_adea
#' @aliases roi_adea
#' @keywords internal
#' @inheritParams adea
#' @param solver Any of the available solver that will be used by ROI to solve the optimization problem. Use <code>ROI_installed_solvers()</code> to list them.
#' @return list(lp, status, solver, ux, vy, eff, loads = list())
roi_adea <- function(input, output, orientation, load.orientation, solver)
{
    ## Check input and output
    ## err <- adea_check(input, output)
    ## if (err != TRUE) stop(err)
    orientation <- match.arg(orientation, c('input', 'output'))
    load.orientation <- match.arg(load.orientation, c('inoutput', 'input', 'output'))
    ## Initialize values
    ## status <- NULL
    ## ux <- NULL
    ## vy <- NULL
    ## eff <- NULL
    ## ndmu <- nrow(input)
    ## ni <- ncol(input)
    ## no <- ncol(output)
    ## nio <- ni + no
    
    ## Solve dea problem
    .dea <- roi_dea(input = input, output = output, orientation = orientation, solver = solver)

    ## Build adea problem
    lp <- roi_build_adea(input = input, output = output, eff = .dea$eff, orientation = orientation, load.orientation = load.orientation, solver = solver, lp = .dea$lp)

    ## Solve adea problem
    .adea <- roi_solve_adea(input = input, output = output, solver = solver, lp = lp)
    
    ## Compute scores
    .adea$eff <- eff_dea(input = input, output = output, orientation = orientation, ux = .adea$ux, vy = .adea$vy)
    
    ## Compute loads
    .adea$loads <- adea_loads(input, output, .adea$ux, .adea$vy, load.orientation = load.orientation)
    
    ## Return lp
    .adea$lp <- lp

    ## Writing lp problem (This write operation may rise an strange error)
    ## cat("debug:roi_adea:roi_adea.R:49: writing lp problem to /tmp/adea_roi.lp...\n")
    ## ROI_write(lp, file = '/tmp/adea_roi.lp', type = 'lp_lpsolve')
    
    ## Return
    class(.adea) <- 'roiadea'
    .adea
}
