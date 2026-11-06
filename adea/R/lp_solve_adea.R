#' Build adea problem
#'
#' For the given input and output build the lp_solve problem and return it.
#'
#' @name lp_solve_adea
#' @aliases lp_solve_adea
#' @keywords internal
#' @inheritParams adea
#' @param solve If TRUE then solve dea model
#' @param eff The efficiency scores from dea analysis.
#' @param lp The problem returned from lp_solve.dea or NULL
#' @return lp adea problem for the given input, output and scores
lp_solve_adea <- function(input, output, eff = NULL, orientation, load.orientation, solve = FALSE, lp = NULL)
{
    ## Check input and output
    err <- adea_check(input = input, output = output, eff = eff)
    if (err != TRUE) stop(err)
    orientation <- match.arg(orientation, c('input', 'output'))
    load.orientation <- match.arg(load.orientation, c('inoutput', 'input', 'output'))

    ## Check if lp is provided if not, build as first stage
    if (missing(lp) || is.null(lp)) lp <- lp_solve_dea(input = input, output = output, orientation = orientation)$lp

    ## Check for vectors and build matrix
    ## if (is.vector(input)) input <- matrix(input, ncol = 1)
    ## if (is.vector(output)) output <- matrix(output, ncol = 1)

    ## Initialise values
    status <- NULL
    ux <- NULL
    vy <- NULL
    ndmu <- .adea_check(input)
    ni <- ncol(input)
    no <- ncol(output)
    nio <- ni + no
    
    ## Number of alpha variables
    ## nalpha <- switch(load.orientation,
    ##                 input = ni,
    ##                 output = no,
    ##                 inoutput = nio)

    ## Compute the size of new problem
    ## The variables are: weights for each DMU + alpha + level
    ## To make it easy keep all alpha columns, for inputs and outputs, even when they are not uses
    ncol <- ndmu * nio + nio + 1

    ## The rows are: (ndmu + ndmu^2) + (ndmu + nio)
    nrow <- (ndmu + 2) * ndmu + nio

    ## Add alpha columns
    lpSolveAPI::resize.lp(lp, nrow, ncol) # Resize do not really add columns just add space
    for (i in 1:(nio+1)) lpSolveAPI::add.column(lp, c(0), c(1))

    ## Set objective function
    lpSolveAPI::set.objfn(lp, c(-1), c(ncol))

    ## Change to on row.add.mode improves the performance
    ## Objective function must be before to activate row.add.mode
    ## Seems not to be available after solved
    ## row.add.mode(lp, state = 'on')
    ## Add rows for scores constraints
    if (!is.null(eff)) {
        if (orientation == 'input') {
            for (i in 1:ndmu)
                lpSolveAPI::add.constraint(lp, output[i, ], type = '=', rhs = eff[i], ((i-1) * nio + 1):((i -1) * nio + no))
        }
        if (orientation == 'output') {
            for (i in 1:ndmu)
                lpSolveAPI::add.constraint(lp, input[i, ], type = '=', rhs = eff[i], ((i-1) * nio + 1 + no):((i -1) * nio + ni + no))
        }
    }

    ## Add alpha constraints
    lp_solve_add_alpha_constraints(lp = lp, input = input, output = output, eff = eff, orientation = orientation, load.orientation = load.orientation)
    
    ## Add alpha level constraints
    if (load.orientation == 'input' || load.orientation == 'inoutput') {
        for (i in 1:ni)
            lpSolveAPI::add.constraint(lp, c(-1 , 1), type = '<=', rhs = 0, c(ndmu * nio + i, ncol))
    }
    if (load.orientation == 'output' || load.orientation == 'inoutput') {
        for (i in 1:no)
            lpSolveAPI::add.constraint(lp, c(-1 , 1), type = '<=', rhs = 0, c(ndmu * nio + ni + i, ncol))
    }

    ## Change to off row.add.mode
    ##row.add.mode(lp, state = 'off')

    ## Solve the model
    if (solve) {
        
        ## Set some options to avoid numeric problems (testing only)
        ## lp.control(lp, verbose = 'full', basis.crash = 'leastdegenerate', simplextype = c('primal', 'primal'))
        
        ## This seems to be the best value for scaling
        ## lp.control(lp, scaling = c("geometric", "dynupdate"))
        lpSolveAPI::lp.control(lp, scaling = c("none"))
        
        ## Call to solver
        status = solve(lp)

        ## Check return status (drop alpha values)
        if (status != 0) stop(paste0(gettext('Unable to solve adea model. Solver status is '), status, '. (', lpsolve.status.txt[status + 1], '.)'))

        ## Get u and v values
        sol <- lpSolveAPI::get.variables(lp)[1:(ndmu * nio)]
        sol <- matrix(sol, nrow = ndmu, ncol = nio, byrow = TRUE)

        ## Get weights for outputs, named vy for compatibility with Benchmarking package
        vy <- sol[, 1:no, drop = FALSE]
        colnames(vy) <- colnames(output)
        rownames(vy) <- rownames(output)

        ## Get weights for inputs, named ux for compatibility with Benchmarking package
        ux = sol[, (no+1):nio, drop = FALSE]
        colnames(ux) <- colnames(input)
        rownames(ux) <- rownames(input)
        
        ## Compute scores
        eff <- switch(orientation,
                      input = rowSums(vy * output),
                      output = rowSums(ux * input)
                      )
        names(eff) <- rownames(input)
    }

    ## Return the list values
    list(lp = lp, status = status, ux = ux, vy = vy, eff = eff);
}

## Auxiliary function to add alpha constraint to an adea model
lp_solve_add_alpha_constraints <- function(lp, input, output, eff, orientation, load.orientation) {

    ## Compute needed numbers
    ndmu <- nrow(input)
    ni <- ncol(input)
    no <- ncol(output)
    nio <- ni + no
    
    ## Add alpha constraints for inputs
    if (load.orientation == 'input' || load.orientation == 'inoutput') {
        for (i in 1:ni)
        {
            xt <- switch(orientation,
                         input = c(input[, i] * ni / ndmu, -1),
                         output = c(input[, i] * ni / sum(eff), -1)
                         )
            indices <- seq(no + i, ndmu * nio, by = nio)
            indices <- c(indices, ndmu * nio + i)
            lpSolveAPI::add.constraint(lp, xt, type = '=', rhs = 0, indices = indices)
        }
    }
    ## Add alpha constraints for outputs
    if (load.orientation == 'output' || load.orientation == 'inoutput') {
        for (i in 1:no)
        {
            xt <- switch(orientation,
                         input = c(output[, i] * no / sum(eff), -1),
                         output = c(output[, i] * no / ndmu, -1)
                         )
            indices <- seq(i, ndmu * nio, by = nio)
            indices <- c(indices, ndmu * nio + i + ni)
            lpSolveAPI::add.constraint(lp, xt, type = '=', rhs = 0, indices = indices)
        }
    }
}

## Definition of the lpsolve status text messages
lpsolve.status.txt <- c(gettext("Optimal solution found"),
                        gettext("The model is sub-optimal"),
                        gettext("The model is infeasible"),
                        gettext("The model is unbounded"),
                        gettext("The model is degenerate"),
                        gettext("Numerical failure encountered"),
                        gettext("Process aborted"),
                        gettext("Timeout"),
                        gettext("The model was solved by presolve"),
                        gettext("The branch and bound routine failed"),
                        gettext("The branch and bound was stopped because of a break-at-first or break-at-value"),
                        gettext("A feasible branch and bound solution was found"),
                        gettext("No feasible branch and bound solution was found"))

