#' Build dea problem
#'
#' For the given input and output build the lp_solve problem and return it.
#'
#' @name lp_solve_dea
#' @aliases lp_solve_dea
#' @keywords internal
#' @inheritParams adea
#' @param solve If TRUE then solve dea model
#' @return list(lp, status, ux, vy, eff)
lp_solve_dea <- function(input, output, orientation = c('input', 'output'), solve = FALSE)
{
    ## Inform about deprecation
    warning('lp_solve_dea is deprecated and will be deleted in a future version of the package')
    ## Check input and output
    err <- adea_check(input, output)
    if (err != TRUE) stop(err)
    orientation <- match.arg(orientation)
    ## Initialise values
    status <- NULL
    ux <- NULL
    vy <- NULL
    eff <- NULL
    ndmu <- nrow(input)
    ni <- ncol(input)
    no <- ncol(output)
    nio <- ni + no
    nrow <- (ndmu + 1) * ndmu
    ncol <- nio * ndmu
    ## Build lp
    lp <- lpSolveAPI::make.lp(nrow = nrow, ncol = ncol)
    ## RHS
    lpSolveAPI::set.rhs(lp, rep(1, ndmu), 1:ndmu)
    ## Constraint type
    lpSolveAPI::set.constr.type(lp, rep('<=', nrow))
    lpSolveAPI::set.constr.type(lp, rep('=', ndmu), 1:ndmu)
    ## Build efficiency conditions
    ## Build output, vy, part matrix, i.e. the first ndmu x no columns
    for (u in 1:ndmu)
        for (j in 1:no)
            lpSolveAPI::set.column(lp, (u - 1) * nio + j, output[, j], seq(ndmu + u, nrow, by = ndmu))
    ## Build input, ux, part matrix, i.e. the last ndmu x ni columns
    for (u in 1:ndmu)
        for (i in 1:ni)
            lpSolveAPI::set.column(lp, (u - 1) * nio + i + no, -input[, i], seq(ndmu + u, nrow, by = ndmu))
    ## Build normalisations
    if (orientation == 'input') {
        for (u in 1:ndmu)
            for (i in 1:ni)
                lpSolveAPI::set.mat(lp, u, (u - 1) * nio + i + no, input[u, i])
    } else if (orientation == 'output') {
        for (u in 1:ndmu)
            for (j in 1:no)
                lpSolveAPI::set.mat(lp, u, (u - 1) * nio + j, output[u, j])
    } else {
        stop('orientation:', orientation, 'not implemented yet.\n')
    }
    ## Build objective
    if (orientation == 'input') {
        objective <- -c(rbind(t(output), matrix(0, nrow = ni, ncol = ndmu)))
    } else if  (orientation == 'output') {
        objective <- c(rbind(matrix(0, nrow = no, ncol = ndmu), t(input)))
    } else {
        stop('orientation:', orientation, 'not implemented yet.\n')
    }
    lpSolveAPI::set.objfn(lp, objective)

    ## Solve the model
    if (solve) {
        ## Call to solver
        status = solve(lp)
        ## Check return status
        if (status != 0) stop(paste(gettext('Unable to solve dea model. Solver status is '), status))
        ## Get u and v values
        sol <- lpSolveAPI::get.variables(lp)
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
    list(lp = lp, status = status, ux = ux, vy = vy, eff = eff)
}
