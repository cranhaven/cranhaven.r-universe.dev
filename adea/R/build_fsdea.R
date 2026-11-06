#' Build fsdea problem
#'
#' For the given input and output build the feature selection problem and return it.
#'
#' This function implement the feature selection method in the article [Benitez-Pena, S., Bogetoft, P. and Romero Morales, D. *Feature Selection in Data Envelopment Analysis: A Mathematical Optimization approach* Omega, Elsevier BV, **2020**, Vol. 96, pp. 102068](http://www.sciencedirect.com/science/article/pii/S0305048318312131)
#'
#' Note: As this function is mainly for internal use, to avoid unnecessary overload, it does not do an extensive check of the input parameters. Use the higher level fsdea function instead.
#' 
#' @name build_fsdea
#' @aliases build_fsdea
#' @keywords internal
#' @inheritParams adea
#' @param ninputs a number which is the number input features, variables, to be selected. Its default value is the number of input variables.
#' @param noutputs a number which is the number output features, variables, to be selected. Its default value is the number of output variables.
#' @param nvariables a number of total features, variables, to be selected. Only if both ninputs and noutputs are omitted. In other case it is ignored.
#' @return lp
build_fsdea <- function(input, output, orientation = c('input', 'output'), ninputs, noutputs, nvariables, lp = NULL)
{
    ## Check input and output
    ## err <- adea_check(input, output)
    ## if (err != TRUE) stop(err)
    orientation <- match.arg(orientation)
    ## Initialize values
    ndmu <- nrow(input)
    ni <- ncol(input)
    no <- ncol(output)
    nio <- ni + no

    if (missing(lp) || is.null(lp)) {
        lp <- roi_build_dea(input = input, output = output, orientation = orientation)
    } else {
        ## Paranoid check
        ## cat('build_fsdea:build_fsdea.R:33: debug: test result', attr(lp, 'adea', exact = TRUE)$lp == 'roi_dea', '\n')
    }
    
    ## If not missed check ninputs values
    if (ninputs < 1) stop('ninputs must be at least 1')
    if (ninputs > ni) stop('ninputs must be at most the number of input variables')
    ## If not missed check noutputs values
    if (noutputs < 1) stop('noutputs must be at least 1')
    if (noutputs > no) stop('noutputs must be at most the number of output variables')
    ## If not missed check nvariables values
    if (nvariables < 2) stop('nvariables must be at least 2')
    if (nvariables > nio) stop('nvariables must be at most the total number of variables')
    ## Build standard DEA problem
    lp <- roi_build_dea(input = input, output = output, orientation = orientation)

    ## Update constraint matrix dimension and number of variables
    ## Standard DEA
    ## nrow <- (ndmu + 1) * ndmu
    ## ncol <- nio * ndmu
    ## nrow is the number of rows to add
    nrow <- nio * ndmu + 3
    ncol <- nio * ndmu + nio
    lp$constraints$L$ncol <- ncol
    lp$n_of_variables <- ncol
    ## Debug
    ## cat('debug:solve_fsdea:solve_fsdea.R:75: ni:      ', ni, '\n')
    ## cat('debug:solve_fsdea:solve_fsdea.R:76: ninputs: ', ninputs, '\n')
    ## cat('debug:solve_fsdea:solve_fsdea.R:77: no:      ', no, '\n')
    ## cat('debug:solve_fsdea:solve_fsdea.R:77: noutputs:', noutputs, '\n')
    ## cat('debug:solve_fsdea:solve_fsdea.R:79: ndmu:    ', ndmu, '\n')
    ## cat('debug:solve_fsdea:solve_fsdea.R:80: nrow:    ', nrow, '\n')
    ## cat('debug:solve_fsdea:solve_fsdea.R:81: ncol:    ', ncol, '\n')
    
    ## Build lp_matrix as sparse matrix as in package slam, only new rows
    L <- simple_triplet_zero_matrix(nrow = nrow, ncol = ncol)
    ## Columns 
    ## * Weights for outputs of first DMU
    ## * Weights for inputs of second DMU
    ## * ...
    ## * Aditional variables, in this case feature selection variables, first output, second input
    ## Build constraints:
    ## * First block normalization constraints (from Standard DEA)
    ## * Second block efficiencies (from Standard DEA)
    ## * Third block feature selection
    ## * Fourth block number of features, first for noutputs, second for ninputs, and third for nvariables
    ## RHS
    ## Initialize new rows at rhs
    rhs <- rep(0, nrow)
    ## rhs of number of variables constraints
    rhs[nrow - 2] <- noutputs
    rhs[nrow- 1] <- ninputs
    rhs[nrow] <- nvariables

    ## Constraint type
    dir <- rep('<=', nrow)
    dir[nrow - 0:2] <- '<='
    
    ## Build feature selection constraints
    ## Instead of build it as x <= M z, as taking into account that M can be set to 1/input.
    ## They are implemented as input x <= z
    l <- 1
    for(u in 1:ndmu)
        for(i in 1:nio) {
            L[l, l] <- ifelse(i <= no, output[u, i], input[u, i - no])
            L[l, i + ndmu * nio] <- -1
            ##L[l, l] <- 1
            ##k <- ifelse(i <= ni, input[u, i], output[u, i - ni])
            ##if (k > 1e-8) L[l, i + ndmu * nio] <- -1/k
            l <- l +1
        }
    ## Build number of feature constraints
    L[nrow - 2, seq(ncol - nio + 1, ncol - ni)] <- 1
    L[nrow - 1, seq(ncol - ni + 1, ncol)] <- 1
    L[nrow, seq(ncol - nio + 1, ncol)] <- 1

    ## Rebuild, resize, objective (Same as standard DEA but a constant)
    lp$objective$L$ncol <- ncol
    
    ## Replace constraints
    names <- c(lp$constraints$names, paste0('O_', 1:no), paste0('I_', 1:ni))
    lp$constraints$names <- names
    constraints <- L_constraint(L = L, dir = dir, rhs = rhs, names = names)
    constraints(lp) <- rbind(constraints(lp), constraints)

    ## Set variable types
    types(lp) <- c(rep('C', ncol - nio), rep('B', nio))
    
    ## Write model to file
    ## ROI_write(lp, file = '/tmp/solve_fsdea.mod', type = 'lp_cplex')

    ## Rebuild entire lp
    ## objective <- as.vector(lp$objective$L)
    ## rhs <- lp$constraints$rhs
    ## constraints <- L_constraint(L = lp$constraints$L, dir = lp$constraints$dir, rhs = rhs)
    ##lp <- OP(objective = as.vector(lp$objetive$L), constraints = lp$constraints, max = FALSE)
    
    ## Return
    attr(lp, 'adea')$lp <- 'fsdea'
    lp
}
