update.expectreg <-
function(object, add_formula = NULL,  
                                  data = NULL, estimate = NULL, smooth = NULL,
                                  lambda = NULL, expectiles = NULL, 
                                  delta_garrote = NULL, ci = NULL, ...) {
    
    
    if(is.null(data)) {
        data <- object$data
        }
    if(is.null(estimate)) {
        estimate <- class(object)[2]
        }
    if(is.null(smooth)) {
        smooth <- object$smooth_orig
        }
    if(is.null(expectiles)) {
        expectiles <- object$asymmetries
        }
    if(is.null(lambda)) {
        lambda <- 1
        }
    if(is.null(ci)) {
        ci <- FALSE
        if(!is.null(object$covmat)) {ci <- TRUE}
        }
    if(is.null(add_formula)) {
        formula_in <- object$formula
    } else {
        formula_in <- update.formula(object$formula,add_formula)
    }
    
    expectreg.ls(formula = formula_in,data=data,estimate=estimate,smooth=smooth,lambda=lambda,expectiles=expectiles,ci=ci,delta_garrote=delta_garrote)
    }
