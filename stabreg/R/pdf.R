.EPS <- 1e-16
.REGR_NAMES <- c("loc", "scale", "alpha", "beta")



stable_pdf_fourier_integral <- function(x, a, b){
    stopifnot(a <= 2)
    .Call("R_stable_pdf_fourier_integral", x, a, b)
}

stable_sym_pdf_fourier_integral <- function(x, a){
    stopifnot(a <= 2)
    .Call("R_stable_sym_pdf_fourier_integral", x, a)
}


stable_sym_pdf <- function(x, a){
    stopifnot(a <= 2)
    .Call("R_stable_sym_pdf", x, a)
}



#works only for positive x
stable_pdf_series_infinity <- function(x, a, b){

    n_inf <- NULL
    eps <- 1e-16

    if(a >= 1.1)
        n_inf <- 80L
    else if(0.9 < a | a < 0.5)
        stop('Error: .9 < alpha < 1.1 if beta != 0, and alpha < .5 not supported')
    else if(0.5 <= a)
        n_inf <- 90L

    z = -b * tan((pi*a)/2);

    min_inf_x <- ( (1+z^2)^(n_inf/2) * a /(pi * eps) * gamma(a*n_inf)/gamma(n_inf) )^(1/(a*n_inf-1))
    min_inf_x <- min_inf_x + z
    inf_cond <- min_inf_x < x

    x_infcond <- x[inf_cond]

    pdf_infcond <- .Call("R_stable_pdf_series_infinity", x_infcond, a, b, as.integer(n_inf))

    pdf <- rep(NA_real_, length(x))
    pdf[inf_cond] <- pdf_infcond
    pdf
}



get_n_inf <- function(a){
    n <- NA_integer_
    if(1.1 <= a)
        n <- 80L
    else if(0.9 < a | a < 0.5)
        stop("Unsupported alpha value: ", a)
    else if(0.5 <= a)
        n <- 90L
    else
        stop("Unsupported alpha value: ", a)
    n
}


stable_pdf <- function(x, a, b){
    if(length(a) > 1 | length(b) > 1){
        return(.Call("R_stable_pdf_iter_singleobs", x, a, b));
    }

    if(a < 0.5 | (b != 0 & a > 0.9 & a < 1.1 ) | a > 2)
        stop("Parameter configuration not supported: a = ", a, ", b = ", b)

    if(b == 0){
        .Call("R_stable_sym_pdf", x, a)
    }
    else if(a == 2){
        dnorm(x, sd = sqrt(2))
    }
    else{
        z <- -b * tan((pi*a)/2)
        pdf <- rep(NA_real_, length(x))
        xlz_cond <- x < z

        if(any(xlz_cond))
            pdf[xlz_cond] = stable_pdf(-x[xlz_cond], a, -b)

        if(any(!xlz_cond)){
            n_inf <- get_n_inf(a)
            min_inf_x <- ( (1+z^2)^(n_inf/2) * a /(pi * .EPS) * gamma(a*n_inf)/gamma(n_inf) )^(1/(a*n_inf-1))

            min_inf_x <- min_inf_x + z
            inf_cond <- min_inf_x < x

            if(any(inf_cond))
                pdf[inf_cond] <- .Call("R_stable_pdf_series_infinity", x[inf_cond], a, b, as.integer(n_inf))

            fourier_cond <- (z < x) & !inf_cond


            if(any(fourier_cond)){
                pdf[fourier_cond] <- .Call("R_stable_pdf_fourier_integral", x[fourier_cond], a, b)
            }
        }
        pdf
    }
}


stable_pdf_singleobs <- function(x, a, b){
    .Call("R_stable_pdf_singleobs", x, a, b)
}


stable_mle_fit <- function(x, init_vals = NULL, trace = FALSE){
    if(anyNA(x))
        stop("NAs in x not allowed")

    objf <- function(p){
        -(-length(x)*log(p[3]) + sum(log( stable_pdf((x - p[4])/p[3], p[1], p[2])  )))
    }

    init_loc <- 0#median(x)
    init_scale <- mad(x, na.rm = TRUE)

    lower_bounds <- c(1.1, -0.99999, init_scale / 10, min(x))
    upper_bounds <- c(2, 0.99999, init_scale * 10, max(x))

    if(is.null(init_vals))
        init_vals <- c(1.75, 0, init_scale, init_loc)

    r <- nlminb(objective = objf, start = init_vals,
                lower = lower_bounds, upper = upper_bounds,
                control = list(trace = trace))

    if(r$convergence != 0)
        warning("Optimization did not converge")


    list(par = structure(r$par, names = c("alpha", "beta", "scale", "loc")),
         log_lik = -r$objective,
         aic = 8 + 2*r$objective)

}




stable_lm <- function(formula, data, trace = 0, output_se = TRUE,
                      calc_confbounds = FALSE, conf = 0.95, optim_control = list())
{
    if(calc_confbounds)
        output_se <- TRUE # need these to set search region for conf bounds


    mf <- model.frame(formula = formula, data = data)
    X <- model.matrix(attr(mf, "terms"), mf)
    y <- model.response(mf)
    n_regr <- ncol(X)

    objf <- function(p){
        #compute residuals
        resids <- y - c(X %*% p[1:n_regr])
        a <- .DEFAULT_INV_LINKS[["alpha"]](p[n_regr + 1])
        b <- .DEFAULT_INV_LINKS[["beta"]](p[n_regr + 2])
        scale <- .DEFAULT_INV_LINKS[["scale"]](p[n_regr + 3])

        pdf_vals <- stable_pdf(resids/scale, a, b)

        pdf_vals <- pmax(.Machine$double.xmin, pdf_vals)
        -(-length(resids)*log(scale) + sum(log( pdf_vals  )))
    }


    init_params  <- c(rep(0, n_regr), 1.85, 0,  mad(y, na.rm = TRUE))


    r <- nlminb(objective = objf, start = init_params,
                lower = -Inf,
                upper = Inf, control = optim_control)


    if(r$convergence != 0)
        warning("Optimization did not converge")


    if(r$par[n_regr + 1] > 4.488636){  #alpha is > 1.99; beta estimate will not make sense so set to 0
        r$par[n_regr + 2] <- 0
    }


    out_se <- NULL
    approx_cb <- NULL
    if(output_se){
        hess <- numDeriv::hessian(objf, r$par)
        out_se <- sqrt(diag(solve(hess)))

        out_se <- structure(out_se, names = c(colnames(X), c("alpha", "beta", "scale")))

        approx_cb <- list(
            left = r$par - qnorm(conf) * out_se,
            right = r$par + qnorm(conf) * out_se
        )


        approx_cb[["left"]][n_regr + 1] <- .DEFAULT_INV_LINKS[["alpha"]](approx_cb[["left"]][n_regr + 1])
        approx_cb[["right"]][n_regr + 1] <- .DEFAULT_INV_LINKS[["alpha"]](approx_cb[["right"]][n_regr + 1])

        approx_cb[["left"]][n_regr + 2] <- .DEFAULT_INV_LINKS[["beta"]](approx_cb[["left"]][n_regr + 2])
        approx_cb[["right"]][n_regr + 2] <- .DEFAULT_INV_LINKS[["beta"]](approx_cb[["right"]][n_regr + 2])

        approx_cb[["left"]][n_regr + 3] <- .DEFAULT_INV_LINKS[["scale"]](approx_cb[["left"]][n_regr + 3])
        approx_cb[["right"]][n_regr + 3] <- .DEFAULT_INV_LINKS[["scale"]](approx_cb[["right"]][n_regr + 3])
    }

    cb <- NULL
    if(calc_confbounds){
        cb <- calc_exact_confbounds(y, X, r$par, out_se, -r$objective, conf = conf, trace = trace)
    }

    #tranform vars back to original scale
    r$par[n_regr + 1] <- .DEFAULT_INV_LINKS[["alpha"]](r$par[n_regr + 1])
    r$par[n_regr + 2] <- .DEFAULT_INV_LINKS[["beta"]](r$par[n_regr + 2])
    r$par[n_regr + 3] <- .DEFAULT_INV_LINKS[["scale"]](r$par[n_regr + 3])

    structure(list(
        coefs = structure(r$par,
                          names = c(colnames(X), c("alpha", "beta", "scale"))
                          ),
        log_lik = -r$objective,
        aic = 2*(n_regr + 3) + 2*r$objective,
        se = out_se,
        cb = cb,
        conf_level = conf,
        formula = formula,
        approx_cb = approx_cb,
        is_glm = FALSE
    ),
    class = "stabreg")

}



get_regr_idc <- function(n_regr){
    stopifnot(is.list(n_regr))
    k <- 1
    regr_idc <- list()

    for(rn in .REGR_NAMES){
        i <- 1
        if(!is.null(n_regr[[rn]]))
            i <- n_regr[[rn]]

        regr_idc[[rn]] <- k:(k + i - 1)
        k <- k + i
    }
    regr_idc
}




check_formula_list <- function(formulas){
    if(!("loc" %in% names(formulas)))
        formulas[["loc"]] <- formula( ~ 1)

    formulas
}



.DEFAULT_LINKS <- list(
    alpha = function(a){
        log((a - 1.1) / (2 - a))
    },
    beta = function(b){
        log((1 + b) / (1 - b))
    },
    scale = function(s){
        log(s)
    },
    loc = function(x) x
)

.DEFAULT_INV_LINKS <- list(
    alpha = function(x){
        x <- pmin(log( (.Machine$double.xmax - 1.1) / 2 )  - 0.0001, x)
        (exp(x) * 2 + 1.1) / (1 + exp(x))
    },
    beta = function(x){
        x <- pmin(log(.Machine$double.xmax - 1) - 0.0001, x)
        (exp(x) - 1) / (exp(x) + 1)
    },
    scale = function(x){
        x <- pmin(log(.Machine$double.xmax)  - 0.0001 , x)
        exp(x)
    },
    loc = function(x) x
)


# "formulas" is a list for formulas.
# all formulas are supplied without a response varibale, just " ~ x + z" etc.
# Only need to supply the relevant formulas, e.g. list(loc = formula(~ x*z), alpha = formula(~ x))
stable_glm <- function(y_name, formulas, data, output_se = TRUE, calc_confbounds = FALSE, conf = 0.95,
                       trace = 0, optim_control = list())
{
    stopifnot(is.character(y_name), y_name %in% colnames(data))
    formulas <- check_formula_list(formulas)

    X_list <- lapply(formulas, function(x) model.matrix(x, data = data))
    n_regr <- lapply(X_list, ncol)
    regr_idc <- get_regr_idc(n_regr)


    # p order: loc, alpha, beta, scale
    # if a param has a regression, no of elements in p is no. of regressors, otherwise 1
    objf <- function(p){
        ff <- function(x){
            if( is.null(formulas[[x]]) ){
                if(x == "loc"){
                    data[[y_name]]
                }
                else {
                    .DEFAULT_INV_LINKS[[x]](p[regr_idc[[x]]])
                }
            }
            else {
                if(x == "loc"){
                    data[[y_name]] - c(X_list[[x]] %*% p[regr_idc[[x]]])
                }
                else{
                    .DEFAULT_INV_LINKS[[x]](c(X_list[[x]] %*% p[regr_idc[[x]]]))
                }
            }
        }

        res <- lapply(.REGR_NAMES, ff)
        names(res) <- .REGR_NAMES


        pdf_vals <- stable_pdf(res[["loc"]] /res[["scale"]], res[["alpha"]], res[["beta"]])
        pdf_vals <- pmax(.Machine$double.xmin, pdf_vals)
        -(-nrow(data)*log(res[["scale"]]) + sum(log(pdf_vals)))
    }

    init_params <- rep(0, length(unlist(regr_idc)))

    r <- nlminb(objective = objf, start = init_params, control = optim_control)

    if(r$convergence != 0)
        warning("Optimization did not converge. nlminb message: ", r$message)

    pars <- r$par
    approx_cb <- NULL
    se_failflag <- FALSE
    if(output_se){
        hess <- numDeriv::hessian(objf, r$par)
        out_se <- tryCatch(sqrt(diag(solve(hess))),
                           error = function(x){
                               warning("Inversion of Hessian failed: ", x$message);
                               NULL
                           })

        if(is.null(out_se)){
            se_failflag <- TRUE
            out_se <- rep(NA_real_, length(pars))
        }


        approx_cb <- list(
            left = r$par - qnorm(conf) * out_se,
            right = r$par + qnorm(conf) * out_se
        )


        for(rn in .REGR_NAMES){
            approx_cb[["left"]][regr_idc[[rn]]] <- .DEFAULT_INV_LINKS[[rn]](approx_cb[["left"]][regr_idc[[rn]]])
            approx_cb[["right"]][regr_idc[[rn]]] <- .DEFAULT_INV_LINKS[[rn]](approx_cb[["right"]][regr_idc[[rn]]])
        }
    } else {
        out_se <- rep(NA_real_, length(pars))
    }
    tvals <- rep(NA_real_, length(pars))


    for(rn in .REGR_NAMES){
        tvals[regr_idc[[rn]]] <- pars[regr_idc[[rn]]] / out_se[regr_idc[[rn]]]
        if(is.null(formulas[[rn]])){
            names(pars)[regr_idc[[rn]]] <- rn
            pars[regr_idc[[rn]]] <- .DEFAULT_INV_LINKS[[rn]](pars[regr_idc[[rn]]])
        }
        else {
            names(pars)[regr_idc[[rn]]] <- paste0(rn, " ", colnames(X_list[[rn]]))
        }
    }
    names(out_se) <- names(tvals) <- names(pars)


    cb <- NULL
    if(calc_confbounds & !se_failflag){
        cb_raw <- calc_exact_confbounds_glm(data[[y_name]], X_list, regr_idc, pars, out_se, -r$objective, conf = conf, trace = trace)

        left_cb <- right_cb <- rep(NA_real_, length(pars))
        for(rn in names(cb_raw)){
            left_cb[ regr_idc[[rn]] ] <- cb_raw[[rn]][["left"]]
            right_cb[ regr_idc[[rn]] ] <- cb_raw[[rn]][["right"]]
        }
        cb <- list(left = left_cb, right = right_cb)
    }

    structure(list(
        coefs = pars,
        log_lik = -r$objective,
        aic = 2*length(pars) + 2*r$objective,
        se = out_se,
        cb = cb,
        conf_level = conf,
        formula = formulas[["loc"]],
        approx_cb = approx_cb,
        is_glm = TRUE,
        optim_convergence = r$convergence
    ),
    class = "stabreg")

}
