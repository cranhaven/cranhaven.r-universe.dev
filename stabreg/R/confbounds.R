

get_par_vec <- function(p, fixed_idx, fixed_val){
#
#     if(length(p) == 1)
#         return(p)

    if(fixed_idx > length(p) + 1)
        stop("fixed_idx out of bounds")

    if(fixed_idx == 1L)
        c(fixed_val, p)
    else if(fixed_idx == length(p) + 1)
        c(p, fixed_val)
    else
        c(p[1:(fixed_idx - 1)], fixed_val, p[fixed_idx:length(p)])
}




get_par_vec_glm <- function(p, fixed_idx, fixed_val){
    if(fixed_idx > length(p) + 1)
        stop("fixed_idx out of bounds")

    if(fixed_idx == 1L)
        c(fixed_val, p)
    else if(fixed_idx == length(p) + 1)
        c(p, fixed_val)
    else
        c(p[1:(fixed_idx - 1)], fixed_val, p[fixed_idx:length(p)])
}



#Profile Likelihood is calculated by keeping one parameter fixed
#and maximzing likelihood over all the others. Here we're doing this over
#the location-regressors, since these are the ultimate subject of goodness-of-fit
#analysis in the regression. "par_val" is the value at which to evaluate the
#profile likelihood. "par_idx" refers to column index in X at which this parameter
#exists.
profile_lik <- function(y, X, par_val, par_idx, trace = FALSE, init_params = NULL){
    if(ncol(X) < par_idx)
        stop("par_idx outside of X column range")

    # If only one loc-regressor, we're optimizing only over the stable params
    # Otherwise, we're optimizing over the rest of loc-regressors (aside from
    # the target one), so have to essentially keep the target loc-regressor constant
    n_regr <- ncol(X)
    n_free_loc <- n_regr - 1  #no. of "free" loc-regressors to optimize over


    objf <- function(p){
        # p here will be the params to optimize over. If model has only one loc-regressor,
        # p will only contain the stable params. I.e. the first n_free_loc
        # elements (possibly 0) of p are the loc-regressors to optimize over

        #compute residuals by constructing a coefficient vector where
        #the non-free param value is inserted at its designated position
        resids <- {
            if(n_free_loc > 0)
                y - c(X %*% get_par_vec(p[1:n_free_loc], par_idx, par_val))
            else
                y - c(X %*% par_val)
        }

        a      <- p[n_free_loc + 1]
        b      <- p[n_free_loc + 2]
        scale  <- p[n_free_loc + 3]

        -(-length(resids)*log(scale) + sum(log( stable_pdf(resids/scale, a, b)  )))
    }

    init_scale   <- mad(y)

    lower_bounds <- c(rep(-Inf, n_free_loc),  1.1, -0.99999, init_scale / 10)
    upper_bounds <- c(rep( Inf, n_free_loc),  2.0,  0.99999, init_scale * 10)
    if(is.null(init_params))
        init_params  <- c(rep(0, n_free_loc),     1.75,  0,       init_scale)

    r <- nlminb(objective = objf, start = init_params,
                lower = lower_bounds,
                upper = upper_bounds, control = list(trace = trace))


    -r$objective
}


#removes the target par from regr_idc and shifts all index values
#one to the leftc
adjust_regr_idc <- function(regr_idc, par_type, par_idx){
    adj_regr_idc <- regr_idc

    i <- which(names(adj_regr_idc) == par_type)

    # remove target parameter
    adj_regr_idc[[i]] <- adj_regr_idc[[i]][-par_idx]


    # porentially adjust vector of trailing par indexes for target par type
    n <- length(adj_regr_idc[[i]])
    if(n > 0 & n >= par_idx){
        adj_regr_idc[[i]][par_idx:n] <- adj_regr_idc[[i]][par_idx:n] - 1
    }

    # shift idc for all par types after par_type
    k <- i + 1
    while(k <= length(adj_regr_idc)){
        adj_regr_idc[[k]] <- adj_regr_idc[[k]] - 1
        k <- k + 1
    }
    adj_regr_idc
}


# par_type refers to which stable parameter in X_list we are computing for, e.g. "alpha"
profile_lik_glm <- function(y, X_list, par_type, regr_idc, par_val, par_idx, trace = FALSE, init_params = NULL){

    N <- nrow(X_list[[1]])

    if(is.null(X_list[[par_type]])){
        stop("Attempt at profile likelihood calc for non-regression parameter")
    }


    #its assumed that regr_idc is the index list for the original parater vector, ,whereas here
    # we are holding one par constant and thus the actual par vector is 1 element shorter
    # therefore a need to adjust the original regr_idc
    regr_idc <- adjust_regr_idc(regr_idc, par_type, par_idx)

    # p order: loc, alpha, beta, scale
    # if a param has a regression, no of elements in p is no. of regressors, otherwise 1
    objf <- function(p){
        ff <- function(x){

            if( is.null(X_list[[x]]) ){
                if(x == "loc"){
                    y
                }
                else {
                    # no x==par_type case here since assumed we don't compute confintercals for pars wihtout regressions
                    .DEFAULT_INV_LINKS[[x]](p[regr_idc[[x]]])
                }
            }
            else {
                if(x == par_type){

                    # impute constant val
                    if(ncol(X_list[[x]]) > 1)
                        p_par <- get_par_vec_glm(p[regr_idc[[x]]], par_idx, par_val)
                    else
                        p_par <- par_val

                } else {
                    p_par <- p[regr_idc[[x]]]
                }

                if(x == "loc"){
                    y - c(X_list[[x]] %*% p_par)
                }
                else{
                    .DEFAULT_INV_LINKS[[x]](c(X_list[[x]] %*% p_par))
                }
            }
        }

        res <- lapply(.REGR_NAMES, ff)
        names(res) <- .REGR_NAMES


        pdf_vals <- stable_pdf(res[["loc"]] /res[["scale"]], res[["alpha"]], res[["beta"]])
        pdf_vals <- pmax(.Machine$double.xmin, pdf_vals)
        -(-N*log(res[["scale"]]) + sum(log(pdf_vals)))
    }

    init_params <- rep(0, length(unlist(regr_idc)))

    r <- nlminb(objective = objf, start = init_params, control = list(trace = trace))

    -r$objective
}



find_conf_bound <- function(y, X, par_idx, par_mle_val, par_se, mle_fit_ll,
                            init_params = NULL, conf = 0.95,
                            trace = 0, is_glm = FALSE, par_type = NULL, regr_idc = NULL){

    if(conf > 0.99)
        stop("max conf 0.99 allowed")
    if(is.na(par_se)){
        return(rep(NA_real_, 2))
    }

    u <- -qchisq(conf, 1) / 2

    f <- function(x){
        if(is_glm){
            pfl <- profile_lik_glm(y, X, par_type = par_type, regr_idc = regr_idc, par_val = x, par_idx = par_idx, init_params = init_params)
        } else {
            pfl <- profile_lik(y, X, par_val = x, par_idx = par_idx, init_params = init_params)
        }
        pfl - u - mle_fit_ll
    }

    left_bound <- par_mle_val - 3 * qnorm(conf) * par_se # x3 to have a buffer
    right_bound <- par_mle_val + 3 * qnorm(conf) * par_se

    lr <- tryCatch(
        uniroot(f, c(left_bound, par_mle_val),
                f.upper = -u, f.lower = f(left_bound),
                tol = abs(par_mle_val / 50),
                trace = trace)$root,
        error = function(x) -Inf
    )

    rr <- tryCatch(
        uniroot(f, c(par_mle_val, right_bound),
                f.upper = -u, f.lower = f(right_bound),
                tol = abs(par_mle_val / 50),
                trace = trace)$root,
        error = function(x) Inf
    )

    c(lr, rr)
}




calc_exact_confbounds <- function(y, X, mle_fits, se_vals, mle_fit_ll, conf = 0.95, trace = FALSE){
    left_vals <- right_vals <-rep(NA_real_, ncol(X))
    for(i in 1:ncol(X)){
        cb <- find_conf_bound(y, X, i, mle_fits[i], se_vals[i], mle_fit_ll,
                              init_params = mle_fits[-i],
                              conf = conf,
                              trace = trace)
        left_vals[i] <- cb[1]
        right_vals[i] <- cb[2]
    }
    data.frame("left" = left_vals,
               "right" = right_vals, stringsAsFactors = FALSE)
}



calc_exact_confbounds_glm <- function(y, X_list, regr_idc, mle_fits, se_vals, mle_fit_ll, conf = 0.95, trace = FALSE){

    out <- list()

    for(par_name in names(X_list))
    {
        left_vals <- right_vals <- rep(NA_real_, ncol(X_list[[par_name]]))

         for(i in 1:ncol(X_list[[par_name]])){

            cb <- find_conf_bound(y, X_list, i, mle_fits[ regr_idc[[par_name]][i] ],
                                  se_vals[ regr_idc[[par_name]][i] ],
                                  mle_fit_ll,
                                  init_params = mle_fits[ regr_idc[[par_name]][-i] ],
                                  conf = conf,
                                  trace = trace,
                                  is_glm = TRUE,
                                  regr_idc = regr_idc,
                                  par_type = par_name)
            left_vals[i] <- cb[1]
            right_vals[i] <- cb[2]
        }

        out[[par_name]] <-  data.frame("left" = left_vals, "right" = right_vals, stringsAsFactors = FALSE)
    }
    out
}

