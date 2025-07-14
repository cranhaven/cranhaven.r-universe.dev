# Vendor in some functionality from {trending} that has not yet been released
# on CRAN

# TODO - once the newer version of {trending} is released on CRAN this needs to
#        be removed.

# -------------------------------------------------------------------------
trending_model <- function(model_name, args) {
    cls <- c(
        sprintf("%s_trending_model", substitute(model_name)),
        "trending_model"
    )

    qfun <- bquote(
        function(formula, ...) { # formula argument ensures printing of formula first
            if (getRversion() >= "4.1.0") {
                nms <- ...names()
            } else {
                dots <- match.call(expand.dots = FALSE)$`...`
                nms <- names(dots)
            }
            if ("data" %in% nms && length(nms)) {
                stop("'data' should not be passed as argument", call. = FALSE)
            }
            out <- match.call()
            out[[1]] <- as.name(.(model_name))
            structure(out, class = c(.(cls), class(out)))
        }
    )

    do.call(eval(qfun), args)
}

# -------------------------------------------------------------------------
glm_model <- function(formula, family = gaussian, ...) {
    stopifnot(inherits(formula, "formula"))
    args <- as.list(substitute(list(...))[-1L])
    args$formula = formula
    args$family = substitute(family)
    trending_model("glm", args)
}

# -------------------------------------------------------------------------
glm_nb_model <- function(formula, ...) {
    stopifnot(inherits(formula, "formula"))
    args <- as.list(substitute(list(...))[-1L])
    args$formula = formula
    trending_model("glm.nb", args)
}

# -------------------------------------------------------------------------
new_trending_fit <- function(x) {
    structure(x, class = c("trending_fit", class(x)))
}

# -------------------------------------------------------------------------
new_trending_fit_tbl <- function(
    x,
    model_name = NULL,
    result = "result",
    warnings = "warnings",
    errors = "errors"
) {
    new_tibble(
        x,
        model_name = model_name,
        result = result,
        warnings = warnings,
        errors = errors,
        nrow = nrow(x),
        class = "trending_fit_tbl"
    )
}

# -------------------------------------------------------------------------
new_trending_predict <- function(x) {
    structure(x, class = c("trending_predict", class(x)))
}

# -------------------------------------------------------------------------

new_trending_predict_tbl <- function(
    x,
    model_name = NULL,
    result = "result",
    warnings = "warnings",
    errors = "errors"
) {
    stopifnot(
        is.data.frame(x),
        is.null(model_name) || is.character(model_name),
        is.character(result),
        is.character(warnings),
        is.character(errors)
    )
    new_tibble(
        x,
        model_name = model_name,
        result = result,
        warnings = warnings,
        errors = errors,
        nrow = nrow(x),
        class = "trending_predict_tbl"
    )
}

# -------------------------------------------------------------------------
fit.trending_model <- function(x, data, as_tibble = TRUE, ...) {
    x[["data"]] <- substitute(data)
    envir = parent.frame()
    #if (inherits(x, "brm_trending_model")) envir$brm <- brms::brm
    if (inherits(x, "glm.nb_trending_model")) envir$glm.nb <-  MASS::glm.nb
    f <- make_catcher(eval)
    res <- f(x, envir)
    if (as_tibble) {
        res <- tibble(
            result = list(res[[1]]),
            warnings = list(res[[2]]),
            errors = list(res[[3]])
        )
        res <- new_trending_fit_tbl(res)
    } else {
        res <- new_trending_fit(res)
    }
    res
}

# -------------------------------------------------------------------------
get_response.trending_model <- function(x, ...) {
    formula <- get_formula.trending_model(x)
    as.character(formula)[2]
}

# -------------------------------------------------------------------------
get_result.trending_fit <- function(x, ...) x$result

# -------------------------------------------------------------------------
get_result.trending_fit_tbl <- function(x, ...) x[[attr(x, "result")]]

# -------------------------------------------------------------------------
get_formula.trending_model <- function(x, ...) x$formula

# -------------------------------------------------------------------------
get_result.trending_predict_tbl <- get_result.trending_fit_tbl

# -------------------------------------------------------------------------
get_fitted_model.trending_fit <- get_result.trending_fit

# -------------------------------------------------------------------------
get_fitted_model.trending_fit_tbl <- get_result.trending_fit_tbl

# -------------------------------------------------------------------------
get_fitted_data.trending_fit_tbl <- function(x, ...) {
    models <- get_fitted_model.trending_fit_tbl(x)
    lapply(
        models,
        function(x) {
            res <- if (inherits(x, "brmsfit")) x$data else x$model
            attr(res, "data_name") <- NULL
            attr(res, "terms") <- NULL
            res
        }
    )
}

# -------------------------------------------------------------------------
get_fitted_data.trending_fit <- function(x, ...) {
    model <- get_fitted_model.trending_fit(x)
    res <- if (inherits(model, "brmsfit")) model$data else model$model
    attr(res, "data_name") <- NULL
    attr(res, "terms") <- NULL
    res
}

# -------------------------------------------------------------------------
get_formula.trending_fit <- function(x, ...) {
    res <- get_fitted_model.trending_fit(x)
    if (inherits(res, "brmsfit")) res$formula else res$call$formula
}

# -------------------------------------------------------------------------
get_formula.trending_fit_tbl <- function(x, ...) {
    models <- get_fitted_model.trending_fit_tbl(x)
    lapply(
        models,
        function(m) if (inherits(m, "brmsfit")) m$formula else m$call$formula
    )
}

# -------------------------------------------------------------------------
get_formula.trending_model <- function(x, ...) x$formula

# -------------------------------------------------------------------------
get_response.trending_fit <- function(x, ...) {
    formula <- get_formula.trending_fit(x)
    if (inherits(formula, "brmsformula")) formula <- formula$formula
    as.character(formula)[2]
}

# -------------------------------------------------------------------------
get_response.trending_fit_tbl <- function(x, ...) {
    formula <- get_formula.trending_fit_tbl(x)
    lapply(
        formula,
        function(x) {
            if (inherits(x, "brmsformula")) x <- x$formula
            as.character(x)[2]
        }
    )
}

# -------------------------------------------------------------------------
get_predictors.trending_model <- function(x, ...) {
    formula <- get_formula.trending_model(x)
    if (inherits(formula, "brmsformula")) formula <- formula$formula
    vars <- all.vars(formula)
    response <- get_response.trending_model(x)
    vars[!vars %in% response]
}

# -------------------------------------------------------------------------
get_predictors.trending_fit <- get_predictors.trending_model

# -------------------------------------------------------------------------
get_predictors.trending_fit_tbl <- function(x, ...) {
    formulas <- get_formula.trending_fit_tbl(x)
    vars <- lapply(
        formulas,
        function(x) {
            if (inherits(x, "brmsformula")) x <- x$formula
            all.vars(x)
        }
    )
    response <- get_response.trending_fit_tbl(x)
    .mapply(function(x, y) x[!x %in% y], dots = list(x = vars, y = response), MoreArgs = NULL)
}

# -------------------------------------------------------------------------
predict.trending_fit <- function(
    object,
    new_data,
    name = "estimate",
    alpha = 0.05,
    add_ci = TRUE,
    ci_names = c("lower_ci", "upper_ci"),
    add_pi = TRUE,
    pi_names = c("lower_pi", "upper_pi"),
    simulate_pi = FALSE,
    sims = 2000,
    uncertain = TRUE,
    as_tibble = TRUE,
    ...
) {

    # ensure that we have a model to use for predictions
    fitted_model <- get_fitted_model.trending_fit(object)

    # if no data supplied we use the model data
    if (missing(new_data)) new_data <- get_fitted_data.trending_fit(object)

    # check for name clashes with the existing input
    check_names(
        new_data = new_data,
        name = name,
        add_ci = add_ci,
        ci_names = ci_names,
        add_pi = add_pi,
        pi_names = pi_names
    )

    res <- predict_individual(
        fitted_model,
        new_data = new_data,
        response = get_response.trending_fit(object),
        predictors = get_predictors.trending_fit(object),
        name = name,
        alpha = alpha,
        add_ci = add_ci,
        ci_names = ci_names,
        add_pi = add_pi,
        pi_names = pi_names,
        simulate_pi = simulate_pi,
        sims = sims,
        uncertain = uncertain
    )

    if (as_tibble) {
        res <- tibble(
            result = list(res[[1]]),
            warnings = list(res[[2]]),
            errors = list(res[[3]])
        )
        res <- new_trending_predict_tbl(res)
    } else {
        res <- new_trending_predict(res)
    }
    res
}

# -------------------------------------------------------------------------
predict.trending_fit_tbl <- function(
    object,
    new_data,
    name = "estimate",
    alpha = 0.05,
    add_ci = TRUE,
    ci_names = c("lower_ci", "upper_ci"),
    add_pi = TRUE,
    pi_names = c("lower_pi", "upper_pi"),
    simulate_pi = FALSE,
    sims = 2000,
    uncertain = TRUE,
    ...
) {
    # if no data supplied we use the model data
    fitted_models <- get_fitted_model.trending_fit_tbl(object)
    if (missing(new_data)) {
        ok <- vapply(fitted_models, function(x) !is.null(x), logical(1))
        new_data <- if (!any(ok)) NULL else get_fitted_data.trending_fit_tbl(object)[ok][[1]] # OK as data will be same for all
    }

    res <- .mapply(
        FUN = predict_individual,
        dots = list(
            model = fitted_models,
            response = get_response.trending_fit_tbl(object),
            predictors = get_predictors.trending_fit_tbl(object)
        ),
        MoreArgs = list(
            new_data = new_data,
            name = name,
            alpha = alpha,
            add_ci = add_ci,
            ci_names = ci_names,
            add_pi = add_pi,
            pi_names = pi_names,
            simulate_pi = simulate_pi,
            sims = sims,
            uncertain = uncertain
        )
    )

    nms_var <- attr(object, "model_name")
    if (!is.null(nms_var)) names(res) <- object[[nms_var]]
    res <- lapply(seq_along(res[[1]]), function(i) lapply(res, "[[", i))
    res <- tibble(result = res[[1]], warnings = res[[2]], errors = res[[3]])
    model_name <- NULL
    if (!is.null(nms_var)) {
        res <- cbind(tibble(model_name = object[[nms_var]]), res)
        model_name <- nms_var
    }
    new_trending_predict_tbl(res, model_name = model_name)
}

# -------------------------------------------------------------------------
predict_individual <- function(
    model, new_data, response, predictors,
    name = "estimate", alpha = 0.05,
    add_ci = TRUE, ci_names = c("lower_ci", "upper_ci"),
    add_pi = TRUE, pi_names = c("lower_pi", "upper_pi"),
    simulate_pi = FALSE, sims = 2000, uncertain = TRUE
) {

    # wrap add_estimate to catch warnings and errors
    fun <- make_catcher(add_prediction)

    out <- fun(
        model,
        new_data = new_data,
        name = name,
        alpha = alpha,
        add_ci = add_ci,
        ci_names = ci_names,
        add_pi = add_pi,
        pi_names = pi_names,
        simulate_pi = simulate_pi,
        sims = sims,
        uncertain = uncertain
    )

    # only save attributes that are used
    if (!add_ci) ci_names <- NULL
    if (!add_pi) pi_names <- NULL

    # make result a subclass of tibble
    result <- out$result
    if (!is.null(result)) {
        result <- new_tibble(
            result,
            response = response,
            predictors = predictors,
            estimate = name,
            ci_names = ci_names,
            pi_names = pi_names,
            nrow = nrow(result),
            class = "trending_prediction"
        )
        out$result <- result
    }
    out
}

# -------------------------------------------------------------------------
add_prediction <- function(object, new_data, ...) {
    switch(
        class(object)[1],
        glm = add_prediction_glm(object = object, new_data = new_data, ...),
        negbin = add_prediction_glm(object = object, new_data = new_data, ...),
        not_implemented(object, call. = TRUE)
    )
}

# -------------------------------------------------------------------------
add_prediction_glm <- function(object, new_data, name, alpha, add_ci, ci_names,
                               add_pi, pi_names, simulate_pi, sims, uncertain,
                               ...) {

    # add confidence intervals using ciTools::add_ci
    out <- ciTools::add_ci(
        df = new_data,
        fit = object,
        alpha = alpha,
        names = ci_names,
        yhatName = name
    )

    # optionally add prediction intervals
    if (add_pi && simulate_pi) {         # simulated intervals use ciTools::add_pi
        resp <- as.character(formula(object)[2])
        if (is.null(new_data[[resp]])) {   # Hack to fix bug in ciTools
            out[[resp]] <- out[[name]]
            out <- ciTools::add_pi(
                df = out,
                fit = object,
                alpha = alpha,
                names = pi_names,
                yhatName = "predpred",
                nSims = sims
            )
            out[[resp]] <- NULL
        } else {
            out <- ciTools::add_pi(
                df = out,
                fit = object,
                alpha = alpha,
                names = pi_names,
                yhatName = "predpred",
                nSims = sims
            )
        }
        out$predpred <- NULL
    } else if (add_pi) { # "analytic" intervals

        pred <- out[[name]]
        if (uncertain) {
            lower_ci <- out[[ci_names[1]]]
            upper_ci <- out[[ci_names[2]]]
        } else {
            lower_ci <- pred
            upper_ci <- pred
        }
        lower_pi_nm <- pi_names[1]
        upper_pi_nm <- pi_names[2]

        # prediction intervals
        fam <- family(object)$family

        if (inherits(object, "negbin")) {
            theta <- object$theta
            setheta <- object$SE.theta
            out[[lower_pi_nm]] <- qnbinom(alpha / 2, mu = lower_ci, size = theta)
            out[[upper_pi_nm]] <- qnbinom(1 - alpha / 2, mu = upper_ci, size = theta)
        } else if (fam == "poisson") {
            out[[lower_pi_nm]] <- qpois(alpha / 2, lambda = lower_ci)
            out[[upper_pi_nm]] <- qpois(1 - alpha / 2, lambda = upper_ci)
        } else if (fam == "quasipoisson") {
            overdispersion <- summary(object)$dispersion
            out[[lower_pi_nm]] <- qnbinom(alpha / 2, mu = lower_ci, size = lower_ci / (overdispersion - 1))
            out[[upper_pi_nm]] <- qnbinom(1 - alpha / 2, mu = upper_ci, size = upper_ci / (overdispersion - 1))
        } else if (fam == "gamma") {
            overdispersion <- summary(object)$dispersion
            out[[lower_pi_nm]] <- qgamma(alpha / 2, shape = 1 / overdispersion, rate = 1 / (lower_ci * overdispersion))
            out[[upper_pi_nm]] <- qgamma(1 - alpha / 2, shape = 1 / overdispersion, rate = 1 / (upper_ci * overdispersion))
        } else if (fam == "binomial") {
            out[[lower_pi_nm]] <- qbinom(alpha / 2, size = object$prior.weights, prob = lower_ci / object$prior.weights)
            out[[upper_pi_nm]] <- qbinom(1 - alpha / 2, size = object$prior.weights, prob = upper_ci / object$prior.weights)
        } else if (fam == "gaussian") {
            sigma_sq <- summary(object)$dispersion
            tmp <- predict(object, out, se.fit = TRUE, type = "link")
            se_terms <- tmp$se.fit
            t_quant <- qt(p = alpha / 2, df = object$df.residual, lower.tail = FALSE)
            se_global <- sqrt(sigma_sq + se_terms^2)
            out[[lower_pi_nm]] <- pred - t_quant * se_global
            out[[upper_pi_nm]] <- pred + t_quant * se_global
        } else {
            stop("Unsupported glm family type")
        }

        # corrections for extremes
        if (isTRUE(all.equal(alpha, 0))) {
            out[[lower_pi_nm]] <- ifelse(is.infinite(lower_ci), -Inf, out[[lower_pi_nm]])
            out[[upper_pi_nm]] <- ifelse(is.infinite(upper_ci), Inf, out[[upper_pi_nm]])
            if (inherits(object, "negbin")) {
                out[[lower_pi_nm]] <- ifelse(is.nan(out[[lower_pi_nm]]), 0, out[[lower_pi_nm]])
            }
        }

    }

    # optionally remove confidence intervals (leaves the estimate)
    if (!add_ci) out[ci_names] <- NULL

    # ensure result is tibble
    as_tibble(out)
}

# -------------------------------------------------------------------------
check_names <- function(new_data, name, add_ci, ci_names, add_pi, pi_names) {
    nms <- names(new_data)
    if (name %in% nms) {
        fmt <- c(
            'Please provide an alternative value for `name`:\n',
            '       column named "%s" already present in `new_data`.'
        )
        stop(sprintf(fmt, name), call. = FALSE)
    }

    ci_present <- ci_names[ci_names %in% nms]
    if (add_ci && length(ci_present)) {
        fmt <- c(
            'Please provide alternative value for `ci_names`:\n',
            '       columns named "%s", already present in `new_data`.'
        )
        msg <- sprintf(fmt, paste(ci_present, collapse = ", "))
        stop(msg, call. = FALSE)
    }

    pi_present <- pi_names[pi_names %in% nms]
    if (add_pi && length(pi_present)) {
        fmt <- c(
            'Please provide alternative value for `pi_names`:\n',
            '       columns named "%s", already present in `new_data`.'
        )
        msg <- sprintf(fmt, paste(pi_present, collapse = ", "))
        stop(msg, call. = FALSE)
    }
}

# -------------------------------------------------------------------------
# h/t Martin Morgan
# https://stackoverflow.com/a/4952908
make_catcher <- function(fun) {
    function(...) {

        # create variables in environment to store output
        warn <- err <- NULL
        env <- environment()

        # define handlers
        warning_handler <- function(w) {
            assign("warn", c(warn, conditionMessage(w)), env, inherits = TRUE)
            invokeRestart("muffleWarning")
        }

        error_handler <- function(e) {
            assign("err", conditionMessage(e), env, inherits = TRUE)
            NULL
        }

        # capture output
        res <- withCallingHandlers(
            tryCatch(fun(...), error = error_handler),
            warning = warning_handler
        )

        list(result = res, warnings = warn, errors = err)
    }
}


