#' Compare regression models using AIC and BIC.
#'
#' Obtain a quick model comparison matrix for a selection of models
#' @param x a regression model (lm, glm, svyglm, ...)
#' @param ... other models
#' @return an `inzmodelcomp` object containing model comparison statistics
#' @author Tom Elliott
#' @export
#' @examples
#' m0 <- lm(Sepal.Length ~ 1, data = iris)
#' m1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
#' m2 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
#' compare_models(m0, m1, m2)
compare_models <- function(x, ...) {
    UseMethod("compare_models")
}

#' @describeIn compare_models default method
#' @export
compare_models.default <- function(x, ...) {
    # x should be a model
    xclass <- class(x)
    model.list <- c(list(x), list(...))
    if (length(model.list) > 1) {
        if (any(!sapply(model.list, function(z) all(class(z) == xclass))))
            stop("Models must be of the same type")
    }

    AIC <- AIC(x, ...)
    BIC <- BIC(x, ...)

    if (length(model.list) > 1) {
        df <- AIC$df
        AIC <- AIC$AIC
        BIC <- BIC$BIC
    }
    else df <- attr(stats4::logLik(x), "df")

    mat <- cbind(df, AIC, BIC)
    structure(mat,
        .Dimnames = list(
            Model = as.character(match.call())[-1],
            colnames(mat)
        ),
        class = "inzmodelcomp"
    )
}

#' @describeIn compare_models method for survey GLMs
#' @export
compare_models.svyglm <- function(x, ...) {
    model.list <- c(list(x), list(...))
    if (length(model.list) > 1) {
        if (any(!sapply(model.list, function(z) inherits(z, "svyglm"))))
            stop("Models must be of the same type")
    }

    AIC <- AIC(x, ...)

    # models must be nested to compute BIC, and the "maximal" model needs
    # to be computed
    maximal <- which.max(sapply(model.list, function(z) length(names(coef(z)))))

    model_names <- as.character(match.call())[-1]
    BIC <- try(BIC(x, ..., maximal = model.list[[maximal]]), silent = TRUE)
    if (inherits(BIC, "try-error")) BIC <- NULL

    if (length(model.list) > 1) {
        AIC <- AIC[, "AIC"]
        if (!is.null(BIC))
            BIC <- BIC[, "BIC"]
    } else {
        AIC <- AIC["AIC"]
        BIC <- BIC["BIC"]
    }

    mat <- cbind(AIC, BIC)
    if (length(model.list) > 1)
        mat <- cbind(mat, maximal = 1:length(model.list) == maximal)
    structure(mat,
        .Dimnames = list(
            Model = as.character(match.call())[-1],
            colnames(mat)
        ),
        class = "inzmodelcomp"
    )
}

#' @export
print.inzmodelcomp <- function(x, ...) {
    if (nrow(x) == 1) {
        print(unclass(x))
        return()
    }

    hasBIC <- "BIC" %in% colnames(x)
    z <- data.frame(
        Model = rownames(x),
        AIC = x[, "AIC"],
        stringsAsFactors = TRUE
    )
    if (hasBIC) {
        z$BIC <- x[, "BIC"]
        if ("maximal" %in% colnames(x)) {
            z$maximal <- ifelse(x[, "maximal"] == 1, "*", "")
            colnames(z)[4] <- ""
        }
    }
    print(z, row.names = FALSE)
    if ("maximal" %in% colnames(x)) {
        if (hasBIC)
            cat("\nModel BICs compared to the maximal model, denoted with *\n")
        else
            cat("\nBIC not shown as no single model contains all of the variables\n")
    }
    invisible(NULL)
}
