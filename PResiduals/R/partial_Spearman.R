#'  Partial Spearman's Rank Correlation
#'
#' \code{partial_Spearman} computes the partial Spearman's rank correlation between variable \var{X} and variable \var{Y} adjusting for other variables, \var{Z}.
#' The basic approach involves fitting a specified model of \var{X} on \var{Z}, a specified model of \var{Y} on \var{Z}, obtaining the probability-scale residuals from both models, and then calculating their Pearson's correlation.
#' \var{X} and \var{Y} can be any orderable variables, including continuous or discrete variables.
#' By default, \code{partial_Spearman} uses cumulative probability models (also referred as cumulative link models in literature) for both \var{X} on \var{Z} and \var{Y} on \var{Z} to preserve the rank-based nature of Spearman's correlation, since the model fit of cumulative probability models only depends on the order information of variables. However, for some specific types of variables, options of fitting parametric models are also available. See details in fit.x and fit.y
#'
#'
#' To compute the partial Spearman's rank correlation between \var{X} and \var{Y} adjusting for \var{Z}, \samp{formula} is specified as \code{\var{X} | \var{Y} ~ \var{Z}}.
#' This indicates that models of \code{\var{X} ~ \var{Z}} and
#' \code{\var{Y} ~ \var{Z}} will be fit.
#'
#' @references Li C and Shepherd BE (2012)
#' A new residual for ordinal outcomes.
#' \emph{Biometrika}. \bold{99}: 473--480.
#' @references Shepherd BE, Li C, Liu Q (2016)
#' Probability-scale residuals for continuous, discrete, and censored data.
#' \emph{The Canadian Jouranl of Statistics}. \bold{44}:463--476.
#' @references Liu Q, Shepherd BE, Wanga V, Li C (2018)
#' Covariate-Adjusted Spearman's Rank Correlation with Probability-Scale Residuals.
#' \emph{Biometrics}. \bold{74}:595--605.
#'
#' @param formula an object of class \code{\link{Formula}} (or one
#' that can be coerced to that class): a symbolic description of the
#' model to be fitted.  The details of model specification are given
#' under \sQuote{Details}.
#'
#' @param fit.x,fit.y the fitting functions used for the models of X or Y on Z.
#' The default function is \samp{orm}, which fits cumulative probability models for continuous or discrete ordinal variables. Other options include \samp{lm}, which fits linear regression models and obtains the probability-scale residuals by assuming normality;
#' \samp{lm.emp}, which fits linear regression models and obtains the probability-scale residuals by empirical ranking;
#' \samp{poisson}, which fits Poisson models for count variables; \samp{nb}, which fits negative binomial models for count variables; and \samp{logistic}, which fits logistic regression models for binary variables.
#'
#' @param link.x,link.y the link family to be used for the ordinal model of
#' \var{X} on \var{Z}.  Defaults to \samp{logit}. Other options are
#' \samp{probit}, \samp{cloglog}, \samp{loglog}, \samp{cauchit} and \samp{logistic} (equivalent with \samp{logit}). Used only when
#' \samp{fit.x} is \samp{orm}.
#'
#' @param data an optional data frame, list or environment (or object
#' coercible by \code{\link{as.data.frame}} to a data frame)
#' containing the variables in the model.  If not found in
#' \code{data}, the variables are taken from
#' \code{environment(formula)}, typically the environment from which
#' \code{partial_Spearman} is called.
#'
#' @param subset an optional vector specifying a subset of
#' observations to be used in the fitting process.
#'
#' @param na.action action to take when \code{NA} present in data.
#'
#' @param fisher logical indicating whether to apply fisher transformation to compute confidence intervals and p-values for the correlation.
#'
#' @param conf.int numeric specifying confidence interval coverage.
#'
#' @return object of \samp{partial_Spearman} class.
#' @export
#' @seealso \code{\link{print.partial_Spearman}}
#' @examples
#' data(PResidData)
#' #### fitting cumulative probability models for both Y and W
#' partial_Spearman(c|w ~ z,data=PResidData)
#' #### fitting a cumulative probability model for W and a poisson model for c
#' partial_Spearman(c|w~z, fit.x="poisson",data=PResidData)
#' partial_Spearman(c|w~z, fit.x="poisson", fit.y="lm.emp", data=PResidData )
#' @importFrom rms lrm orm
#' @importFrom SparseM as.matrix

partial_Spearman <- function(formula, data, fit.x="orm", fit.y="orm", link.x=c("logit", "probit", "cloglog", "loglog","cauchit", "logistic"),
                    link.y = c("logit", "probit", "cloglog", "loglog","cauchit", "logistic"),
                    subset, na.action=getOption('na.action'), fisher=TRUE,conf.int=0.95){

    ## Construct the model frames for x ~ z and y ~ z
    F1 <- Formula(formula)
    Fx <- formula(F1, lhs=1)
    Fy <- formula(F1, lhs=2)

    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action",
                 "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf$na.action <- na.action
    ## We set xlev to a benign non-value in the call so that it won't get partially matched
    ## to any variable in the formula. For instance a variable named 'x' could possibly get
    ## bound to xlev, which is not what we want.
    mf$xlev <- integer(0)
    mf[[1L]] <- as.name("model.frame")


    mx <- my <- mf

    ## NOTE: we add the opposite variable to each model frame call so that
    ## subsetting occurs correctly. Later we strip them off.
    mx[["formula"]] <- Fx
    yName <- all.vars(Fy[[2]])[1]
    mx[[yName]] <- Fy[[2]]

    my[["formula"]] <- Fy
    xName <- all.vars(Fx[[2]])[1]
    my[[xName]] <- Fx[[2]]

    mx <- eval(mx, parent.frame())
    mx[[paste('(',yName,')',sep='')]] <- NULL

    my <- eval(my, parent.frame())
    my[[paste('(',xName,')',sep='')]] <- NULL

    data.points <- nrow(mx)

    ## Construct the model matrix z
    mxz <- model.matrix(attr(mx,'terms'),mx)
    zzint <- match("(Intercept)", colnames(mxz), nomatch = 0L)
    if(zzint > 0L) {
        mxz <- mxz[, -zzint, drop = FALSE]
    }

    myz <- model.matrix(attr(my,'terms'),my)
    zzint <- match("(Intercept)", colnames(myz), nomatch = 0L)
    if(zzint > 0L) {
        myz <- myz[, -zzint, drop = FALSE]
    }

    score.xz <- switch(fit.x,
                       logistic = ordinal.scores(mx, mxz, method="logit"),
                       lm = lm.scores(y=model.response(mx), X=mxz, emp=FALSE),
                       lm.emp = lm.scores(y=model.response(mx), X=mxz, emp=TRUE),
                       poisson = poisson.scores(y=model.response(mx), X=mxz),
                       nb = nb.scores(y=model.response(mx), X=mxz),
                       orm = orm.scores(y=model.response(mx), X=mxz, link=link.x[1]))

    score.yz <- switch(fit.y,
                       logistic = ordinal.scores(my, myz, method="logit"),
                       lm = lm.scores(y=model.response(my), X=myz, emp=FALSE),
                       lm.emp = lm.scores(y=model.response(my), X=myz, emp=TRUE),
                       poisson = poisson.scores(y=model.response(my), X=myz),
                       nb = nb.scores(y=model.response(my), X=myz),
                       orm = orm.scores(y=model.response(my), X=myz, link=link.y[1]))


    ## return value
    ans <- list(
        TS=list(),
        fisher=fisher,
        conf.int=conf.int,
        data.points=data.points
        )

    ## presid vs presid (use pdf of normal)
    tb = corTS(score.xz$presid, score.yz$presid,
        score.xz$dl.dtheta, score.yz$dl.dtheta,
        as.matrix(score.xz$d2l.dtheta.dtheta), as.matrix(score.yz$d2l.dtheta.dtheta),
        score.xz$dpresid.dtheta, score.yz$dpresid.dtheta,fisher)
    tb.label = "partial Spearman"


    ans$TS$TB <- list(
        ts=tb$TS, var=tb$var.TS, pval=tb$pval.TS,
        label = tb.label
        )

    ans <- structure(ans, class="partial_Spearman")

    ## Apply confidence intervals
    for (i in seq_len(length(ans$TS))) {
        ts_ci <- getCI(ans$TS[[i]]$ts,ans$TS[[i]]$var,ans$fisher,conf.int)
        ans$TS[[i]]$lower <- ts_ci[,1]
        ans$TS[[i]]$upper <- ts_ci[,2]
    }

    return(ans)
}
