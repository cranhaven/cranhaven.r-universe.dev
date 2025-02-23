#'  Conditional Partial Spearman's Rank Correlation
#'
#' \code{conditional_Spearman} computes the partial Spearman's rank correlation between variable \var{X} and variable \var{Y} adjusting for variable \var{Z} conditional on \var{Zc}.
#' \var{X} and \var{Y} can be any orderable variables, including continuous and discrete variables. Covariate \var{Z} can be multidimensional. \var{X}, \var{Y}, and \var{Z} are specified by the argument \samp{formula}.
#' \var{Zc} is a one-dimensional covariate, specified by the argument \samp{conditional.by}.
#' The basic approach involves fitting a specified model of \var{X} on \var{Z}, a specified model of \var{Y} on \var{Z}, obtaining the probability-scale residuals, \var{Xres} and \var{Yres}, from both models, and then modeling their Pearson's correlation conditional on \var{Zc}.
#' Different methods are provided to model the Pearson's correlation between the two sets of probability-scale residuals. See details in \samp{conditional.method}.
#' As in \samp{partial.Spearman}, by default \code{conditional_Spearman} uses cumulative link models for both continous and discrete ordinal variables \var{X} and \var{Y} to preserve the rank-based nature of Spearman's correlation. For some specific types of variables, options of fitting parametric models are also available. See details in \samp{fit.x} and \samp{fit.y}.
#'
#'
#' To compute the partial Spearman's rank correlation between \var{X} and \var{Y} adjusting for \var{Z} conditional on \var{Zc}, \samp{formula} is specified as \code{\var{X} | \var{Y} ~ \var{Z}} and \samp{conditional.by} is specified as \var{Zc}.
#' This indicates that models of \code{\var{X} ~ \var{Z}} and \code{\var{Y} ~ \var{Z}} will be fit, and the correlation between the probability-scale residuals from these two models will be modeled conditional on \var{Zc}.

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
#' @param conditional.by the name of the variable on which the partial Spearman's correlation is conditional. See \sQuote{Details}.
#'
#' @param conditional.method the method to be used for modeling conditional correlation between probability-scale residuals. The default option is \samp{lm}, which fits linear regression models for \var{Xres}\var{Yres} on \var{Zc}, \var{Xres^2} on \var{Zc}, and \var{Yres^2} on \var{Zc}, and then uses the fitted values to compute the Pearson's correlation between \var{Xres} and \var{Yres} conditional on \var{Zc}.
#' Other options include \samp{kernel}, which computes correlation between \var{Xres} and \var{Yres} conditional on \var{Zc} using a kernel weighted method, and \samp{stratification}, which computes the correlation between \var{Xres} and \var{Yres} seperately for each value of \var{Zc}.
#'
#' @param conditional.formula the formula to be used when \samp{conditional.method} is specified as \samp{lm}.
#'
#' @param kernel.function the kernel function to be used when \samp{conditional.method} is specified as \samp{kernel}. Defaults to \samp{normal}. Other options are \samp{triweight}, \samp{quartic}, \samp{biweight}, \samp{epanechnikov}, \samp{uniform}, and \samp{triangle}.
#'
#' @param kernel.bandwidth the kernel bandwidth to be used when \samp{conditional.method} is specified as \samp{kernel}. The default value is calculated using Silverman' rule. Users can also specify a positive numeric value.
#'
#' @param fit.x,fit.y the fitting functions used for the model of \var{X} or \var{Y} on
#' \var{Z}.  The default function is \samp{orm}, which fits cumulative link models for continuous or discrete ordinal variables. Other options include \samp{lm} (fit linear regression models and obtain the probability-scale residuals by assuming normality),
#' \samp{lm.emp} (fit linear regression and obtain the probability-scale residuals by empirical ranking),
#' \samp{poisson} (fit Poisson models for count variables), \samp{nb} (fit negative binomial models for count variables), and \samp{logistic} (fit logistic regression models for binary variables).
#'
#' @param link.x,link.y the link family to be used for the ordinal model of
#' \var{X} on \var{Z}.  Defaults to \samp{logit}. Other options are
#' \samp{probit}, \samp{cloglog}, \samp{cauchit}, and \samp{logistic} (equivalent with \samp{logit}). Used only when
#' \samp{fit.x} is \samp{orm}.
#'
#' @param data an optional data frame, list or environment (or object
#' coercible by \code{\link{as.data.frame}} to a data frame)
#' containing the variables in the model.  If not found in
#' \code{data}, the variables are taken from
#' \code{environment(formula)}, typically the environment from which
#' \code{conditional_Spearman} is called.
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
#' @return object of \samp{conditional_Spearman} class.
#' @export
#' @seealso \code{\link{print.conditional_Spearman}},\code{\link{print.conditional_Spearman}}
#' @examples
#' data(PResidData)
#' library(rms)
#' #### fitting cumulative link models for both Y and W
#' result <- conditional_Spearman(c|y~ x + w, conditional.by="w",
#'                                        conditional.method="lm", conditional.formula="~rcs(w)",
#'                                        fit.x="poisson",fit.y="orm",
#'                                        data=PResidData, fisher=TRUE)
#' plot(result)
#' @importFrom rms orm lrm
#' @importFrom SparseM as.matrix
#' @importFrom stats predict sd
#'


conditional_Spearman <- function(formula, conditional.by, data,
                                         conditional.method=c("lm", "kernel", "stratification"),
                                         conditional.formula= paste("~",conditional.by, sep=""),
                                         kernel.function=c("normal", "gaussian", "triweight", "quartic", "biweight", "epanechnikov","uniform", "triangle"),
                                         kernel.bandwidth="silverman",
                                         fit.x="orm", fit.y="orm",
                                         link.x=c("logit", "probit","cloglog", "loglog","cauchit", "logistic"),
                                         link.y = c("logit", "probit","cloglog", "loglog","cauchit", "logistic"),
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
                       logistic = ordinal.scores(mx, mxz, method="nlogit"),
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


    ans <- list(
      est=list(),
      fisher=fisher,
      conf.int=conf.int,
      data.points=data.points,
      conditional.by=conditional.by,
      conditional.method=conditional.method[1]
    )

    if(conditional.method[1]=="lm"){

      Fc <- as.Formula(conditional.formula)
      mc <- mf
      ## NOTE: we add variables X and Y to model frame call so that subsetting occurs correctly
      ## We then strip them off.
      mc[["formula"]] <- Fc
      mc[[yName]] <- Fy[[2]]
      mc[[xName]] <- Fx[[2]]
      mc <- eval(mc, parent.frame())
      mc[[paste('(',xName,')',sep='')]] <- NULL
      mc[[paste('(',yName,')',sep='')]] <- NULL

      #### this seems to not work
     # if(conditional.by!=attr(attr(mc, "terms"), "term.labels")) stop ("conditioinal.formual should only use conditional.by as the dependent variable")

      mcz <- model.matrix(attr(mc,'terms'),mc)
      zzint <- match("(Intercept)", colnames(mcz), nomatch = 0L)
      if(zzint > 0L) {
        mcz <- mcz[, -zzint, drop = FALSE]
      }
      score.prod <- lm.scores(score.xz$presid*score.yz$presid, mcz)
      score.xres2 <- lm.scores(score.xz$presid^2, mcz)
      score.yres2 <- lm.scores(score.yz$presid^2, mcz)

      npar.xz <- dim(score.xz$dl.dtheta)[2]
      npar.yz <- dim(score.yz$dl.dtheta)[2]
      npar.prod <- dim(score.prod$dl.dtheta)[2]
      npar.xres2 <- dim(score.xres2$dl.dtheta)[2]
      npar.yres2 <- dim(score.yres2$dl.dtheta)[2]

      Ntheta <- npar.xz + npar.yz + npar.prod + npar.xres2 + npar.yres2


      bigphi <- cbind(score.xz$dl.dtheta,
                      score.yz$dl.dtheta,
                      score.prod$dl.dtheta,
                      score.xres2$dl.dtheta,
                      score.yres2$dl.dtheta)

      A <- matrix(0, Ntheta, Ntheta)

      A[(1:npar.xz), (1:npar.xz)] <- as.matrix(score.xz$d2l.dtheta.dtheta)

      A[npar.xz + (1:npar.yz), npar.xz + (1:npar.yz)] <- as.matrix(score.yz$d2l.dtheta.dtheta)

      A[npar.xz + npar.yz + (1: npar.prod), npar.xz + npar.yz + (1: npar.prod)] <- score.prod$d2l.dtheta.dtheta

      if (npar.xres2>0){
        A[npar.xz + npar.yz + npar.prod+ (1: npar.xres2), npar.xz + npar.yz + npar.prod+ (1: npar.xres2)] <- score.xres2$d2l.dtheta.dtheta
      }

      if(npar.yres2>0){
        A[npar.xz + npar.yz + npar.prod+ npar.xres2+(1: npar.yres2), npar.xz + npar.yz + npar.prod+ npar.xres2+(1: npar.yres2)] <- score.yres2$d2l.dtheta.dtheta
      }

      par.1 <- t(score.yz$presid * cbind(1, mcz)) %*% t(score.xz$dpresid.dtheta)
      par.2 <- t(score.xz$presid * cbind(1, mcz)) %*% t(score.yz$dpresid.dtheta)
      A[npar.xz + npar.yz + (1: npar.prod), 1:npar.xz] <- par.1
      A[npar.xz + npar.yz + (1: npar.prod), npar.xz + (1:npar.yz)] <- par.2


      par.3 <- t(2*score.xz$presid * cbind(1, mcz)) %*% t(score.xz$dpresid.dtheta)
      par.4 <- t(2*score.yz$presid * cbind(1, mcz)) %*% t(score.yz$dpresid.dtheta)
      A[npar.xz + npar.yz + npar.prod+ (1: npar.xres2), 1:npar.xz] <- par.3
      A[npar.xz + npar.yz + npar.prod+ npar.xres2+(1: npar.yres2), npar.xz + (1:npar.yz)] <- par.4

      SS <- solve(A, t(bigphi))
      var.theta <- tcrossprod(SS, SS)

      prod.coef <- score.prod$mod$coef
      names(prod.coef) <- paste("prod:", names(prod.coef))
      yres2.coef <- score.yres2$mod$coef
      names(yres2.coef) <- paste("yres2:", names(yres2.coef))
      xres2.coef <- score.xres2$mod$coef
      names(xres2.coef) <- paste("xres2:", names(xres2.coef))

      est.coef <- c(prod.coef, xres2.coef, yres2.coef)

      var.coef <- var.theta[(npar.xz+npar.yz+1): Ntheta , (npar.xz+npar.yz+1): Ntheta]
      colnames(var.coef) <- rownames(var.coef) <- names(est.coef)
      #resid.prod <- score.xz$presid*score.yz$presid
      est.prod <- predict(score.prod$mod)
      est.xres2 <- predict(score.xres2$mod)
      est.yres2 <- predict(score.yres2$mod)
      est.gamma <- est.prod/ sqrt(est.xres2  * est.yres2)

      dgamma.dprod <- apply(cbind(1, mcz), 2, FUN=function(x) 1/sqrt(est.xres2*est.yres2)*x)
      dgamma.dxres2<- apply(cbind(1, mcz), 2, FUN=function(x)  - est.gamma/2/est.xres2 *x)
      dgamma.dyres2 <- apply(cbind(1, mcz), 2, FUN=function(x)  - est.gamma/2/est.yres2 *x)
      dgamma.dcoef <- cbind(dgamma.dprod, dgamma.dxres2, dgamma.dyres2)

      var.gamma <- diag(dgamma.dcoef %*% var.coef %*% t(dgamma.dcoef))

      pval.gamma = 2 * pnorm( -abs(est.gamma)/sqrt(var.gamma))

      if (fisher==TRUE){
        ####Fisher's transformation
        gamma_f <- log( (1+est.gamma)/(1-est.gamma) )
        var.gamma_f <- var.gamma*(2/(1-est.gamma^2))^2
        pval.gamma <- 2 * pnorm( -abs(gamma_f)/sqrt(var.gamma_f))
      }

      result.ci <- getCI(est.gamma, var.gamma, fisher=fisher, ci=conf.int)


      est <- data.frame(var=data[, conditional.by],
                        est=est.gamma,
                        stderr=sqrt(var.gamma),
                        p=pval.gamma,
                        'lower CI'=result.ci[, "lower"],
                        'upper CI'=result.ci[, "upper"])
      colnames(est)[1] <- conditional.by
      result <- list(est=est,
                     est.coef=est.coef,
                     var.coef=var.coef,
                     conditional.formula=Fc)


    } else if(conditional.method[1]=="kernel"){
      nw.kernel <- function(x, h, kernel){
        ws <- sapply(x, FUN=function(i) kernel.function( (i-x)/h, kernel=kernel)/sum(kernel.function( (i-x)/h, kernel=kernel)))
        return(ws)
      }

      if(kernel.bandwidth =="silverman"){
        h=1.06*sd(data[, conditional.by])*dim(data)[1]^(-1/5)

      } else if (is.numeric(kernel.bandwidth)){
        h=kernel.bandwidth
      } else stop("kernel.bandwidth needs to be silverman or a numeric value")

      wt <- nw.kernel(data[, conditional.by], h=h, kernel=kernel.function[1])
      weighted.cor <- apply(wt, 2,
                            FUN=function(wt) corr(cbind(score.xz$presid, score.yz$presid), wt))

      est <- cbind(data[, conditional.by], weighted.cor)
      colnames(est) <- c(conditional.by, "est")
      result <- list(est=est,
                     kernel.function=kernel.function[1],
                     kernel.bandwidth=h)
    }else if (conditional.method[1]=="stratification"){
      if(class(data[, conditional.by])=="character" ) data[, conditional.by]=factor(data[, conditional.by])

      if(is.null(levels(data[,conditional.by]))){
        stop("Please make sure the conditional.by variable is a factor if using stratification for conditional.method")
      }else{
        z.unique <- levels(data[, conditional.by])

        result <- t(sapply(1:length(z.unique),
                           FUN=function(i) corTS.stratification(ind=data[, conditional.by]==z.unique[i],
                                                                presid.x=score.xz$presid,
                                                                presid.y=score.yz$presid,
                                                                dl.dtheta.x=score.xz$dl.dtheta,
                                                                dl.dtheta.y=score.yz$dl.dtheta,
                                                                d2l.dtheta.dtheta.x=score.xz$d2l.dtheta.dtheta,
                                                                d2l.dtheta.dtheta.y=score.yz$d2l.dtheta.dtheta,
                                                                dpresid.dtheta.x=score.xz$dpresid.dtheta,
                                                                dpresid.dtheta.y=score.yz$dpresid.dtheta,
                                                                fisher=fisher )))

        result.ci <- getCI(result[,"TS"], result[, "var.TS"], fisher=fisher, ci=conf.int)
        est <- data.frame(var=z.unique,
                          est=result[,"TS"],
                          stderr=sqrt(result[, "var.TS"]),
                          p=result[, "pval.TS"],
                          'lower CI'=result.ci[, "lower"],
                          'upper CI'=result.ci[, "upper"])

        colnames(est)[1] <- conditional.by
        result <- list(est=est)
      }

    } else stop("Please specify conditional.method as lm, kernel, or stratification.")
    ans$est <- result
    ans <- structure(ans, class="conditional_Spearman")

    return(ans)
}
