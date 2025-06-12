#' Treatment effect estimation and hypothesis testing
#'
#' Estimate treatment effect size. Estimate variation and conduct hypothesis
#' testing by bootstrap analysis.
#'
#' @param imp.rst A class \code{IDEMIMP} object returned from
#'     \code{\link{imImpAll}}
#' @param n.boot Number of bootstrap samples
#' @param n.cores Number of cores for parallel computation. Fixed at 1 for Windows.
#' @param effect.quantiles Composite quantiles of interest for measuring
#'     treatment effect
#' @param update.progress Parameter reserved for run \code{idem} in GUI mode
#' @param quant.ci Quantiles for extracting bootstrap confidence intervals
#' @param ... Extra options for ranking subjects using the composite endpoint
#'     that include \itemize{ \item{\code{cut.z}: }{ Clinically meaningful difference in
#'     the functional outcome} \item{\code{cut.surv}: }{ Clinically meaningful difference
#'     in survival time}}
#'
#' @param seed Random seed
#'
#' @details
#'
#' If \code{n.boot=0}, bootstrap analysis will not be conducted. Instead, only
#' the treatment effect size will be estimated using the imputed data.
#'
#' @return
#'
#' A class \code{IDEMTEST} list containing
#'
#' \describe{
#' \item{lst.var}{List of specification parameters}
#'
#' \item{deltas}{Vector of sensitivity parameters}
#'
#' \item{theta}{A data frame with columns
#' \itemize{
#' \item \code{Delta0}: Sensitivity parameter for control arm
#' \item \code{Delta1}: Sensitivity parameter for intervention arm
#' \item \code{Theta}: Estimated \eqn{\theta}
#' \item \code{SD}: Standard deviation (when \code{n.boot >0})
#' \item \code{PValue}: p-value (when \code{n.boot >0}}
#' }
#'
#' \item{effect.quantiles}{A data frame with columns
#'
#' \itemize{
#' \item \code{Delta}:Sensitivity parameter
#' \item \code{TRT}:Treatment arm
#' \item \code{Q}: Quantiles of the composite endpoint to be estimated
#'
#' \item \code{QuantY}: Estimated quantiles if the quantiles correspond to
#' functional outcome (when \code{n.boot >0})
#'
#' \item \code{QuantSurv}: Estimated quantiles if the quantiles correspond to
#' survival days (when \code{n.boot >0})
#'
#' \item \code{Q}: Boostrap quantiles for the QuantY (when \code{n.boot >0})
#'
#' \item \code{QSurv}: Boostrap quantiles for the QuantSurv (when
#' \code{n.boot >0}) }}
#'
#' \item{bootstrap}{A list with length \code{n.boot}. The \eqn{i}th item is the
#'     class \code{IDEMEST} list corresponding to the \eqn{i}th bootstrap
#'     sample} }
#'
#' @examples
#' \dontrun{
#' rst.abc <- imData(abc, trt="TRT", surv="SURV", outcome=c("Y1","Y2"),
#'                  y0=NULL, endfml="Y2",
#'                  trt.label = c("UC+SBT", "SAT+SBT"),
#'                  cov=c("AGE"), duration=365, bounds=c(0,100));
#' rst.fit  <- imFitModel(rst.abc);
#' rst.imp <- imImpAll(rst.fit, deltas=c(-0.25,0,0.25),
#'                     normal=TRUE, chains = 2, iter = 2000, warmup = 1000);
#' rst.est <- imInfer(rst.imp, n.boot = 0, effect.quantiles = c(0.25,0.5,0.75));
#' rst.test <- imInfer(rst.imp, n.boot = 100, effect.quantiles = c(0.25,0.5,0.75));}
#'
#' @export
#'
imInfer <- function(imp.rst,
                    n.boot = 0,
                    n.cores = 1,
                    update.progress=NULL,
                    effect.quantiles=c(0.25,0.5,0.75),
                    quant.ci=c(0.025, 0.975),
                    ...,
                    seed = NULL) {

    stopifnot(get.const("IMP.CLASS") %in% class(imp.rst));
    stopifnot(!is.null(imp.rst$org.data));

    if( .Platform$OS.type == "windows" && n.cores > 1) {
        warning("n.cores is set to be 1 on Windows.");
        n.cores <- 1;
    }

    if (is.numeric(seed)) {
        old.seed.kind <- RNGkind("L'Ecuyer-CMRG");
        old.seed      <- .Random.seed;
        set.seed(seed);
    }

    ##original result
    rst.org <- get.estimate(imp.rst,
                            effect.quantiles=effect.quantiles);

    if (0 == n.boot) {
        rtn.rst  <- list(theta            = rst.org$theta,
                         effect.quantiles = rst.org$effect.quantiles,
                         survivor         = rst.org$survivor);
    } else {
        data.all  <- imp.rst$org.data
        lst.var   <- imp.rst$lst.var
        deltas    <- imp.rst$deltas
        n.imp     <- imp.rst$n.imp
        imp.par   <- imp.rst$imp.par
        normal    <- imp.rst$normal
        use_mice  <- imp.rst$use_mice
        n.cores   <- min(n.cores, parallel::detectCores()-1);

        if ("PROGRESS" %in% toupper(class(update.progress))
            & n.boot > 0)
            update.progress$set(value=1, detail=paste(""));

        rst.bs <- parallel::mclapply(1:n.boot,
                                     function(x) {
                                if ("PROGRESS" %in% toupper(class(update.progress)) & n.boot > 0) {
                                    update.progress$set(value=x/n.boot,
                                                        detail=paste("Bootstrap", x, sep=" "));
                                } else {
                                    cat("---- Bootstrap", x, "\n");
                                }
                                get.boot.single(data.all,
                                                lst.var,
                                                deltas   = deltas,
                                                n.imp    = n.imp,
                                                normal   = normal,
                                                imp.par  = imp.par,
                                                use_mice = use_mice,
                                                effect.quantiles=effect.quantiles);
                            }, mc.cores=n.cores);

        rst.test <- get.tests(rst.org, rst.bs,
                              duration = imp.rst$lst.var$duration,
                              quantiles = quant.ci);

        ##return
        rtn.rst  <- list(theta            = rst.test$theta,
                         effect.quantiles = rst.test$effect.quantiles,
                         survivor         = rst.test$survivor,
                         bootstrap        = rst.bs);
    }


    ##reset seed
    if (is.numeric(seed)) {
        RNGkind(old.seed.kind[1]);
        .Random.seed <- old.seed;
    }

    ##return
    rtn.rst <- c(rtn.rst,
                 list(lst.var = rst.org$lst.var,
                      deltas  = rst.org$deltas));
    class(rtn.rst) <- get.const("TEST.CLASS")

    rtn.rst
}


#' Print inference results
#'
#' Print method of class \code{IDEMINFER} for treatment effect estimation and
#' hypothesis testing results
#'
#'
#' @param x A class \code{IDEMINFER} list generated by \code{\link{imInfer}}
#' @param delta0 Selected treatment arm 0 sensitivity parameters
#' @param delta1 Selected treatment arm 1 sensitivity parameters
#' @param ... Extra arguments
#'
#' @examples
#'
#' \dontrun{
#' rst.abc <- imData(abc, trt="TRT", surv="SURV", outcome=c("Y1","Y2"),
#'                  y0=NULL, endfml="Y2",
#'                  trt.label = c("UC+SBT", "SAT+SBT"),
#'                  cov=c("AGE"), duration=365, bounds=c(0,100));
#' rst.fit <- imFitModel(rst.abc);
#' rst.imp <- imImpAll(rst.fit, deltas=c(-0.25,0,0.25),
#'                     normal=TRUE, chains = 2, iter = 2000, warmup = 1000);
#' rst.test <- imInfer(rst.imp, n.boot = 100);
#' print(rst.test, delta0 = 0, delta1  = 0.15)}
#'
#' @export
#'
print.IDEMINFER <- function(x, delta0=NULL, delta1=NULL, ...) {
    cat("\nThe sensitivity parameters considered were\n");
    print(x$deltas);

    rst <- get.theta.quant(x, delta0 = delta0, delta1 = delta1)

    if (0 == length(x$bootstrap)) {
        cat("\n\nPlease conduct bootstrap analysis for hypothesis testing.\n")
    } else {
        cat("\n\nThe hypothesis testing and confidence intervals are \n");
        cat("based on", length(x$bootstrap), "bootstrap samples.")

        if (length(x$bootstrap) < 100)
            cat(" Please consider more \nbootstrap samples (e.g. >100) for the validity \nof the results.\n");
        cat("\n");
    }

    invisible(rst)
}


#' Plot hypothesis testing results
#'
#' Generate contour plot of p-values or treatment effect theta for sensitivity
#' analysis results
#'
#' @param x A class \code{IDEMINFER} list generated by \code{\link{imInfer}}
#' @param opt contour plots of \code{pvalue} or \code{effect}
#' @param con.v Levels of contour plot
#' @param nlevels Levels of color scale
#' @param ... Options for \code{filled.contour}
#' @details
#'
#' The plot will only be generated when bootstrap analysis has been conducted.
#'
#' @examples
#' \dontrun{
#' rst.abc <- imData(abc, trt="TRT", surv="SURV", outcome=c("Y1","Y2"),
#'                  y0=NULL, endfml="Y2",
#'                  trt.label = c("UC+SBT", "SAT+SBT"),
#'                  cov=c("AGE"), duration=365, bounds=c(0,100));
#' rst.fit  <- imFitModel(rst.abc);
#' rst.imp <- imImpAll(rst.fit, deltas=c(-0.25,0,0.25),
#'                     normal=TRUE, chains = 2, iter = 2000, warmup = 1000);
#' rst.est <- imInfer(rst.imp, n.boot = 100);
#' plot(rst.est);}
#'
#' @method plot IDEMINFER
#'
#' @export
#'
plot.IDEMINFER <- function(x, con.v=0.05, nlevels=30, opt = c("pvalue", "effect"), ...) {

    if (0 == length(x$bootstrap)) {
        print("Please conduct bootstrap analysis first.")
        return();
    }

    opt <- match.arg(opt);
    col.var <- switch(opt,
                      pvalue = "PValue",
                      effect = "Theta")


    lst.var <- x$lst.var;
    trt.len <- NULL;
    get.para(lst.var, environment());

    cur.data <- x$theta;
    plot.contour(cur.data, trt.len, col.var, con.v = con.v, nlevels = 30, ...);
}
