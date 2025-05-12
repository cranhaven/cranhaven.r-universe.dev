#' Fit Functional Concurrent Regression
#'
#' @description
#' This function implements functional concurrent regression for sparse functional responses with both functional and scalar covariates.
#' This function is a wrapper for mgcv's \code{\link{gam}}/\code{\link{bam}}.
#'
#' @param formula
#'            formula will accept any input formula which is valid for \code{\link{gam}}.
#'            The formula should only include terms not associated with the random function intercept b_i(t_{ij}). See Examples.
#'
#' @param argvals a string indicating the functional domain variable name in data
#'
#' @param subj a string indicating the unique subject identifier name in data
#'
#' @param argvals.new new values of the functional domanin to predict using \code{\link{face.sparse}}, optional
#'   if one desires to predict at points of the functional domain not included in the data fitting procedure,
#'   they must be supplied in this argument.
#'
#' @param data dataframe including all variables of interest. Must not have any missing data for variables used in model fitting.
#'   data must also not contain any variables named: "g", "phi" followed by any numbers, or "sp" followed by any numbers. These
#'   names are reserved for the fitting procedure.
#'
#' @param niter number of times to iterate the covariance estimation
#'
#' @param sp logical arguement indicating whether smoothing parameters for random effects should be supplied to
#'   \code{\link{gam}} or \code{\link{bam}} using estimates from \code{\link{face.sparse}}
#'   (TRUE), or whether smoothing parameters for random effects should
#'   be estimated by mgcv (FALSE). Defaults to FALSE.
#'
#' @param nPhi number of random effects to include in final model (i.e. number of eigenfunctions of the covariance function).
#'   Default value (NULL) results in the use of all estimated random effects.
#'
#' @param use_bam logical argument indicating whether to use \code{\link{gam}} or \code{\link{bam}}.
#'    For moderate or large number of eigenfunctions
#'    it is recommended to use \code{\link{bam}}.
#'
#' @param discrete logical argument indicating whether whether to supple discrete = TRUE argument to \code{\link{bam}}.
#'    This argument may reduce computation time, but is currently listed as ``experimental". Not available when use_bam = FALSE.
#'    Defaults to FALSE.
#'
#' @param face.args list of arguments to pass to \code{\link{face.sparse}}. Can not pass the arguments ``data", ``newdata", ``center" or ``argvals.new"
#'     as these are determined by the procedure.
#'
#' @param ...  arguments to be passed to mgcv::gam()/bam()
#'
#' @details
#'
#' The models fit are of the form
#'
#' \deqn{y = f_0(t_{ij}) + f_1(t_{ij})X_{ij} + ... + b_i(t_{ij}) + \epsilon_{ij}}
#'
#' Note that this function will accept any valid formula for \code{\link{gam}}/\code{\link{bam}}.
#' However, only the identity link function is available at this time.
#' See the package vignettes for additional descriptions of dynamic prediction and the class of models fit by this function.
#'
#'
#' @return An object of class \code{fcr} containing five elements
#' \describe{
#' \item{fit}{An object corresponding to the fitted model from the mgcv package}
#' \item{face.object}{An object corresponding to the estimated covariance features}
#' \item{runtime}{Model fitting time}
#' \item{argvals}{Character scalar corresponding the name of the functional domain variable}
#' \item{runtime}{logical scalar corresponding to sp argument used in model fitting}}
#'
#' @examples
#'
#' \dontshow{
#' ## toy example, use small number of interior knots for fpca/small number of
#' ## eigenfunctions to speed up computation time. See example below for a more realistic analysis.
#' data <- content[1:1000,]
#' k <- 4
#' K <- 3
#' fit <- fcr(formula = Y ~ s(argvals, k = K) + Male,
#'            argvals = "argvals", subj = "subj", data = data, use_bam=TRUE,
#'            nPhi = 1,
#'            face.arges = list(knots = k))
#' }
#'
#'
#' \donttest{
#'
#' data <- content
#' ## smoothing parameters
#' k <- 12  # number of interior knots for fpca (results in k + 3 basis functions)
#' K <- 15 # dimenson of smooth for time varying coefficients
#'
#' ## functional domain where we need predictions
#' tnew <- sort(unique(data$argvals))
#'
#' ###########################################
#' ## Step 1: Smooth time-varying covariate ##
#' ###########################################
#' dat.waz <- data.frame("y" = data$waz, "subj" = data$subj, argvals = data$argvals)
#' fit.waz <- face.sparse(dat.waz, newdata = dat.waz, knots = k, argvals.new = tnew)
#' data$wazPred <- fit.waz$y.pred
#'
#'
#' #####################
#' ## Step 2: Fit fcr ##
#' #####################
#' fit <- fcr(formula = Y ~ s(argvals, k=K, bs="ps") +
#'                          s(argvals, by=Male, k=K, bs="ps") +
#'                          s(argvals, by=wazPred, bs="ps"),
#'            argvals = "argvals", subj="subj", data=data, use_bam=TRUE, argvals.new=tnew,
#'            face.args = list(knots=k, pve=0.99))
#'
#' ## plot covariance features
#' plot(fit, plot.covariance=TRUE)
#'
#' ## plot coefficient functions and qq plots for random effects
#' plot(fit)
#'
#' ########################
#' ## Step 3: Prediction ##
#' ########################
#' ## data frames for in-sample and dynamic predictions
#' data_dyn <- data_in <- data
#'
#' ## change subject IDs to values not used in model fitting
#' ## for dynamic prediction
#' data_dyn$subj <- data_dyn$subj + 1000
#'
#' ## make all observations beyond 0.5 NA in both data frames
#' ## and dynamically predict the concurrent covariate in
#' ## dynamic prediction
#' inx_na <- which(data_dyn$argvals > 0.5)
#' data_dyn$Y[inx_na] <- data_dyn$waz[inx_na] <- NA
#' data_dyn$wazPred <- predict(fit.waz,
#'                             newdata= data.frame("subj" = data_dyn$subj,
#'                                                 "argvals" = data_dyn$argvals,
#'                                                 "y" = data_dyn$Y))$y.pred
#'
#' data_in$Y[inx_na]  <- NA
#'
#'
#' ## in sample and dynamic predictions on the same subjects
#' insample_preds  <- predict(fit, newdata = data)
#' dynamic_preds   <- predict(fit, newdata = data_dyn)
#'
#'}
#'
#' @references
#'
#' Jaganath D, Saito M Giman RH Queirox DM, Rocha GA, Cama V, Cabrera L, Kelleher D, Windle HJ,
#' Crabtree JE, Jean E, Checkley W. First Detected Helicobacter pylori Infection in Infancy Modifies the
#' Association Between Diarrheal Disease and Childhood Growth in Peru. Helicobacter (2014); 19:272-297.
#'
#' Leroux A, Xiao L, Crainiceanu C, Checkley W (2017). Dynamic prediction in functional concurrent
#' regression with an application to child growth.
#'
#' Xiao L, Li C, Checkley W, Crainiceanu C. Fast covariance estimation for sparse functional data.
#' Statistics and Computing, (2017).
#'
#' @importFrom stats predict as.formula coef complete.cases
#'
#' @importFrom utils data
#'
#' @export
#'
fcr <- function(formula, argvals, subj, argvals.new = NULL, data = NULL, niter = 1,
                sp = FALSE, nPhi = NULL, use_bam = FALSE, discrete = FALSE,
                face.args = list(knots = 12, lower = -3, pve=0.95),
                ...){
        stopifnot(class(data) == "data.frame")
        stopifnot(class(subj) == "character")
        stopifnot(is.list(face.args))
        stopifnot(!any(c("data","newdata","center","argvals.new") %in% names(face.args)))
        stopifnot(all(subj %in% names(data), argvals %in% names(data)))
        if(!use_bam & discrete) stop("discrete = TRUE is only available with use_bam = TRUE")
        if(any(grepl("^phi[0-9]+|^sp[0-9]+",colnames(data)))){
                stop("column names `sp[0-9]+` and `phi[0-9]+ are reserved`")
        }
        if("g" %in% colnames(data)){
                stop("column name `g` is reserved`")
        }

        ## check to see if new predictions are in the range of the data used to fit the model
        if(!is.null(argvals.new)){
           if(max(argvals.new) > max(data[[argvals]]) | min(argvals.new) < min(data[[argvals]])) {
                   warning("Range of arvals.new exceeds the range of the funcitonal domain in the data.")
           }
        }

        model <- as.character(formula)
        stopifnot(model[1] == "~" & length(model) == 3)
        outcome <- model[[2]]

        if(!outcome %in% names(data)) stop("Outcome variable must be named and included in data.")

        fit_pilot <- gam(as.formula(formula), data = data, ...)
        curEst_fx <- fit_pilot$fitted.values

        if(length(curEst_fx) != nrow(data)) stop("Method not implemented to handle missing data in model fitting. Please remove missing data and refit.")

        run_time <- c()

        data$g  <- factor(data[[subj]])
        if(is.null(argvals.new)){
                ut <- sort(unique(c(data[[argvals]],
                                    seq(min(data[[argvals]]), max(data[[argvals]]), len =100))))
        } else {
                ut <- sort(unique(c(argvals.new, data[[argvals]],
                                    seq(min(data[[argvals]]), max(data[[argvals]]), len =100))))
        }

        for(n in 1:niter){
                start_time <- proc.time()

                resid1  <- curEst_fx - data[[outcome]]
                datCest <- data.frame("argvals" = as.vector(data[[argvals]]),
                                      "subj" = as.vector(data[[subj]]),
                                      "y" = as.vector(resid1))

                Cest1  <- do.call("face.sparse",
                                  c(list(data = datCest, newdata = datCest,
                                         center = FALSE, argvals.new = ut),
                                    face.args))


                if(is.null(nPhi)){
                        nPhi <- length(Cest1$eigenvalues)
                } else {
                        nPhi <- min(length(Cest1$eigenvalues), nPhi)
                }
                message(paste("Number of Eigenfunctions used in estimation =", nPhi))

                data <- createPhi(Cest1, data = data, argvals = argvals, nPhi = nPhi)

                if(use_bam){
                        fit <- bam(formula = as.formula(createFormula(Cest1, formula = formula, sp = sp, nPhi = nPhi)),
                                   data=data, discrete = discrete, ...)
                } else if (!use_bam){
                        fit <- gam(formula = as.formula(createFormula(Cest1, formula = formula, sp = sp, nPhi = nPhi)),
                                   data=data, ...)
                }
                run_time[[n]] <- proc.time() - start_time

                curEst <- fit$fitted.values

                if(niter > 1){
                        coef_names <- names(coef(fit))
                        phi_names  <- unique(regmatches(coef_names, regexpr("phi[0-9]+", coef_names)))
                        curEst_fx  <- predict(fit, exclude = paste("s(g):", phi_names, sep=""))
                        nPhi <- NULL

                        rm_cols <- which(grepl("^phi[0-9]+",colnames(data)))
                        data[rm_cols] <- NULL
                }

                rm(list=ls()[which(ls() %in% paste("sp",0:10000,sep=""))])
        }

        ret        <- list("fit" = fit,
                           "face.object" = Cest1,
                           "runtime" = run_time,
                           "argvals" = argvals,
                           "sp" = sp)
        class(ret) <- "fcr"
        ret
}
