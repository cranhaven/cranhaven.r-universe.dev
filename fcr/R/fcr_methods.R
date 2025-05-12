#' Prediction for fcr
#'
#' Predict method for \code{\link{fcr}}
#'
#' @param object object of class fcr.
#'
#' @param newdata data frame including all predictors used in the model fitting procedure.
#'        missing values for the responses are OK. Missing covariate values will result in pairwise deletion with
#'        a warning message.
#'
#' @param type defaults to link (i.e. response). See \code{\link{predict.gam}} for additional information.
#'
#' @param ... additional arguments to be passed to \code{\link{predict.gam}}
#'
#' @return An object containing two elements
#' \describe{
#' \item{dynamic_predictions}{Predictions corresponding to dynamic predictions (i.e. subject ids not included in the original fitting).
#'  Note that these predictions are slower and do not incorporate the estimated covariance between random effects and fixed effects in making
#'  predictions. This is different than the in sample predictions which account for this estimated covariance. See \code{\link{predict.gam}}
#'  for more information.
#'
#'  \itemize{
#'  \item fitted.values
#'    \itemize{
#'    \item y.pred fitted \eqn{\hat{y}}
#'    \item se.fit standard errors used to create confidence intervals for \eqn{\hat{y}}, \eqn{\sqrt{var(y)}}
#'    \item se.fit.p standard errors used for creating prediction intervals for \eqn{\hat{y}}, \eqn{\sqrt{var(y) + \hat{\sigma^2}}}
#'    \item random subject specific random effects \eqn{b_i}
#'
#'    }
#'  \item scores matrix of BLUPs for subjects' eigenscores
#'  \item data newdata supplied to the function
#'
#'  }
#'
#'  }
#' \item{insample_predictions}{Predictions for subject ids included in the original fitting. This returns all output from the relevant
#' \code{\link{predict.gam}}/\code{\link{predict.bam}} call. }}
#'
#' @examples
#' ## see examples in fcr
#'
#' @importFrom stats predict
#'
#' @method predict fcr
#' @S3method predict fcr
#'
#' @export
#'
predict.fcr <- function(object, newdata, type="link", ...){
        ## error checking
        stopifnot(class(object) == "fcr")
        stopifnot(is.data.frame(newdata))
        stopifnot("subj" %in% colnames(newdata))
        stopifnot(all(is.finite(newdata$subj)))
        stopifnot(type %in% c("link","terms","iterms","lpmatrix"))
        if(any(grepl("^phi[0-9]+|^sp[0-9]+",colnames(data)))){
                stop("column names `sp[0-9]+` and `phi[0-9]+ are reserved`")
        }
        if("g" %in% colnames(data)){
                stop("column name `g` is reserved`")
        }


        fit     <- object$fit
        argvals <- object$argvals
        outcome <- as.character(object$fit$formula)[2]

        if(sum(!newdata[[argvals]] %in% object$face.object$argvals.new) != 0) {
                rmidx   <- which(!newdata[[argvals]] %in% object$face.object$argvals.new)
                newdata <- newdata[-rmidx,]
                warning(paste(length(rmidx),
                              " rows of newdata contained values of the functional domain not fit",
                              "and were removed. Please refit the model with all desired predicted",
                              "values supplied using the `argvals.new` argument."))
        }

        coef_names <- names(coef(fit))
        phi_names  <- unique(regmatches(coef_names, regexpr("phi[0-9]+", coef_names)))

        for(k in phi_names) {
                newdata[[k]] <- 0
        }

        subj_in  <- unique(fit$model$g)
        data_in  <- subset(newdata, newdata$subj %in% subj_in)
        data_out <- subset(newdata, !(newdata$subj %in% subj_in))

        if(type != "link" & nrow(data_out) > 0){
                stop(paste("type", type, "not supported for subjects not included in model fitting."))
        }

        if(nrow(data_in) > 0){
                data_in <- createPhi(object$face.object, data = data_in, argvals = argvals, nPhi = length(phi_names))

                data_in[["g"]] <- data_in[["subj"]]
                pred_in <- predict(fit, newdata = data_in, type = type, ...)

                if(nrow(data_out) == 0) return(list("dynamic_predictions" = NA,
                                                    "insample_predictions" = pred_in))
        } else {
                pred_in <- NA
        }
        data_out[["g"]] <- sort(unique(fit$model$g))[1]


        s2    <- c(fit$sig2)
        uid   <- unique(data_out$subj)
        ut    <- object$face.object$argvals.new

        if(object$sp) {
                sp    <- fit$full.sp[grepl("phi", names(fit$full.sp))]
        } else if (!object$sp) {
                sp    <- fit$sp[grepl("phi", names(fit$sp))]
        }

        preds <- scores <- random <- se <- se_p <- c()

        inx_lp <- which(!grepl("s\\(g\\):phi[0-9]+", coef_names))
        Vp0    <- fit$Vp[inx_lp,inx_lp]

        for(i in 1:length(uid)) {
                tmp_f   <- subset(data_out, data_out$subj == uid[i])


                miss_outcome <- is.na(tmp_f[[outcome]])
                tmp          <- tmp_f[!miss_outcome,]

                Z_f <- matrix(NA, ncol = length(phi_names), nrow = nrow(tmp_f))
                for(k in 1:length(phi_names)) {
                        Z_f[,k] <- vapply(tmp_f[[argvals]], function(x){
                                object$face.object$eigenfunctions[ut==x,k]
                        }, numeric(1))
                }
                Z <- Z_f[!miss_outcome,,drop=FALSE]

                Xb_f <- predict(fit,
                                newdata = tmp_f,
                                exclude = paste("s(g):", phi_names,")", sep=""))
                Xb   <- Xb_f[!miss_outcome]


                G <- diag(s2/sp)

                if(nrow(tmp) > 1)  R <- diag(rep(s2, nrow(tmp)))
                if(nrow(tmp) == 1) R <- s2

                V <- Z %*% G %*% t(Z) + R

                ei <- G %*% t(Z) %*% solve(V) %*% (tmp[[outcome]] - Xb)
                bi <- as.vector(t(ei) %*% t(Z_f))

                v_y <- Z_f %*% (G - G %*% t(Z) %*% solve(V) %*% Z %*% G) %*% t(Z_f)
                X1.test <- predict(fit,tmp_f,type="lpmatrix")[,inx_lp]
                v_y <- v_y + X1.test%*%Vp0%*%t(X1.test)


                scores <- c(scores, ei)
                random <- c(random, bi)
                se     <- c(se, sqrt(diag(v_y)))
                se_p   <- c(se_p, sqrt(diag(v_y) + s2))
                preds  <- c(preds, Xb_f + bi)
        }

        dyn_pred <- list("fitted.values" = data.frame("y.pred" = preds,   ## BLUP for y
                                                      "se.fit" = se,      ## confidence se
                                                      "se.fit.p" = se_p,  ## prediction se
                                                      "random" = random), ## BLUP for u
                         "scores" = matrix(scores, ncol=ncol(object$face.object$eigenfunctions),
                                           nrow=length(uid), byrow=TRUE),
                         "data" = newdata)

        list("dynamic_predictions" = dyn_pred,
             "insample_predictions" = pred_in)

}





#' Plotting an fcr model fit
#'
#' Plot method for \code{\link{fcr}}. Takes a fitted fcr object and plots either the features of the covariance function, or the
#' smooth terms and qqplots for random effects. See \code{\link{plot.gam}} for further details.
#'
#' @param x object of class fcr.
#'
#' @param plot.covariance logical argument, indicates whether to plot features of the covariance
#'        function (correlation function, variance function, and eigenfunctions of the covariance function).
#'        If FALSE, will call \code{\link{plot.gam}} on the fitted gam/bam object. See \code{\link{plot.gam}} for additional details.
#'        Defaults to FALSE.
#'
#' @param ... additional arguments to be passed to \code{\link{plot.gam}}.
#'
#'
#' @return If plot.covariance is FALSE, this function will silently return a list fo the data used to create the plots.
#'
#' @examples
#'
#' ## see examples in fcr
#'
#' @importFrom graphics par plot matplot legend
#'
#' @method plot fcr
#' @S3method plot fcr
#'
#' @export
#'
plot.fcr <- function(x, plot.covariance = FALSE, ...){
        stopifnot(class(x) == "fcr")

        if(plot.covariance) {
                oldpar <- par()$ask
                par(ask=TRUE)

                tnew <- x$face.object$argvals.new
                inx <- which(tnew %in% seq(min(tnew), max(tnew), len = 100))

                image.plot(tnew[inx],tnew[inx], x$face.object$Cor.new[inx,inx],
                           xlab="Functional Domain", ylab="Functional Domain", main = "Correlation Function")

                plot(tnew[inx], x$face.object$Chat.raw.diag.new[inx], xlab="Functional Domain",
                     ylab="", main = "Variance Function", type="l")

                matplot(x$face.object$eigenfunctions[inx,], xlab="Functional Domain", ylab="",
                     main="Eigenfunctions of the Covariance Function", type='l')
                evals <- x$face.object$eigenvalues
                evals <- sprintf("%5.3f",evals)
                evals[evals == "0.000"] <- "<0.000"
                legend("top", legend = paste("eval", 1:length(evals), " = ", evals ,sep=""),
                       col=1:length(evals), lty = 1:length(evals),
                       bty='n',ncol=3)

                par(ask=oldpar)
        }

        if(!plot.covariance) {
                plot(x$fit, ...)
        }
}




