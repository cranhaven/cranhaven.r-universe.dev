#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: lcc.R                                                         #
# Contains: lcc function                                              #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 06/10/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Longitudinal Concordance Correlation (LCC) Estimated by Fixed
##'   Effects and Variance Components using a Polynomial Mixed-Effects
##'   Regression Model
##'
##' @description The \code{lcc} function gives fitted values and
##'   non-parametric bootstrap confidence intervals for LCC,
##'   longitudinal Pearson correlation (LPC), and longitudinal accuracy
##'   (LA) statistics. These statistics can be estimated using different
##'   structures for the variance-covariance matrix for random effects
##'   and variance functions to model heteroscedasticity among the
##'   within-group errors using or not the time as a covariate.
##'
##' @usage
##' lcc(data, resp, subject, method, time, interaction, qf,
##'     qr, covar, gs, pdmat, var.class, weights.form, time_lcc, ci,
##'     percentileMet, alpha, nboot, show.warnings, components,
##'     REML, lme.control, numCore)
##'
##' @param data an object of class \code{data.frame}.
##'
##' @param resp character string. Name of the response variable in the
##'   data set.
##'
##' @param subject character string. Name of the subject variable in the
##'   data set.
##'
##' @param method character string. Name of the method variable in the
##'   data set. The first level of method is used as the gold-standard
##'   method.
##'
##' @param time character string. Name of the time variable in the data
##'   set.
##'
##' @param interaction an option to estimate the interaction effect
##'   between \code{method} and \code{time}. If \code{TRUE}, the
##'   default, interaction effect is estimated. If \code{FALSE} only the
##'   main effects of time and method are estimated.
##'
##' @param qf an integer specifying the degree time polynomial trends,
##'   normally 1, 2 or 3. (Degree 0 is not allowed). Default is
##'   \code{qf=1}
##'
##' @param qr an integer specifying random effects terms to account for
##'   subject-to-subject variation.  Note that \code{qr=0} specifies a
##'   random intercept (form \code{~ 1|subject}); \code{qr=1} specifies
##'   random intercept and slope (form \code{~ time|subject}). If
##'   \code{qr=qf=q}, with \eqn{q \ge 1}, random effects at subject
##'   level are added to all terms of the time polynomial regression
##'   (form \code{~ poly(time, q, raw = TRUE)|subject}). Default is
##'   \code{qr=0}.
##'
##' @param covar character vector. Name of the covariates to be included
##'   in the model as fixed effects. Default to \code{NULL}, never
##'   include.
##'
##' @param gs character string. Name of method level which represents
##'   the gold-standard. Default is the first level of method.
##'
##' @param pdmat standard classes of positive-definite matrix structures
##'   defined in the \code{\link[nlme]{pdClasses}} function. The
##'   different positive-definite matrices structures available in the
##'   \code{lcc} function are \code{pdSymm}, the default,
##'   \code{pdLogChol}, \code{pdDiag}, \code{pdIdent},
##'   \code{pdCompSymm}, and \code{pdNatural}.
##'
##' @param var.class standard classes of variance functions to model the
##'   variance structure of within-group errors using covariates, see
##'   \code{\link[nlme]{varClasses}}. Default to \code{NULL}, correspond
##'   to homoscedastic within-group errors. Available standard classes:
##'   \describe{ \item{\code{varIdent}:}{allows different variances
##'   according to the levels of the stratification variable.}
##'   \item{\code{varExp}:}{exponential function of the variance
##'   covariate; see \code{\link[nlme]{varExp}}.}}
##'
##' @param weights.form character string. An one-sided formula
##'   specifying a variance covariate and, optionally, a grouping factor
##'   for the variance parameters in the \code{var.class}. If
##'   \code{var.class=varIdent}, the option ``method'', form
##'   \code{~1|method} or ``time.ident'', form \code{~1|time}, must be
##'   used in the \code{weights.form} argument. If
##'   \code{var.class=varExp}, the option ``time'', form \code{~time},
##'   or ``both'', form \code{~time|method}, must be used in the
##'   \code{weights.form} argument.
##'
##' @param time_lcc regular sequence for time variable merged with
##'   specific or experimental time values used for LCC, LPC, and LA
##'   predictions. Default is \code{NULL}. The list may contain the
##'   following components: \describe{ \item{\code{time}:}{a vector of
##'   specific or experimental time values of given length. The
##'   experimental time values are used as default.  }
##'   \item{\code{from}:}{the starting (minimum) value of time
##'   variable.}
##'
##' \item{\code{to}:}{the end (maximum) value of time variable.}
##'
##' \item{\code{n}:}{an integer specifying the desired length of the
##' sequence. Generally, \code{n} between 30 and 50 is adequate.}  }
##'
##' @param ci an optional non-parametric boostrap confidence interval
##'   calculated for the LCC, LPC and LA statistics. If \code{TRUE}
##'   confidence intervals are calculated and printed in the
##'   output. Default is \code{FALSE}.
##'
##' @param percentileMet an optional method for calculating the
##'   non-parametric bootstrap intervals. If \code{FALSE}, the default,
##'   is the normal approximation method. If \code{TRUE}, the percentile
##'   method is used instead.
##'
##' @param alpha significance level. Default is 0.05.
##'
##' @param nboot an integer specifying the number of bootstrap
##'   samples. Default is 5,000.
##'
##' @param show.warnings an optional argument that shows the number of
##'   convergence errors in the bootstrap samples. If \code{TRUE} shows
##'   in which bootstrap sample the error occurred. If \code{FALSE}, the
##'   default, shows the total number of convergence errors.
##'
##' @param components an option to print LPC and LA statistics. If
##'   \code{TRUE} the estimates and confidence intervals for LPC and LA
##'   are printed in the output. If \code{FALSE}, the default, provides
##'   estimates and confidence interval only for the LCC statistic.
##'
##' @param REML if \code{TRUE}, the default, the model is fit by
##'   maximizing the restricted log-likelihood. If \code{FALSE} the
##'   log-likelihood is maximized.
##'
##' @param lme.control a list of control values for the estimation
##'   algorithm to replace the default values of the function
##'   \code{\link[nlme]{lmeControl}} available in the \code{\link{nlme}}
##'   package. Defaults to an empty list. The returned list is used as
##'   the control argument for the \code{lme} function.
##'
##' @param numCore number of cores used in parallel during bootstrapping
##'   computation. Default is 1.
##'
##' @return an object of class lcc. The output is a list with the
##'   following components: \item{model}{summary of the polynomial
##'   mixed-effects regression model.} \item{Summary.lcc}{fitted values
##'   for the  LCC  or LCC, LPC and LA (if \code{components=TRUE});
##'   concordance correlation coefficient (CCC) between methods for each
##'   level of \code{time} as sampled values, and the CCC between
##'   mixed-effects model predicted values and observed values from data
##'   as goodness of fit (gof)}
##'   \item{data}{the input dataset.}
##'
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}, Rafael de Andrade Moral, John Hinde
##'
##' @seealso \code{\link{summary.lcc}}, \code{\link{fitted.lcc}},
##'   \code{\link{print.lcc}}, \code{\link{lccPlot}},
##'   \code{\link{plot.lcc}}, \code{\link{coef.lcc}},
##'   \code{\link{ranef.lcc}}, \code{\link{vcov.lcc}},
##'   \code{\link{getVarCov.lcc}}, \code{\link{residuals.lcc}},
##'    \code{\link{AIC.lcc}}
##'
##' @references Lin, L. A Concordance Correlation Coefficient to
##'   Evaluate Reproducibility. \emph{Biometrics}, 45, n. 1, 255-268,
##'   1989.
##' @references Oliveira, T.P.; Hinde, J.; Zocchi S.S. Longitudinal
##'   Concordance Correlation Function Based on Variance Components: An
##'   Application in Fruit Color Analysis. \emph{Journal of
##'   Agricultural, Biological, and Environmental Statistics}, v. 23,
##'   n. 2, 233–254, 2018.
##' @references Oliveira, T.P.; Moral, R.A.; Zocchi, S.S.; Demetrio,
##'   C.G.B.; Hinde, J. lcc: an R packageto estimate the concordance
##'   correlation, Pearson correlation, and accuracy over
##'   time. \emph{PeerJ}, 8:c9850, 2020. DOI:10.7717/peerj.9850
##'
##' @keywords nlme ggplot2
##'
##' @examples
##' data(hue)
##' ## Second degree polynomial model with random intercept, slope and
##' ## quadratic term
##' fm1 <- lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'            method = "Method", time = "Time", qf = 2, qr = 2)
##' print(fm1)
##' summary(fm1)
##' summary(fm1, type="model")
##' lccPlot(fm1) +
##'  ylim(0,1) +
##'  geom_hline(yintercept = 1, linetype = "dashed") +
##'  scale_x_continuous(breaks = seq(1,max(hue$Time),2))
##'
##' @examples
##' ## Estimating longitudinal Pearson correlation and longitudinal
##' ## accuracy
##' fm2 <- update(fm1, components = TRUE)
##' summary(fm2)
##' lccPlot(fm2) +
##'  ylim(0,1) +
##'  geom_hline(yintercept = 1, linetype = "dashed") +
##'  scale_x_continuous(breaks = seq(1,max(hue$Time),2)) +
##'  theme_bw()
##'
##' @examples
##' ## A grid of points as the Time variable for prediction
##' fm3 <- update(fm2, time_lcc = list(from = min(hue$Time),
##'            to = max(hue$Time), n=40))
##' summary(fm3)
##' lccPlot(fm3) +
##'  ylim(0,1) +
##'  geom_hline(yintercept = 1, linetype = "dashed") +
##'  scale_x_continuous(breaks = seq(1,max(hue$Time),2)) +
##'  theme_bw()
##'
##' @examples
##' \dontrun{
##' ## Including an exponential variance function using time as a
##' ## covariate.
##' fm4 <- update(fm2,time_lcc = list(from = min(hue$Time),
##'               to = max(hue$Time), n=30), var.class=varExp,
##'               weights.form="time")
##' summary(fm4,  type="model")
##' fitted(fm4)
##' fitted(fm4, type = "lpc")
##' fitted(fm4, type = "la")
##' lccPlot(fm4) +
##'  geom_hline(yintercept = 1, linetype = "dashed")
##' lccPlot(fm4, type = "lpc") +
##'  geom_hline(yintercept = 1, linetype = "dashed")
##' lccPlot(fm4, type = "la") +
##'  geom_hline(yintercept = 1, linetype = "dashed")
##'
##' ## Non-parametric confidence interval with 500 bootstrap samples
##' fm5 <- update(fm1, ci = TRUE, nboot = 500)
##' summary(fm5)
##' lccPlot(fm5) +
##'  geom_hline(yintercept = 1, linetype = "dashed")
##' 
##'
##' ## Considering three methods of color evaluation
##' data(simulated_hue)
##' attach(simulated_hue)
##' fm6 <- lcc(data = simulated_hue, subject = "Fruit",
##'            resp = "Hue", method = "Method", time = "Time",
##'            qf = 2, qr = 1, components = TRUE,
##'            time_lcc = list(n=50, from=min(Time), to=max(Time)))
##' summary(fm6)
##' lccPlot(fm6, scales = "free")
##' lccPlot(fm6, type="lpc", scales = "free")
##' lccPlot(fm6, type="la", scales = "free")
##' detach(simulated_hue)
##'
##' ## Including an additional covariate in the linear predictor
##' ## (randomized block design)
##' data(simulated_hue_block)
##' attach(simulated_hue_block)
##' fm7 <- lcc(data = simulated_hue_block, subject = "Fruit",
##'            resp = "Hue", method = "Method",time = "Time",
##'            qf = 2, qr = 1, components = TRUE, covar = c("Block"),
##'            time_lcc = list(n=50, from=min(Time), to=max(Time)))
##' summary(fm7)
##' lccPlot(fm7, scales="free")
##' detach(simulated_hue_block)
##'
##' ## Testing interaction effect between time and method
##' fm8 <- update(fm1, interaction = FALSE)
##' anova(fm1, fm8)
##'
##' ## Using parallel computing with 3 cores, and a set.seed(123)
##' ## to verify model reproducibility.
##' set.seed(123)
##' fm9 <- lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'               method = "Method", time = "Time", qf = 2, qr = 2,
##'               ci=TRUE, nboot = 30, numCore = 3)
##'
##' # Repeating same model with same set seed.
##' set.seed(123)
##' fm10 <- lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'               method = "Method", time = "Time", qf = 2, qr = 2,
##'               ci=TRUE, nboot = 30, numCore = 3)
##'
##' ## Verifying if both fitted values and confidence intervals
##' ## are identical
##' identical(fm9$Summary.lcc$fitted,fm10$Summary.lcc$fitted)
##'}
##'
##' @export
lcc <- function(data, resp, subject, method, time,
                interaction = TRUE, qf = 1, qr = 0, covar = NULL,
                gs = NULL, pdmat = pdSymm, var.class = NULL,
                weights.form = NULL, time_lcc = NULL, ci = FALSE,
                percentileMet = FALSE, alpha = 0.05, nboot = 5000,
                show.warnings = FALSE, components=FALSE, REML = TRUE,
                lme.control = NULL,  numCore = 1) {
  # getting function call
  lcc_call <- match.call()
  #---------------------------------------------------------------------
  # The init function is used to check the declared arguments
  #---------------------------------------------------------------------
  Init <- init(var.class = var.class, weights.form = weights.form,
               REML = REML, qf = qf, qr = qr, pdmat = pdmat,
               dataset = data, resp = resp, subject = subject,
               method = method, time = time, gs = gs, numCore = numCore)
  pdmat <- Init$pdmat
  MethodREML <- Init$MethodREML
  var.class <- Init$var.class
  #---------------------------------------------------------------------
  # Getting relevant model information
  #---------------------------------------------------------------------
  model.info <- try(lccModel(dataset = data, resp = resp,
                             subject = subject, pdmat = pdmat,
                             method = method, time = time, qf = qf,
                             qr = qr, interaction = interaction,
                             covar = covar, gs = gs,
                             var.class = var.class,
                             weights.form = weights.form,
                             lme.control = lme.control,
                             method.init = MethodREML))
  #---------------------------------------------------------------------
  # Verifying convergence
  #---------------------------------------------------------------------
  if (model.info$wcount == "1") {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop(message(model.info$message), call. = FALSE)
  }
  #---------------------------------------------------------------------
  model <- model.info$model
  q_f <- qf
  q_r <- qr
  x <- NULL
  y <- NULL
  lme.control <- model.info$lme.control
  MethodREML <- model.info$method.init
  tk <- sort(unique(model.info$data$time))
  lev.lab <- levels(model.info$data$method)
  lev.method <- length(lev.lab)
  lev.lab <- unique(merge(rep("method",q_f),lev.lab))
  lev.lab <- transform(lev.lab,newcol = paste(x,y, sep = ""))
  fx <- fixef(model)
  pat <- list()
  #---------------------------------------------------------------------
  # Obtaining the fixed effect parameters by method to calculate the
  # systematic differences of expected values between methods.
  #---------------------------------------------------------------------
  for(i in 2:lev.method) pat[[i-1]] <- grep(lev.lab$newcol[i], names(fx))
  beta1 <- fx[-unlist(pat)]
  betas <- list()
  for (i in 2:lev.method) betas[[i-1]] <- -fx[pat[[i-1]]]
  #---------------------------------------------------------------------
  # Internal function for calculations and graphs
  #---------------------------------------------------------------------
  lcc.int_full <- lccInternal(model = model, q_f = q_f, q_r=q_r,
                              interaction = interaction, tk = tk,
                              covar = covar,
                              pdmat = pdmat, diffbeta = betas,
                              time_lcc = time_lcc, ci = ci,
                              percentileMet = percentileMet, alpha = alpha,
                              nboot = nboot, labels = lev.lab,
                              var.class = var.class, weights.form = weights.form,
                              show.warnings = show.warnings, components =
                                                               components,
                              lme.control = lme.control, method.init =
                                                           MethodREML,
                              numCore = numCore)
  lcc <- list("model" = model, "Summary.lcc" = lcc.int_full[[1]],
              "data" = data, "plot_info" = lcc.int_full[-1],
              "call" = lcc_call)
  class(lcc) <- "lcc"
  return(invisible(lcc))
}
