#' @name NanoIndent.OEFPIL
#' @title Estimation of parameters in nanoindentation
#' @description Fitting the unloading curve in nanoindentation process by power law function with parameters estimated by iterated linearization algorithm (OEFPIL). The special case of \code{\link{OEFPIL}} function customized for using in nanoindentation (see 'Details').
#' @usage NanoIndent.OEFPIL(data, alpha.start, m.start, hp.start, unload.data = FALSE, ucut = 0.98,
#'                  lcut = 0.4, CM, uh = 0.5, uF = 0.001, max.iter = 100,
#'                  see.iter.val = FALSE, save.file.name, th = .Machine$double.eps ^ (2 / 3),
#'                  signif.level = 0.05, useNLS = TRUE)
#'
#' @param data an object of type \code{data.frame} with 2 named columns or \code{list} with 2 elements.
#' @param alpha.start a starting value of the fitting constant alpha.
#' @param m.start a starting value of the exponent m.
#' @param hp.start a starting value of the permanent indentation depth hp.
#' @param unload.data a logical value (default \code{FALSE}) indicating the structure of \code{data}. If \code{TRUE}, an input data contains only unloading part of the curve. If \code{FALSE}, an input data contains complete loading, hold and unloading parts of an indentation process.
#' @param ucut a numerical value, indicating the upper bound of cut off.
#' @param lcut a numerical value, indicating the lower bound of cut off.
#' @param CM a covariance matrix of the input \code{data}. See 'Details' for more information.
#' @param uh standard deviation of depth.
#' @param uF standard deviation of load.
#' @param max.iter maximum number of iterations.
#' @param see.iter.val logical. If \code{TRUE}, all the partial results of the OEFPIL algorithm are displayed and saved. The default value is \code{FALSE}.
#' @param save.file.name a name of the file for saving results. If missing, no output file is saved.
#' @param th a numerical value, indicating threshold necessary for the iteration stoppage.
#' @param signif.level a significance level for the confidence interval.
#' @param useNLS logical. If \code{TRUE} (the default value), function will set up starting parameters calculated by \code{\link{nlsLM}} function (nonlinear least square estimation).
#'
#' @details In this special case of the \code{OEFPIL} function, the dependence of parameters is fixed in
#' the form: \eqn{F = \alpha * (h - h_p)^m}, where \eqn{F} is load and \eqn{h} depth measured within a nanoindentation process.
#' It is possible to set own starting values of the parameters, in the other case these values are calculated by the algorithm and printing into the console.
#'
#' A selection of the part of the unloading curve fitted by a power law function is provided with \code{lcut} and \code{ucut} arguments. The default values 0.4 and 0.98 corresponds to the range 40-98 \% \eqn{F_{max}} (maximum force) as recommended in ISO 14577 standard.
#'
#' The \code{CM} has to be a \code{2n} covariance matrix (where \code{n} is length of \code{data}) of following structure: first \code{n} elements of the diagonal correspond to the variance of depth and other to the variance of load.
#' If argument \code{CM} is missing, the input covariance matrix is set to a diagonal matrix with variance of depth and load (calculated from \code{uh} and \code{uF}) on the diagonal.
#' If standard deviations are missing too, the default values (\code{uh}=0.5, \code{uF}=0.001) are used.
#'
#' The estimations and confidence intervals are computed under normality assumption (see \code{\link{OEFPIL}} 'Details').
#'
#' @return Returns an object of class \code{"OEFPIL"}. It is a list containing at least the following components
#'
#' \item{name_Est}{estimations of model parameters.}
#' \item{name_upgraded.start.val}{modified starting values of estimating parameters (result from \code{\link{nlsLM}} function).}
#' \item{cov.m_Est}{estimated covariance matrix of parameters.}
#' \item{it_num}{number of iterations.}
#' \item{CI_parameters}{a list of confidence intervals for estimated parameters (a significance level is based on \code{signif.level} argument).}
#' \item{logs}{warnings or messages of events, which happen during the run of the algorithm.}
#' \item{...}{for other components specification see \code{\link{OEFPIL}}.}
#'
#' \item{contents}{a list of outputs as original values of data and other characteristics, which are usable in plotting or other operations with model results.}
#'
#' If \code{useNLS} argument is set to \code{FALSE}, the \code{name_upgraded.start.val} are the same as \code{start.values} (no \code{nlsLM} procedure for starting value fitting is performed).
#'
#' @references ISO/IEC: \emph{14577-1:2015 Metallic materials – Instrumented indentation test for hardness and materials parameters – Part 1: Test method} (ISO/IEC, Internation Organisation for Standardisation, 2015).
#'
#' Anna Charvátová Campbell, Petr Grolich, Radek šlesinger. (2019). \emph{Niget: Nanoindentation general evaluation tool}. SoftwareX, \strong{Vol. 9}: 248–254.
#' \doi{10.1016/j.softx.2019.03.001}.
#'
#' Köning, R., Wimmer, G. and Witkovský, V. \emph{Ellipse fitting by nonlinear constraints to demodulate quadrature homodyne interferometer signals and to determine the statistical uncertainty of the interferometric phase}. Measurement Science and Technology (2014).
#'
#' @seealso \code{\link{OEFPIL}}
#'
#' @examples
#' ##Use of NanoIndent function for data file "silicaBerk.RData" (a part of the OEFPIL package)
#' signif.level = 0.05
#' output.form.NI <- NanoIndent.OEFPIL(silicaBerk, unload.data = TRUE, ucut = 0.98, lcut = 0.2,
#' uh = 0.5, uF = 0.001, signif.level = signif.level)
#'
#' ##The output is an object of class 'OEFPIL', supplementary functions for this class are available
#' ##Use of summary function
#' summary(output.form.NI)
#'
#' ##Plot of estimated unloading curve
#' plot(output.form.NI, signif.level = signif.level)
#'
#' @export


################################################################################


NanoIndent.OEFPIL <- function(data, alpha.start, m.start, hp.start, unload.data = FALSE,
                       ucut = 0.98, lcut = 0.4, CM, uh = 0.5, uF = 0.001,
                       max.iter = 100, see.iter.val = FALSE, save.file.name,
                       th = .Machine$double.eps ^ (2 / 3), signif.level = 0.05,
                       useNLS = TRUE) {
  ##  Function for calculation of nanoindentation. It uses OEPFIL, which she prepared
  ## data (cut them off) and formula for. Funtional dependence of parameters is fixed in the form:
  ## f = alpha * (h - hp) ^ m, where f is load and h depth (on the unloaded part of curve).
  ## User has an option to enter his own starting values of parameters. If he does not make it,
  ## function will warn him, that the starting values will be set as stated in the algorithm.
  ## Function NanoIndent has some extra features compared to OEFPIL. It can output some extra
  ## warnings. For example if "hp >= min(h)" (does not make sence from phicical prospective).
  ## unload.data . . . FALSE, in case we have both complete load and unload curve, in this case the function will make cut off by its own
  ##                   TRUE, if we know, that we only have unloaded curve;
  ## ucut      .  .  . it's the upper bound of cut off F_max
  ## lcut      .  .  . it's the lower bound of cut off F_max, i.e. if ucut = 0.98,
  ##                   lcut = 0.4 we consider 40 - 98 % from F_max (standard/norm recommendation)

  logs <- NA

  ICL <- FindIndentCurve(data, lcut = lcut, ucut = ucut, unload.data = unload.data)
  ## If we do not use "clean" data, we have to use cut off function
  ## If we have unload function, some cut off must be used

  data.cut <- data.frame(ICL$f_orig, ICL$h_orig)
  colnames(data.cut) <- c("f", "h")
  ## Creation of data file, which has to have named columns - important for future
  ##  work with OEFPIL variable.

  Fmax <- ICL$Fmax
  hmax <- ICL$hmax
  hmin <- ICL$hmin

  # If any of starting values are missing, we will use our
  if (missing(alpha.start) || missing(m.start) || missing(hp.start)) {

    index.miss <- which(c(missing(alpha.start), missing(m.start), missing(hp.start)))
    par.miss <- paste(c("alpha", "m", "hp")[index.miss], collapse = ", ")


    # starting values for alpha, hp and m by proposition of algorithm
    # (choice by proposition of algorithm and correct from the physical prosepctive)
    m.start <- 1.5
    hp.start <- 0.9 * hmin
    alpha.start <- Fmax / ((hmax - hp.start) ^ m.start)
    start.val <- list(alpha = alpha.start, m = m.start, hp = hp.start)

    logg <- paste("Starting values for these parameters were not given: ",
                  par.miss, ".", "\n", "These starting values were used instead:\n",
                  "alpha.start = ", alpha.start, "\n","m.start     = ", m.start,
                  "\n", "hp.start    = ", hp.start, "\n", sep = "")
    message(logg)
    logs <- paste(na.omit(logs), logg, sep = "// ")

  } else {
    start.val <- list(alpha = alpha.start, m = m.start, hp = hp.start)
  }

  form <- f ~ alpha * (h - hp) ^ m

  if (missing(CM)) {
    ## If the user does not define covariance matrix, we will use this one

    vec.uni <- rep(1, length(data.cut$h))
    CM <- CovMatrix(uh, uF, vec.uni, vec.uni)
    ## covariance matrix calculated by the proposition of algorithm
    ## Does not change in the provess of estimation.
  }

  output.form <- OEFPIL(data.cut, form, start.val, CM = CM, max.iter = max.iter,
                        see.iter.val = see.iter.val, save.file.name = save.file.name,
                        th = th, signif.level = signif.level, useNLS = useNLS)

  if (IsListOK(output.form$hp_Est) && IsListOK(output.form$h_Est)) {

    if (output.form$hp_Est >= min(output.form$h_Est)) {
      logg <- "Warning: After finishing the iteration, the value of hp is greater or equal than min of upgraded h values. \n"
      message(logg)
      output.form$logs <- paste(na.omit(c(logs, output.form$logs)), logg, sep = "//")
    }

  }

  return(output.form)
}

################################################################################

FindIndentCurve <- function(data, lcut = 0.4, ucut = 0.98, unload.data = FALSE) {
  ## Function calculate the origin and the end of unloaded curve and cut off from this curve.
  ## First option is, if we have both loaded and unloaded data:
  ## (1) We consider, that the origin is the point, where the maximum strength is measured.
  ## If there are is more than one point, we consider the last of these points.
  ## Measurement containing negative values of force and depth are cutted off. The last step
  ## is to cut off the curve by the percentage ranfe from F_max, entered by lcut, ucut
  ## (2) We have got data just from unloaded curve (unload.data = TRUE). We cut off
  ## the negative values and afterwards modification by ucut and lcut.
  ##   The function ignores NA values in the data.
  ##
  ## data        . . . entry data file; 1. column is h, 2. col. is F
  ## unload.data . . . FALSE, in case we have both complete load and unload curve, in this case the function will make cut off by its own
  ##                   TRUE, if we know, that we only have unloaded curve;
  ## ucut      .  .  . it's the upper bound of cut off F_max
  ## lcut      .  .  . it's the lower (?? nizsi ??) bound of cut off F_max, i.e. if ucut = 0.98,
  ##                   lcut = 0.4 we consider 40 - 98 % from F_max (standard/norm recommendation)

  if (unload.data == FALSE) {

    # cutting off the unload curve
    pos_Fmax <- dim(data)[1] - which.max(rev(data[,2])) + 1
    Fmax <- data[pos_Fmax,2]
    hmax <- data[pos_Fmax,1]
    data <- data[pos_Fmax:dim(data)[1],]

    # cutting off the negative values
    if (any(data < 0)) {
      mz <- min(which(data[,1] < 0), which(data[,2] < 0), na.rm = TRUE)
      data <- data[1:(mz - 1),]
    }

  } else {

    # cutting off the negative values
    if (any(data < 0)) {
      mz <- min(which(data[,1] < 0), which(data[,2] < 0), na.rm = TRUE)
      data <- data[1:(mz - 1),]
    }

    pos_Fmax <- 1
    Fmax <- data[pos_Fmax,2]
    hmax <- data[pos_Fmax,1]

  }

  hmin <- min(data[,1], na.rm = TRUE)

  # cutting off the right part of the unload  curve
  UB <- Fmax * ucut
  LB <- Fmax * lcut

  index <- which( data[,2] <= UB & data[,2] >= LB )

  f_orig <- data[index,2]
  h_orig <- data[index,1]

  return(list(f_orig = f_orig, h_orig = h_orig, Fmax = Fmax, hmax = hmax, hmin = hmin))
}

################################################################################

CovMatrix <- function(uh, uF, hvec, Fvec) {
  # uh, uF - uncertainties of depth and load
  # Fvec - vector of measured values of loading
  ## output: covariance matrix

  N <- length(Fvec)
  M0 <- matrix(0, nrow = N, ncol = N)
  M1 <- uh^2 * diag(hvec)
  M2 <- uF^2 * diag(Fvec)^2
  covM <- cbind(rbind(M1, M0), rbind(M0, M2))
  return(covM)
}



