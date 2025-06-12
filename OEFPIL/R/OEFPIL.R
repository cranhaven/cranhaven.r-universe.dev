#' @name OEFPIL
#' @title Optimal Estimation of Parameters by Iterated Linearization
#' @description Function for computing optimal estimate of parameters of a nonlinear function by iterated linearization (using Taylor expansion). The model considers measurements errors in both (dependent and independent) variables.
#' @usage OEFPIL(data, form, start.val, CM, max.iter = 100, see.iter.val = FALSE,
#'        save.file.name, th, signif.level, useNLS = TRUE)
#'
#' @param data a data file can be any object of type \code{data.frame} with 2 named columns or \code{list} with 2 elements.
#' @param form an object of class \code{\link[stats]{formula}} (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.
#' @param start.val a named list of starting values of estimating parameters.
#' @param CM a covariance matrix of \code{data} (See 'Details' for the information about required structure.).
#' @param max.iter maximum number of iterations.
#' @param see.iter.val logical. If \code{TRUE}, all the partial results of the algorithm are displayed and saved. The default value is \code{FALSE}.
#' @param save.file.name a name of the file for saving results. If missing, no output file is saved.
#' @param th a numerical value, indicating threshold necessary for the iteration stoppage. The default value is \code{.Machine$double.eps ^ (2 / 3)}.
#' @param signif.level a significance level for the confidence interval. If missing, the default value 0.05 is used.
#' @param useNLS logical. If \code{TRUE} (the default value), function will set up starting parameters calculated by \code{\link{nlsLM}} function (nonlinear least square estimation).
#'
#' @details Models for OEPFIL function are specified symbolically. A typical model has the form \code{y ~ f(x, a_1,...,a_n)}, where
#'
#'  \itemize{\item \code{y} is the (numerical) response vector,
#'            \item \code{x} is the predictor,
#'            \item terms \code{a_1,...,a_n} are parameters of specified model.}
#'  Function \code{f} is known nonlinear function with continuous second partial derivatives with respect to \code{x} and parameters \code{a_1,...a_n} (for more details see (Kubáček, 2000).
#'
#'  All calculations are performed assuming normality of a response vector and measurements errors.
#'
#'  In the \code{data} entry of type \code{data.frame}, both columns must be named as variables in formula. The same holds for elements of \code{list}.
#'
#' A choice of \code{start.val} is important for the convergence of the algorithm. If the \code{OEFPIL} algorithm does not converge, starting values modified by \code{nlsLM} function (\code{useNLS = TRUE}) are recommended (see Example 3).
#'
#' The \code{CM} has to be a \code{2n} covariance matrix (where \code{n} is length of \code{data}) of following structure: first \code{n} elements of the diagonal correspond to the variance of independent variable (\code{x}) and other to the variance of dependent variable (\code{y}).
#' If argument \code{CM} is missing, the input covariance matrix is set to a diagonal variance matrix with sample variance on the main diagonal.
#'
#' @return Returns an object of class \code{"OEFPIL"}. It is a list containing the following components
#'
#' \item{name_Est}{estimations of model parameters.}
#' \item{name_upgraded.start.val}{modified starting values of estimating parameters (result from \code{\link{nlsLM}} function).}
#' \item{cov.m_Est}{estimated covariance matrix of parameters.}
#' \item{cov.m_nlsLM}{a covariance matrix of starting values of parameters from \code{\link{nlsLM}} function (if \code{useNLS} was set to \code{TRUE}).}
#' \item{it_num}{number of iterations.}
#' \item{name_previous.step}{the parameter values from the previous iterative step.}
#' \item{CI_parameters}{a list of confidence intervals for estimated parameters (a significance level is based on \code{signif.level} argument).}
#' \item{logs}{warnings or messages of events, which happen during the run of the algorithm.}
#'
#' \item{contents}{a list of outputs as original values of data and other characteristics, which are usable in plotting or other operations with model results.}
#'
#' If \code{useNLS} argument is set to \code{FALSE}, the \code{name_upgraded.start.val} are the same as \code{start.values} (no \code{nlsLM} procedure for starting value fitting is performed).
#'
#'
#' @note The symbol \code{pi} is reserved for the Ludolf's constant. So naming one of the model´s parameters by this symbol results in constant entry of the model.
#'
#' @references Kubáček, L. and Kubáčková, L. (2000) \emph{Statistika a metrologie}. Univerzita Palackého v Olomouci.
#'
#'    Köning, R., Wimmer, G. and Witkovský, V. (2014) \emph{Ellipse fitting by nonlinear constraints to demodulate quadrature homodyne interferometer signals and to determine the statistical uncertainty of the interferometric phase}.
#'     Measurement Science and Technology.
#'
#' @seealso \code{\link{NanoIndent.OEFPIL}} and function \code{\link[minpack.lm]{nlsLM}} from \code{minpack.lm} package for nonlinear least square algorithms.
#'
#' @examples
#' ##Example 1 - Use of OEFPIL function for steam data from MASS library
#' library(MASS)
#' steamdata <- steam
#' colnames(steamdata) <- c("x","y")
#' k <- nrow(steamdata)
#' CM <- diag(rep(5,2*k))
#'
#' st1 <- OEFPIL(steamdata, y ~ b1 * 10 ^ (b2 * x/ (b3 + x)),
#'  list(b1 = 5, b2 = 8, b3 = 200), CM, useNLS = FALSE)
#'
#' ## Displaying results using summary function
#' summary(st1)
#'
#' ## Plot of estimated function
#' plot(st1, signif.level = 0.05)
#'
#' @examples
#' ## Example 2 - Use of OEFPIL for nanoindentation data "silica2098.RData"
#' ## (which is part of the OEFPIL package)
#' ## Preparing arguments for OEFPIL function
#' max.iter = 100
#' see.iter.val = FALSE
#' signif.level = 0.05
#' useNLS = TRUE
#'
#' ## Creating a list with starting values for parameters
#' start.val <- list(alpha=0.1, m=1.5, hp=0.9)
#' names(start.val) <- c("alpha", "m", "hp")
#'
#' ## Imputed formula
#' form <- Load ~ alpha * (Depth - hp) ^ m
#' k <- length(silica2098[,1])
#' CM <- diag(c(rep(0.5^2,k),rep(0.001^2,k)))
#'
#' ## Use of OEFPIL function with defined arguments
#' output.form <- OEFPIL(silica2098, form, start.val, CM = CM, max.iter = max.iter,
#'  see.iter.val = see.iter.val, signif.level = signif.level, useNLS = useNLS)
#'
#' ## Displaying results with summary (the result is the same as in NanoIndent.OEFPIL function)
#' summary(output.form)
#'
#' @import MASS
#' @import minpack.lm
#' @import Deriv
#' @import matrixcalc
#' @import plyr
#' @import ggplot2
#' @import stats
#'
#' @export


OEFPIL <- function(data, form, start.val, CM,  max.iter = 100, see.iter.val = FALSE,
                   save.file.name, th = .Machine$double.eps ^ (2 / 3),
                   signif.level = 0.05, useNLS = TRUE) {
  ##   Optimum Estimate of Function Parameters by Iterated Linearization.
  ## Users will enter initial formula and list of starting values of all parameters from
  ## start.val. It's important to satisfy following conditions instructions:
  ## (1) Initial formula has left and right hand side, where on the left side we specify
  ##     dependant variable, i.e. : y ~ x + a + 3. Notation: ~ x + a + 1 is not possible.
  ## (2) In the entry data table "data" are all columns named. This names corresponds
  ##     to names in entered formula.
  ## (3) On the right hand side of the formula, there must be entered valid functional notation
  ##     - it´s not typical R-style formula, since we does model response with function f().
  ##     Correct notation should be y ~ f(x, a_1, ..., a_n), where y stands for dependant and
  ##     x for independant variable, a_1, ..., a_n are parameters and f() is differentiable
  ##     function (with respect to x and a_1,...,a_n). So our correct notation
  ##     is y ~ "funkcny predpis for y".
  ##     The symbol \code{pi} is reserved for Ludolf's constant. So naming one of the
  ##     model´s parameters by this symbol results in constant entry of the model.


  ######################################################################################
  ## Further parameters:
  ## data           . . . entry data file, where the columns must be named
  ## CM             . . . covariance matrix - does not change in iteration process
  ## max.iter       . . . maximum number of iterations (default = 100)
  ## see.iter.val   . . . logical value (default \code{TRUE}) indicating if we want to display and save partial results of algorithm.
  ## save.file.name . . . name of file, where user want to save iterations of algorithm in, if none
  ##                      is given, nothing is going to save; recommended format is .Rdata
  ## th             . . . threshold necessary for iteration's stoppage
  ## signif.level   . . . significance level for confidence interval
  ## useNLS         . . . TRUE, for calculation of initial estimates with the function nlsLM()
  ##                      FALSE, for calculation of initial estimates from the starting values parameters


  logs <- NA

  if (ncol(data) != 2) {
    stop("The data has to contain two named columns - one for a dependent variable and one for an independent variable!")
  }
  ## stopifnot( ncol(data) == 2 ) # Another variant of program stoppage

  if (length(colnames(data)) != 2 | sum(is.na(colnames(data))) != 0) {
    stop("Both columns of the data have to be named!")
  }
  ## Control, if both columns of data are named.

  if (is.formula(form) == FALSE) {
    stop("There has to be a formula as an input value.")
  }
  ## Control of formula correction

  if (th < .Machine$double.eps ^ (2 / 3)) {
    th <- .Machine$double.eps ^ (2 / 3)
    logg <- paste("Threshold has to be greater than ", signif(.Machine$double.eps^(2 / 3), 4),
                  ". It was set to this value instead. ", "\n", sep = "")
    message(logg) ## display error massage in console
    logs <- paste(na.omit(logs), logg, sep = "//")
  }
  ## Control of threshold

  if (!(is.logical(see.iter.val))) {
    stop("Parameter 'see.iter.val' has to be logical.")
  }
  if (!(is.logical(useNLS))) {
    stop("Parameter 'useNLS' has to be logical.")
  }
  ## Type control of parameters being logical


  input.form.string <- SafeDeparse(form)
  ## Transformation of formula to string

  main.func.call <- str2lang(strsplit(input.form.string, "~")[[1]][2])
  ## Extraction of functional prescription and creation of "call" object

  if (strsplit(input.form.string, "~")[[1]][1] == "") {
    stop("There has to be the name of a dependent variable on the left side of the formula!")
  }
  ## Formula must have prescribed left side and the dependant variable must be named
  ## I.e. y ~ ... is OK, but formula cannot start with ~ ...

  if ("pi" %in% all.vars(form)) {
    orig.vars <- all.vars(form)[- which(all.vars(form) == "pi")]
  } else {
    orig.vars <- all.vars(form)
  }
  ## If the function contain "pi" sign, we make sure that the function will take it an account
  ## as a constatnt and not one of fucntional parameters.


  l <- length(orig.vars) - 2
  ## l is a number of parameters; the number of derivativs which we want to calculate is l + 1

  dep.var.name <- orig.vars[1]

  if (!(dep.var.name %in% colnames(data))) {
    logg <- paste("The dependent variable  '", dep.var.name,
                  "' specified on the left side of a formula is not found in the data (columns of the data have to be named).",
                  sep="")
    stop(logg)
  }
  ## Control of presence of name of dependant variable

  col.dep.var <- which(colnames(data) == dep.var.name)
  col.idp.var <- which(colnames(data) != dep.var.name)

  idp.var.name <- colnames(data)[col.idp.var]

  y <- data[,col.dep.var]

  if (!(idp.var.name %in% orig.vars)) {
    logg <- paste("The variable '", idp.var.name,
                  "' is not found on the right side of a given formula.",
                  sep = "")
    stop(logg)
  }
  ## Control the presence of name of independant variable on the right hand side of formula.

  x <- data[,col.idp.var]

  if (missing(CM)) {

    CM <- diag(c(rep(var(x), length(x)), rep(var(y), length(y))))
    ## If the user does not define covariance matrix, we will use this one

    logg <- paste("A covariance matrix of the data was not given. Therefore a diagonal", "\n",
                  "covariance matrix with sample variance on the main diagonal is used.", "\n", sep = "")
    message(logg)
    logs <- paste(na.omit(logs), logg, sep = "//")

  } else if (is.matrix(CM)) {

    if (all(dim(CM) == rep(dim(data)[1] * 2, 2))) {

      if (!(is.positive.semi.definite(CM))) {
        logg <- paste("'CM' has to be a covariance matrix.", "\n",
                      "Given 'CM' has to be positive semidefinite.", sep="")
        stop(logg)
      }

    } else {
      logg <- paste("'CM' has to be a covariance matrix.", "\n",
                    "Given 'CM' has to be a square matrix.", sep="")
      stop(logg)
    }

  } else {
    logg <- paste("'CM' has to be a covariance matrix.", "\n",
                  "Given'CM' is not a matrix.", sep="")
    stop(logg)
  }
  ## Control of input of covariance matrix

  if (IsListOK(data) == FALSE) {

    n <- dim(data)[1]
    odr <- sort(unique(c(which(is.infinite(data[, 1])),
                         which(is.na(data[, 1])),
                         which(is.infinite(data[, 2])),
                         which(is.na(data[, 2])))))

    data <- data[ - odr,]
    CM <- CM[ - c(odr, odr + n), - c(odr, odr + n)]

    logg <- paste("The data rows ", paste(odr, collapse = ", "),
                  " contained NaN, NA, Inf or -Inf values.", "\n",
                  "These rows were removed and given covariance matrix was upgraded accordingly.", "\n", sep = "")
    message(logg) ## display error massage in console
    logs <- paste(na.omit(logs), logg, sep = "//")
  }

  if (which(orig.vars == idp.var.name) != 2) {
    idp.var.order <- which(orig.vars == idp.var.name)
    vars <- c(orig.vars[1], idp.var.name, orig.vars[-c(1, idp.var.order)])
  } else {
    vars <- orig.vars
  }
  ## We are changing the order of independant variable and parameters
  ## The first spot will take dependant variable, then independent and finally parameters.

  if (!(is.list(start.val))) {
    start.val <- as.list(start.val)
  }
  ## Transformation of starting values to type list, if needed.

  if (!(all(vars[3:length(vars)] %in% names(start.val)))) {
    jm.index <- which(!(vars[3:length(vars)] %in% names(start.val)))
    logg <- paste("There are some parameters on the right side of the formula, but there are no starting values for them: ",
                  paste(vars[2 + jm.index], collapse = ", "), ".", sep = "")
    stop(logg)
  }
  ## Test the presence of all starting values of parameters stated in formula

  if (!(all(names(start.val) %in% vars[3:length(vars)]))) {
    start.val[which(!(names(start.val) %in% vars[3:length(vars)]))] <- NULL
  }
  ## Removing starting values of parameters which are not present in formula.
  ## I.e. removing the starting values, which are extra (we have start value but
  ## no parameter in formula)

  pom.factor <- factor(vars[3:length(vars)], levels = names(start.val),
                       ordered = TRUE)
  pom.factor <- sort(pom.factor)
  vars[3:length(vars)] <- as.vector(pom.factor)
  ## Ordering of parameter's name, in the order of users's list of starting values

  args <-  vector(mode = "list", length = (l + 1))
  names(args) <- vars[2:length(vars)]
  ## Creation of blank list; arguments are entry values of function

  # args[vars[2]] <- start.val[vars[2]]
  ## Eventual assigning of starting values

  MainFunction <- function(args, body, env = parent.frame()) {
    args <- as.pairlist(args)
    eval(call("function", args, body), env)
  }

  LOF <- list()
  LOF[[1]] <- MainFunction(args, main.func.call)

  LOF[2:(l+2)] <- sapply(1:(l+1), function(i) {
    Deriv(MainFunction(args, main.func.call), vars[i+1])
  })
  ## Derivative of entered function with respect to independent variable and parameters

  # Choice of modified starting values with usage of NLS
  if (useNLS == TRUE) {

    lst.start.val <- tryCatch( expr = {

      nlc <- nls.control(maxiter = 1000)

      formula.input <- as.formula(input.form.string)

      nonlin_model <- nlsLM(formula.input, data = data, control = nlc,
                            start = start.val)
      ## If we set up "form" instead of "formula.input", it will fail

      coef.vec <- coefficients(nonlin_model)
      logs <- NA # warning protocol is set to be empty

      L <- list()
      L[names(coef.vec)] <- as.list(coef.vec)
      L[["CM0"]] <- vcov(nonlin_model)
      L[["logs"]] <- logs
      ## the output from this part (i.e. from nlsLM) is list containing all parameters

      L
      ## thanks to the final row is the output from this part list containing all the parameters
    }, error = function(e) {

      err_R <- NULL
      err_R <- conditionMessage(e) # save the error messages from R
      logg <- paste("Cannot use nls(), different start values were used",
                    "\n", "Original nls error: ", err_R, "\n", sep="")
      message(logg) ## display error massage in console
      logs <- paste(na.omit(logs), logg, sep = "//")

      L <- start.val
      CM0 <- NA
      L[["CM0"]] <- CM0
      L[["logs"]] <- logs

      return(L)
    })

    ## ErrM <- paste("Problem with nls(). Original error: ", err_R, sep="")
  } else {

    lst.start.val <- start.val
    CM0 <- NA
    logs <- NA
    lst.start.val[["CM0"]] <- CM0
    lst.start.val[["logs"]] <- logs

  }

  CM0 <- lst.start.val$CM0
  logs <- lst.start.val$logs

  # Function calculate iterations
  lst.iteration <- OEFPILIter(y0 = y, x0 = x, L = lst.start.val, CM = CM, max.iter = max.iter,
                              see.iter.val = see.iter.val, save.file.name = save.file.name,
                              th = th, LOF = LOF, logs = logs)

  contents <- list(input.form.string = input.form.string, LOF = LOF, x = x, y = y,
                   CM = CM, dep.var.name = dep.var.name, idp.var.name = idp.var.name,
                   names.of.parameters = names(lst.iteration$lst.parameters),
                   signif.level = signif.level)
  ## list contains rest of parameters usable in following functions

  names(contents)[3] <- idp.var.name
  names(contents)[4] <- dep.var.name

  lst.output <- list()
  length(lst.output) <- 3*l + 8
  lst.output[1:l] <- lst.iteration$lst.parameters
  lst.output[(l+1):(2*l)] <- lst.start.val[1:l]
  lst.output[[2*l+1]] <- lst.iteration$cov_m
  lst.output[[(2*l+2)]] <- CM0
  lst.output[[(2*l+3)]] <- lst.iteration$yEst
  lst.output[[(2*l+4)]] <- lst.iteration$xEst
  lst.output[[(2*l+5)]] <- lst.iteration$it_num
  lst.output[(2*l+6):(3*l+5)] <- lst.iteration$lst.parameters_previous.step
  lst.output[[(3*l+6)]] <- NA
  lst.output[[(3*l+7)]] <- contents
  lst.output[[(3*l+8)]] <- lst.iteration$logs

  names(lst.output) <- c(paste(names(lst.iteration$lst.parameters),"_Est",sep=""),
                         paste(names(lst.start.val)[1:l],"_upgraded.start.val",sep=""), "cov.m_Est", "cov.m_nlsLM",
                         paste(vars[1], "_Est", sep=""), paste(vars[2], "_Est", sep=""), "it_num",
                         paste(names(lst.iteration$lst.parameters_previous.step), "_previous.step", sep=""),
                         "CI_parameters", "contents", "logs")

  class(lst.output) <- c("OEFPIL", class(lst.output))

  lst.output[[(3*l+6)]] <- confInt.OEFPIL(lst.output, signif.level = signif.level)

  return(lst.output)
}
################################################################################

OEFPILIter <- function(y0, x0, L, CM, max.iter = 100, see.iter.val = FALSE,
                       save.file.name, th, LOF,  logs = NA) {
  ##   Iteration for the calculation of parameter estimation in generalised version of algorythm
  ## y0             . . . entry vector y
  ## x0             . . . entry vector x
  ## L              . . . list of starting values of parameters
  ## CM             . . . covariance matrix -  this part does not change in the process of
  ##                      algorithm
  ## max.iter       . . . maximum number of iterations of the algorithm (default = 100)
  ## see.iter.val   . . . TRUE, if we want to continuously save and output results from the process of algorithm
  ## save.file.name . . . name of file, where user want to save iterations of algorithm in, if none
  ##                      is given, nothing is going to save; recommended format is .Rdata
  ## th             . . . threshold necessary for iteration's stoppage
  ## LOF            . . . entry list of user's defined functions
  ## logs           . . . string containing the messages and warnings about the whole
  ##                      process of function

  l <- length(L) - 2
  ## number of parameters

  L0 <- list()
  length(L0) <- l
  L0 <- L[1:l]
  ## original values

  L1 <- list()
  length(L1) <- l
  ## new values

  # parameter's value will not change in the iterative process; "L1" sign is current
  # "L0" sign is the value from the previous step
  # We set them to be same at the beginning
  L1 <- L0

  Q22 <- NA

  # x1 and y1 are values, which will be updated in the process
  x1 <- x0
  y1 <- y0

  N <- length(y0)

  # set of number of parameter for number of iterations; final number of iterations
  # so it will be considered with zero step
  it_num <- 0

  while((ConditionForIteration(L0, L1, th) || it_num == 0) && it_num < max.iter) {

    fval <-  sapply(x1, function(val, LP){do.call(LOF[[1]], args=c(val, LP))}, L1)
    ## It contains results of function of x1 defined by user, for parameters contained in L1


    b_vec <- y1 - fval

    B11 <- - diag(sapply(x1, function(val, LP){do.call(LOF[[2]], args=c(val, LP))}, L1))
    ## Quantification of derivatives of user's defined function with respect to x1 for
    ## value of parameters contained in L1 and set as a diagonal entries in matrix

    B1 <- cbind(B11, diag(N))

    if ((sum(is.infinite(B1)) != 0) || (sum(is.na(B1)) != 0)) {
      logg <- paste("There is NaN, NA, Inf or -Inf value in matrix B1. Iteration aborted!!!", "\n",
                    "The following results (if any) are only partial.", "\n", sep="")
      message(logg)
      logs <- paste(na.omit(logs), logg, sep = "//")
      break
    }
    ## while cyclus will be stopped in case of NA, NaN, Inf or -Inf value in B1

    B2 <- CreateB2(LOF, L1, x1, l)
    ## creation of matric B2

    if ((sum(is.infinite(B2)) != 0) || (sum(is.na(B2)) != 0)) {
      logg <- paste("There is NaN, NA, Inf or -Inf value in matrix B2. Iteration aborted!!!", "\n",
                    "The following results (if any) are only partial.", "\n", sep="")
      message(logg)
      logs <- paste(na.omit(logs), logg, sep = "//")
      break
    }
    ## while cyclus will be stopped in case of NA, NaN, Inf or -Inf value in B2

    M11 <- B1 %*% CM %*% t(B1) # CM is covariance matrix

    lst.chol <- tryCatch(expr = {
      Lmat <- t(chol(M11)) # Cholesky decomposition of matrix M11

      logg <- NA

      L <- list(Lmat = Lmat, logg = logg)
      ## output from this part is the final row (i.e. L)
    }, error = function(e) {

      err_R <- NULL
      err_R <- conditionMessage(e) # ulozeni chybove hlasky z R
      logg <- paste("Problems with computing Cholesky decomposition (chol() function). Iteration aborted!!!",
                    "\n", "Original chol error: ", err_R, "\n", sep="")
      message(logg) ## display error message in the console
      Lmat <- NA

      L <- list(Lmat = Lmat, logg = logg)
      return(L)
    })
    ## safe startup of function chol. We provide, so that the function will not break,
    ## just display warning.

    if (grepl("Iteration aborted!!!", lst.chol$logg)) {
      logs <- paste(na.omit(logs), lst.chol$logg, sep = "//")
      break
    }
    ## Interruption of cycle, in case of error message of the chol() function

    ## Creating Q matrix using Cholesky SVD decomposition and block inverse matrices
    Lmat <- lst.chol$Lmat
    E <- forwardsolve(Lmat, B2) # Solves a triangular system of linear equations.
    Esing <- svd(E)
    Ue <- Esing$u
    Ve <- Esing$v
    Se <- diag(Esing$d)
    Seinv <- diag(1 / (Esing$d))
    Fmat <- Ve %*% Seinv
    G <- backsolve(t(Lmat), Ue)
    Q21 <- Fmat %*% t(G)
    Q11 <- chol2inv(Lmat) - G %*% t(G)
    Q22 <- - Fmat %*% t(Fmat)
    colnames(Q22) <- names(L1)
    rownames(Q22) <- names(L1)

    output01 <- (diag(2*N) - CM %*% t(B1) %*% Q11 %*% B1) %*% c(x0 - x1, y0 - y1) - CM %*% t(B1) %*% Q11 %*% b_vec
    xdiff_hat <- output01[1:N]
    ydiff_hat <- output01[(N+1):(2*N)]

    output02 <- -Q21 %*% B1 %*% c(x0 - x1, y0 - y1) - Q21 %*% b_vec
    ## vector containing increase of all l parameters in current step

    L0 <- L1

    x1 <- x1 + xdiff_hat
    y1 <- y1 + ydiff_hat
    ## update (improve) values of vectors x1,y1

    L1 <- as.list(unlist(L0) + output02)
    names(L1) <- names(L0)
    ## update (improvement) of values of the rest of the parametra; we have to save either
    ## original and updated values due to condition from the beginning of cycle

    if (IsListOK(L1) == FALSE) {
      logg <- paste("During the iteration, the estimated parameter equals NaN, NA, Inf or -Inf.", "\n",
                    "Iteration aborted!!! The following results (if any) are only partial.", "\n", sep="")
      message(logg)
      logs <- paste(na.omit(logs), logg, sep = "//")
      break
    }
    ## control of correctness of updated (improved) values (L1)

    # The final number of performed iterations.
    it_num <- it_num + 1

    ## Condition which save and output all values of estimates in process of iteration
    ## if it's required by user.
    if (see.iter.val == TRUE) {

      print(paste("it_num ", it_num, ": ###############################",
                  sep = ""))
      print(data.frame(L1, row.names = ""))
      print(- Q22)
      print("##########################################")
    }

    if (it_num == max.iter) {
      logg <- paste("The maximum number of iterations (i.e. ", it_num,") has been reached.", "\n", sep = "")
      message(logg)
      logs <- paste(na.omit(logs), logg, sep = "//")
    }

    if (!missing(save.file.name)) {
      list.to.save <- list(lst.parameters = L1, cov_m = - Q22, yEst = y1, xEst = x1,
                           it_num = it_num)
      list.name <- paste("semi.results_", it_num, sep = "")
      assign(list.name, list.to.save)
      AddObjectToRdata(list.to.save, list.name, rda_file = save.file.name, overwrite = TRUE)

    }
  }

  return(list(lst.parameters = L1, cov_m = -Q22, yEst = y1, xEst = x1,
              it_num = it_num, lst.parameters_previous.step = L0, logs = logs))
}



################################################################################

SafeDeparse <- function(expr){
  ## Function for "safe" split of expression (in our case formula) in the way, that in the final
  ## string will be no jump over the next line


  ret <- paste(deparse(expr), collapse = "")
  ## Add more rows into one with whitespaces

  # Removing of "whitespace"
  gsub("[[:space:]][[:space:]]+", " ", ret)
}

################################################################################

ConditionForIteration <- function(L0, L1, th) {
  ## The output is logical value TRUE, in case the condition of the input into "do while"
  ## is satisfied.
  ## L0 . . . list containing values of parameters from previous step
  ## L0 . . . list containing current values of parameter
  ## th . . . threshold

  L0[which(unlist(L0) == 0)] <- 10 ^ (-6)

  if (sum(abs(unlist(L1) - unlist(L0)) / abs(unlist(L0)) > th) > 0) {
    output <- TRUE
  } else {
    output <- FALSE
  }

  return(output)
}

###############################################################################

CreateB2 <- function(LOF, LP, xvec, m){
  ## Function creates B2 matrix in algorithm
  ## LOF  ... List of all functions passed by user.
  ##          Derivatives with respect to parameter are on the positions 3 and later in the correct order
  ## LP   ... list containing values of parameters in the correct order
  ## xvec ... vector of x values
  ## m    ... number of parameters

  B2 <- sapply(1:m, function(i) {
    sapply(xvec, function(val, LPP){do.call(LOF[[2+i]], args=c(val, LPP))}, LP)
  })

  return(-B2)
}

################################################################################

IsListOK <- function(List) {
  ## Function which checks in list data structure for presence of NaN, NA, Inf or -Inf.
  ## It can be used either for list's components as well as for particular elements of
  ## components of list.

  pv <- sum(sapply(List, `%in%`, x = NA), sapply(List, `%in%`, x = NaN),
            sapply(List, `%in%`, x = Inf), sapply(List, `%in%`, x = - Inf))

  if (pv == 0) {

    return(TRUE)

  } else {

    return(FALSE)
  }
}

################################################################################

AddObjectToRdata <- function(obj, name_obj, rda_file, overwrite = FALSE) {
  ## Function adds on another objects into the RData file. Copied (and modified) from:
  ## https://stackoverflow.com/questions/38364745/add-a-data-frame-to-an-existing-rdata-file
  .dummy <- NULL
  if (!file.exists(rda_file)) save(.dummy, file = rda_file)

  old_e <- new.env()
  new_e <- new.env()

  load(file = rda_file, envir = old_e)

  new_e[[name_obj]] <- obj

  # merge object from old environment with the new environment
  # ls(old_e) is a character vector of the object names
  if (overwrite) {

    # the old variables take precedence over the new ones
    invisible(sapply(ls(new_e), function(x)
      assign(x, get(x, envir = new_e), envir = old_e)))

    # And finally we save the variables in the environment
    save(list = ls(old_e), file = rda_file, envir = old_e)
  }
  else {
    invisible(sapply(ls(old_e), function(x)
      assign(x, get(x, envir = old_e), envir = new_e)))

    # And finally we save the variables in the environment
    save(list = ls(new_e), file = rda_file, envir = new_e)
  }
}

