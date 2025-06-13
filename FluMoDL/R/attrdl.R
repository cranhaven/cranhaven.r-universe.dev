#' Attributable risk from distributed lag nonlinear models
#'
#' This is a general function that computes attributable risk (attributable numbers or fractions)
#' from distributed lag nonlinear models.
#'
#' Original function and documentation written by Antonio Gasparrini and available
#' \href{https://github.com/gasparrini/2014_gasparrini_BMCmrm_Rcodedata}{here}.
#' Slightly amended by Theodore Lytras for use with FluMoDL.
#'
#' @param x An exposure vector OR (only for \code{dir="back"}) a matrix of lagged exposures, for which
#' the attributable risk needs to be computed.
#' @param basis The object of class "crossbasis" used for fitting the model.
#' @param cases The cases vector OR (only for \code{dir="forw"}) the matrix of future cases corresponding to \code{x}.
#' @param model The fitted model. You need to provide either this, or arguments \code{coef} and \code{vcov}.
#' \emph{The model MUST have a log link function.}
#' @param coef Coefficients for \code{basis} IF \code{model} is not provided
#' @param vcov Variance-covariance matrix for \code{basis} IF \code{model} is not provided
#' @param type Either "an" or "af" for attributable number or attributable fraction
#' @param dir Either "back" or "forw" for backward or forward perspectives of attributable risk
#' @param tot If \code{TRUE}, the total attributable risk is computed (number or fraction, depending on argument \code{type})
#' @param cen The reference value used as the counterfactual scenario (the comparator)
#' @param range The range of exposure (for which the attributable risk, compared to \code{cen}, is
#' calculated). If \code{NULL}, the whole range is used.
#' @param sim Set to \code{TRUE} if Monte Carlo simulation samples should be returned.
#' @param nsim Number of simulation samples desired (only for \code{nsim=TRUE}).
#' @param sub Subset of \code{cases} for which to calculate the attributable risk (as an integer index
#' vector). Defaults to \code{1:length(cases)}. Argument \code{cases} should be a vector (not a matrix).
#'
#' @details Documentation below copied from the
#' \href{https://github.com/gasparrini/2014_gasparrini_BMCmrm_Rcodedata/blob/master/attrdl.pdf}{original source}.
#'
#' This function computes the attributable fraction or number for a specific exposure
#' scenario and associated cases, given an estimated exposure-lag-response association defined
#' by a DLNM. Either forward or backward versions of attributable risk measures are available
#' in this setting. The method is described by Gasparrini and Leone (2014), see references below.
#' The function works in combination with other functions in the package dlnm, which is assumed
#' to be available.
#'
#' The exposure and cases are provided by the arguments x and cases, respectively. The original
#' cross-basis and fitted model containg it used for estimation are provided by the arguments
#' basis and model, respectively. Alternatively, the user can provide estimated coefficients and
#' (co)variance matrix with coef and vcov.
#'
#' The function works both with time series and non-time series data. In a time series setting,
#' both x and cases represent a complete series of ordered observations. More generally, the user
#' can apply this function for any kind of data: in this case x must be a matrix of lagged
#' exposures when dir="back", and cases must be a matrix of future cases dir="forw". The function
#' can compute the total attributable risk (if tot=TRUE, the default) or the contribution for each
#' observation. The argument cen defines the value used as counterfactual scenario.
#'
#' If sim=TRUE, the function computes samples of the attributable risk measures by simulating from
#' the assumed normal distribution of the estimated coefficients (only implemented for total
#' estimates). These samples can be used to defined empirical confidence intervals.
#'
#' @return By default, a numeric scalar representing the total attributable fraction or number.
#' If sim=TRUE, a vector of the simulated samples with length nsim. If tot=FALSE, a vector with
#' contributions for all the observations (see Note below). These quantities are defined versus
#' a counterfactual scenario defined through the argument cen.
#'
#' @note The function handles missing values in both the x and cases objects, excluding incomplete
#' observations (also due to lagging) accordingly. However, the total attributable number is
#' rescaled to match the fraction using as denominator the total observed number in cases. This
#' approach uses the all the available information even in the presence of missing values in x.
#' All of this under the assumption that the missing mechanism is unrelated with both exposure and
#' cases values.
#'
#' The functions can be also used with estimates from DLNMs reduced to the overall cumulative
#' exposure-response through the function crossreduce in the package dlnm. In this case, the
#' modified coefficients and (co)variance matrix of the reduced cross-basis in basis must be
#' passed using the arguments coef and vcov. This option can be useful when the original estimates
#' from the full cross-basis are not available any more, for example following a meta-analysis.
#' Given the lag-specific estimates are not available in this case, only the forward version of
#' attributable risk (dir="forw") can be computed. See Gasparrini and Leone (2014) for further info.
#'
#' @author Original author: Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>
#'
#' @references \itemize{ \item Gasparrini A, Leone M. Attributable risk from distributed lag models.
#' \href{https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-14-55}{BMC Med Res Methodol} 2014;14:55.}
#'
#' @import dlnm
#' @importFrom tsModel Lag
#' @importFrom utils packageVersion
#' @importFrom utils getFromNamespace
#'
#' @examples \donttest{
#' # load the package
#' library(FluMoDL)  # package dlnm is automatically loaded
#'
#' # define the cross-basis and fit the model
#' cb <- crossbasis(chicagoNMMAPS$temp, lag=30, argvar=list(fun="bs",
#'    knots=c(-10,3,18)), arglag=list(knots=c(1,3,10)))
#' library(splines)
#' model <- glm(death ~ cb + ns(time, 7*14) + dow,
#'    family=quasipoisson(), chicagoNMMAPS)
#'
#' # global backward attributable risk of temperature (number and fraction)
#' attrdl(chicagoNMMAPS$temp,cb,chicagoNMMAPS$death,model,type="an",cen=21)
#' attrdl(chicagoNMMAPS$temp,cb,chicagoNMMAPS$death,model,cen=21)
#'
#' # global forward attributable fraction
#' attrdl(chicagoNMMAPS$temp,cb,chicagoNMMAPS$death,model,dir="forw",cen=21)
#'
#' # empirical confidence intervals
#' afsim <- attrdl(chicagoNMMAPS$temp,cb,chicagoNMMAPS$death,model,cen=21,
#'    sim=TRUE,nsim=1000)
#' quantile(afsim,c(2.5,97.5)/100)
#'
#' # attributable fraction component due to heat and cold
#' attrdl(chicagoNMMAPS$temp,cb,chicagoNMMAPS$death,model,cen=21,range=c(21,100))
#' attrdl(chicagoNMMAPS$temp,cb,chicagoNMMAPS$death,model,cen=21,range=c(-100,21))
#'
#' # daily attributable deaths in the second month
#' attrdl(chicagoNMMAPS$temp,cb,chicagoNMMAPS$death,model,type="an",
#'    tot=FALSE,cen=21)[31:60]
#'
#' }
#'
#' @export

attrdl <- function(x,basis,cases,model=NULL,coef=NULL,vcov=NULL,type="af",
  dir="back",tot=TRUE,cen,range=NULL,sim=FALSE,nsim=5000,sub=1:length(cases)) {

  .getcoef <- getFromNamespace("getcoef", "dlnm")
  .getvcov <- getFromNamespace("getvcov", "dlnm")
  .getlink <- getFromNamespace("getlink", "dlnm")
  .seqlag <- getFromNamespace("seqlag", "dlnm")
  .mkXpred <- getFromNamespace("mkXpred", "dlnm")


  # CHECK VERSION OF THE DLNM PACKAGE
  if(packageVersion("dlnm")<"2.2.0")
    stop("update dlnm package to version >= 2.2.0")
#
  # EXTRACT NAME AND CHECK type AND dir
  name <- deparse(substitute(basis))
  type <- match.arg(type,c("an","af"))
  dir <- match.arg(dir,c("back","forw"))
#
  # DEFINE CENTERING
  if(missing(cen) && is.null(cen <- attr(basis,"argvar")$cen))
    stop("'cen' must be provided")
  if(!is.numeric(cen) && length(cen)>1L) stop("'cen' must be a numeric scalar")
  attributes(basis)$argvar$cen <- NULL
#
  # SELECT RANGE (FORCE TO CENTERING VALUE OTHERWISE, MEANING NULL RISK)
  if(!is.null(range)) x[x<range[1]|x>range[2]] <- cen
#
  # COMPUTE THE MATRIX OF
  #   - LAGGED EXPOSURES IF dir="back"
  #   - CONSTANT EXPOSURES ALONG LAGS IF dir="forw"
  lag <- attr(basis,"lag")
  if(NCOL(x)==1L) {
    at <- if(dir=="back") tsModel::Lag(x,seq(lag[1],lag[2])) else
      matrix(rep(x,diff(lag)+1),length(x))
  } else {
    if(dir=="forw") stop("'x' must be a vector when dir='forw'")
    if(ncol(at <- x)!=diff(lag)+1)
      stop("dimension of 'x' not compatible with 'basis'")
  }
#
  # NUMBER USED FOR THE CONTRIBUTION AT EACH TIME IN FORWARD TYPE
  #   - IF cases PROVIDED AS A MATRIX, TAKE THE ROW AVERAGE
  #   - IF PROVIDED AS A TIME SERIES, COMPUTE THE FORWARD MOVING AVERAGE
  #   - THIS EXCLUDES MISSING ACCORDINGLY
  # ALSO COMPUTE THE DENOMINATOR TO BE USED BELOW
  if(NROW(cases)!=NROW(at)) stop("'x' and 'cases' not consistent")
  if(NCOL(cases)>1L) {
    if(dir=="back") stop("'cases' must be a vector if dir='back'")
    if(ncol(cases)!=diff(lag)+1) stop("dimension of 'cases' not compatible")
    den <- sum(rowMeans(cases,na.rm=TRUE),na.rm=TRUE)
    cases <- rowMeans(cases)
  } else {
    den <- sum(cases[sub],na.rm=TRUE)
    if(dir=="forw")
      cases <- rowMeans(as.matrix(tsModel::Lag(cases,-seq(lag[1],lag[2]))))
  }
#
################################################################################
#
  # EXTRACT COEF AND VCOV IF MODEL IS PROVIDED
  if(!is.null(model)) {
    cond <- paste0(name,"[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}")
    if(ncol(basis)==1L) cond <- name
    model.class <- class(model)
    coef <- .getcoef(model,model.class)
    ind <- grep(cond,names(coef))
    coef <- coef[ind]
    vcov <- .getvcov(model,model.class)[ind,ind,drop=FALSE]
    model.link <- .getlink(model,model.class)
    if(model.link!="log") stop("'model' must have a log link function")
  }
#
  # IF REDUCED ESTIMATES ARE PROVIDED
  typebasis <- ifelse(length(coef)!=ncol(basis),"one","cb")
#
################################################################################
#
  # PREPARE THE ARGUMENTS FOR TH BASIS TRANSFORMATION
  predvar <- if(typebasis=="one") x else seq(NROW(at))
  predlag <- if(typebasis=="one") 0 else .seqlag(lag)
#
  # CREATE THE MATRIX OF TRANSFORMED CENTRED VARIABLES (DEPENDENT ON typebasis)
  if(typebasis=="cb") {
    Xpred <- .mkXpred(typebasis,basis,at,predvar,predlag,cen)
    Xpredall <- 0
    for (i in seq(length(predlag))) {
      ind <- seq(length(predvar))+length(predvar)*(i-1)
      Xpredall <- Xpredall + Xpred[ind,,drop=FALSE]
    }
  } else {
    basis <- do.call(onebasis,c(list(x=x),attr(basis,"argvar")))
    Xpredall <- .mkXpred(typebasis,basis,x,predvar,predlag,cen)
  }
#
  # CHECK DIMENSIONS
  if(length(coef)!=ncol(Xpredall))
    stop("arguments 'basis' do not match 'model' or 'coef'-'vcov'")
  if(any(dim(vcov)!=c(length(coef),length(coef))))
    stop("arguments 'coef' and 'vcov' do no match")
  if(typebasis=="one" && dir=="back")
    stop("only dir='forw' allowed for reduced estimates")
#
################################################################################
#
  # COMPUTE AF AND AN
  af <- 1-exp(-drop(as.matrix(Xpredall%*%coef)))
  an <- (af*cases)
#
  # TOTAL
  #   - SELECT NON-MISSING OBS CONTRIBUTING TO COMPUTATION
  #   - DERIVE TOTAL AF
  #   - COMPUTE TOTAL AN WITH ADJUSTED DENOMINATOR (OBSERVED TOTAL NUMBER)
  if(tot) {
    isna <- is.na(an[sub])
    af <- sum(an[sub][!isna])/sum(cases[sub][!isna])
    an <- af*sum(cases[sub], na.rm=TRUE)
  } else {
    af <- af[sub]
    an <- an[sub]
  }
#
################################################################################
#
  # EMPIRICAL CONFIDENCE INTERVALS
  if(sim) {
    # SAMPLE COEF
    k <- length(coef)
    eigen <- eigen(vcov)
    X <- matrix(rnorm(length(coef)*nsim),nsim)
    coefsim <- coef + eigen$vectors %*% diag(sqrt(eigen$values),k) %*% t(X)
    if (tot) {
        # RUN THE LOOP
        afsim <- apply(coefsim,2, function(coefi) {
        ani <- ((1-exp(-drop(Xpredall%*%coefi)))*cases)
        sum(ani[!is.na(ani)])/sum(cases[!is.na(ani)])
        })
        ansim <- afsim*den
    } else {
        # RUN THE LOOP
        afsim <- apply(coefsim,2, function(coefi) {
          ani <- ((1-exp(-drop(Xpredall%*%coefi)))*cases)[sub]
          sum(ani[!is.na(ani)])/sum(cases[sub][!is.na(ani)])
        })
        ansim <- afsim*sum(cases[sub], na.rm=TRUE)
    }
  }
#
################################################################################
#
  res <- if(sim) {
    if(type=="an") ansim else afsim
  } else {
    if(type=="an") an else af
  }
#
  return(res)
}

#
