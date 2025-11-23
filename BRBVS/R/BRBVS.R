#' Bivariate Rank-Based Variable Selection
#'
#' This function performs bivariate rank-based variable selection (BRBVS) based on copula survival copula models.
#' It computes rankings for covariates and selects a specified number of variables according to the estimated probabilities.
#' The function returns rankings and selected variables for different criteria.
#'
#' @param x Covariates matrix as a data frame. Input matrix containing the predictor variables.
#' @param y Time to events and censoring matrix as a data frame.
#' @param kmax Numeric. The maximum number of variables to be selected. Must be positive, non-zero, and
#'        less than or equal to the number of columns in `x`.
#' @param copula Character. Type of copula employed in the algorithm. Must be one of the following types:
#'        `N`, `C0`, `C90`, `C180`, `C270`, `GAL0`, `GAL90`, `GAL180`, `GAL270`,
#'        `J0`, `J90`, `J180`, `J270`, `G0`, `G90`, `G180`, `G270`, `F`, `AMH`,
#'        `FGM`, `T`, `PL`, `HO` default is `C0`.
#'        See GJRM package documentation for details: [GJRM package](https://CRAN.R-project.org/package=GJRM).
#' @param margins Character. Type of margin employed in the algorithm. Must be one of `PH`, `PO`, `probit`.
#'        Default is c(`PH`, `PO`).
#'        See GJRM package documentation for more on margins: [GJRM package](https://CRAN.R-project.org/package=GJRM).
#' @param m Numeric. Subsample size, typically set to n/2 where n is the number of observations.
#' @param tau Numeric. A user-defined threshold for variable selection. Must be in the interval (0,1), exclusive.
#' @param n.rep Integer. Number of Bootstrap replicates. Must be positive.
#' @param metric Character, specifies the metric used for ranking the variables.
#'        Must be one of 'CE', 'FIM', 'Abs'. Default is 'FIM'.
#'
#'
#' @return A list containing the following components:
#'   - `mtx.act1E`: Numeric vector of indices of the active variables selected based on the first survival function. Remaining positions (up to 'kmax') are filled with 0.
#'   - `score.r1E`: Numeric vector of the ranked scores for variable selection based on the first survival function, with remaining positions (up to 'kmax'-1) filled with 0.
#'   - `freq.rel1E`: Numeric vector of the relative frequencies of selected variables based on the first survival function (frequencies divided by 'n.rep').
#'   - `mtx.act2E`: Numeric vector of indices of the active variables selected based on the second survival function. Remaining positions (up to 'kmax') are filled with 0.
#'   - `score.r2E`: Numeric vector of the ranked scores for variable selection based on the second survival function, with remaining positions (up to 'kmax'-1) filled with 0.
#'   - `freq.rel2E`: Numeric vector of the relative frequencies of selected variables based on the second survival function (frequencies divided by 'n.rep').
#'   - `metric`: The metric used for ranking the variables.
#'   - `kmax`: The maximum number of variables to be selected.
#'   - `copula`: The type of copula employed in the algorithm.
#'   - `margins`: The type of margins employed in the algorithm.
#'.  - `Namecondings`: Table with name of covariates and encoding used in the output.
#'
#' @export
#'
#'
#' @examples
#'
#'###############################################
#'# Example based on AREDS dataset
#'# This analysis serves solely as a
#'# demonstration of the function's capabilities.
#'###############################################
#'\donttest{
#'data(AREDS)
#'Y<- AREDS[,c('t11','t12', 't21', 't22', 'cens1', 'cens2', 'cens')]
#'X<- AREDS[,c(3, 9)]
#'# Including just 1 covariates as example
#'X$SevScale1E <- scale(as.numeric( X$SevScale1E))
#'X$SevScale2E <- scale(as.numeric(X$SevScale1E))
#'
#'
#' Bivrbvs<- BRBVS(y=Y, x=X, kmax=2,copula='C0',
#'                       margins=c('PO','PO'),
#'                       m=628 , # try to set m=628 (628 is the sample size)
#'                       tau=0.5,
#'                       n.rep=1, # number of bootstrap = 1
#'                       metric='FIM')
#'                       }
#'
#'
#'
#'
#'
#'
BRBVS=function(y, x, kmax, copula, margins , m, tau, n.rep, metric){



  if(!("cens1" %in% names(y))  ) stop("You must provide both censoring indicators.")
  if(!("cens2" %in% names(y))  ) stop("You must provide both censoring indicators.")
  if(!("t11" %in% names(y))  ) stop("You must provide all the time to events. See package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  if(!("t12" %in% names(y))  ) stop("You must provide all the time to events. See package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  if(!("t22" %in% names(y))  ) stop("You must provide all the time to events. See package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  if(!("t21" %in% names(y))  ) stop("You must provide all the time to events. See package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")

  # Check for kmax
  if (kmax <= 0 || kmax > ncol(x)) {
    stop("Error: kmax must be a positive value different from zero and never bigger than the number of columns of x.")
  }

  # Check for copula
  valid_copulas = c("N", "C0", "C90", "C180", "C270", "GAL0", "GAL90", "GAL180", "GAL270",
                    "J0", "J90", "J180", "J270", "G0", "G90", "G180", "G270", "F", "AMH", "FGM", "T", "PL", "HO")
  if (!(copula %in% valid_copulas)) {
    stop("Error: Invalid value for copula see package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  }

  # Check for margins
  valid_margins = c("PH", "PO", "probit")
  if (!(margins[1] %in% valid_margins)) {
    stop("Error: Invalid value for the first margin see package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  }
  if (!(margins[2] %in% valid_margins)) {
    stop("Error: Invalid value for the second margin see package GJRM link https://cran.r-project.org/web/packages/GJRM/index.html.")
  }
  # Check for tau
  if (tau <= 0 || tau >= 1) {
    stop("Error: tau must be a value in the interval (0,1).")
  }

  # Check for n.rep
  if (!is.numeric(n.rep) || n.rep <= 0 || floor(n.rep) != n.rep) {
    stop("Error: n.rep must be a positive integer.")
  }

  # Check for valid measure
  valid_metrics = c('CE', 'FIM', 'Abs')
  if (!(metric %in% valid_metrics)) {
    stop("Error: Invalid value for metrics. Choose from 'CE', 'FIM', 'Abs'. Default is 'FIM'.")
  }

  Features<- names(x)
  Code <-  paste('z', 1:dim(x)[2], sep='')
  names(x) <- paste('z', 1:dim(x)[2], sep='')



  ny <- dim(y)[1] # this is the lenght of y.
  mtx.user=rbvs.user(x, y, ny, m, copula, margins, n.rep, metric)
  #rank1 occhio
  lst.out1E=select.user(m.rank=mtx.user$CE1, kmax=kmax, tau)
  mtx.act1E=c(lst.out1E$active, rep(0, (kmax-length(lst.out1E$active))))
  score.r1E=c(lst.out1E$score.r, rep(0, ((kmax-1)-length(lst.out1E$score.r))))
  frequencies1E=c(lst.out1E$frequencies, rep(0, (kmax-length(lst.out1E$frequencies))))
  #rank2E
  lst.out2E=select.user(m.rank=mtx.user$CE2, kmax=kmax, tau)
  mtx.act2E=c(lst.out2E$active, rep(0, (kmax-length(lst.out2E$active))))
  score.r2E=c(lst.out2E$score.r, rep(0, ((kmax-1)-length(lst.out2E$score.r))))
  frequencies2E=c(lst.out2E$frequencies, rep(0, (kmax-length(lst.out2E$frequencies))))


# secondo ranking {beta \ne 0}

# lst.out1EABS=select.user(m.rank=mtx.user$CE1.2, kmax=kmax, tau)
#  mtx.act1EABS=c(lst.out1EABS$active, rep(0, (kmax-length(lst.out1EABS$active))))
#  score.r1EABS=c(lst.out1EABS$score.r, rep(0, ((kmax-1)-length(lst.out1EABS$score.r))))
#  frequencies1EABS=c(lst.out1EABS$frequencies, rep(0, (kmax-length(lst.out1EABS$frequencies))))
# rank2E
#  lst.out2EABS=select.user(m.rank=mtx.user$CE2.2, kmax=kmax, tau)
#  mtx.act2EABS=c(lst.out2EABS$active, rep(0, (kmax-length(lst.out2EABS$active))))
#  score.r2EABS=c(lst.out2EABS$score.r, rep(0, ((kmax-1)-length(lst.out2EABS$score.r))))
#  frequencies2EABS=c(lst.out2EABS$frequencies, rep(0, (kmax-length(lst.out2EABS$frequencies))))

Namecondings <- cbind(Features,Code )

  # check if frequencies/n.rep makes sense
  Res_BRBVS<- list(mtx.act1E=mtx.act1E,
                   scores1E=lst.out1E$scores,
                   freq.rel1E=frequencies1E/n.rep,
                   mtx.act2E=mtx.act2E,
                   scores2E=lst.out2E$scores,
                   freq.rel2E=frequencies2E/n.rep,
                   metric=metric,
                   kmax=kmax,
                   copula=copula,
                   margins=margins,
                   tau=tau,
                   Namecondings=Namecondings
  )



  class(Res_BRBVS) <- 'BRBVS'

  return(Res_BRBVS)

}





