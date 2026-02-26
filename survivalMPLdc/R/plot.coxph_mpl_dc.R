#' @title Plot a baseline hazard estimates from coxph_mpl_dc Object
#'
#' @description Plot the baseline hazard with the confidence interval estimates
#'
#' @method plot coxph_mpl_dc
#'
#' @param x an object inheriting from class \code{\link{coxph_mpl_dc}}
#' @param parameter the set of parameters of interest.  Indicate \code{parameters="theta"} for the baseline hazard
#' estimated by \eqn{theta} and \code{parameters="gamma"} for the baseline hazard estimated by \eqn{gamma}
#' @param funtype the type of function for ploting, i.e. \code{funtype="hazard"} for baseline hazard,
#' \code{funtype="cumhazard"} for baseline cumulative hazard and \code{funtype="survival"} for
#' baseline survival function
#' @param xout the time values for the baseline hazard plot
#' @param se se=TRUE gives both the MPL baseline estimates and 95\% confidence interval plots while se=FALSE
#' gives only the MPL baseline estimate plot.
#' @param ltys a line type vector with two components, the first component is the line type of the baseline hazard
#' while the second component is the line type of the 95\% confidence interval
#' @param cols a colour vector with two components, the first component is the colour of the baseline hazard
#' while the second component is the colour the 95\% confidence interval
#' @param ... other arguments
#'
#'
#' @details When the input is of class \code{coxph_mpl_dc} and \code{parameters=="theta"}, the baseline estimates
#' base on \eqn{\theta} and xout (with the corresponding 95\% confidence interval if se=TRUE ) are ploted.
#' When the input is of class \code{coxph_mpl_dc} and \code{parameters=="gamma"}, the baseline hazard estimates
#' based on \eqn{\gamma} and xout (with the corresponding 95\% confidence interval if se=TRUE ) are ploted.
#'
#' @return the baseline hazard plot
#'
#' @author Jing Xu, Jun Ma, Thomas Fung
#'
#' @references Brodaty H, Connors M, Xu J, Woodward M, Ames D. (2014). \emph{"Predictors of institutionalization
#' in dementia: a three year longitudinal study"}. Journal of Alzheimers Disease 40, 221-226.
#'
#' Xu J, Ma J, Connors MH, Brodaty H. (2018). \emph{"Proportional hazard model estimation under dependent censoring using copulas and penalized likelihood"}.
#' Statistics in Medicine 37, 2238–2251.
#'
#'
#' @seealso \code{\link{coef.coxph_mpl_dc}}, \code{\link{coxph_mpl_dc.control}}, \code{\link{coxph_mpl_dc}}
#'
#' @importFrom copula indepCopula claytonCopula gumbelCopula frankCopula iTau rCopula
#' @importFrom survival coxph Surv
#' @importFrom splines2 iSpline mSpline
#' @importFrom graphics plot
#' @importFrom stats approx
#' @importFrom graphics lines
#'
#' @examples
#' \donttest{
#'  ##-- Copula types
#'  copula3 <- 'frank'
#'
#' ##-- A real example
#' ##-- One dataset from Prospective Research in Memory Clinics (PRIME) study
#' ##-- Refer to article Brodaty et al (2014),
#' ##   the predictors of institutionalization of dementia patients over 3-year study period
#'
#' data(PRIME)
#'
#' surv<-as.matrix(PRIME[,1:3]) #time, event and dependent censoring indicators
#' cova<-as.matrix(PRIME[, -c(1:3)]) #covariates
#' colMeans(surv[,2:3])  #the proportions of event and dependent censoring
#'
#' n<-dim(PRIME)[1];print(n)
#' p<-dim(PRIME)[2]-3;print(p)
#' names(PRIME)
#'
#' ##--MPL estimate Cox proportional hazard model for institutionalization under medium positive
#' ##--dependent censoring
#' control <- coxph_mpl_dc.control(ordSp = 4,
#'                                 binCount = 200, tie = 'Yes',
#'                                 tau = 0.5, copula = copula3,
#'                                 pent = 'penalty_mspl', smpart = 'REML',
#'                                 penc = 'penalty_mspl', smparc = 'REML',
#'                                 cat.smpar = 'No' )
#'
#' coxMPLests_tau <- coxph_mpl_dc(surv=surv, cova=cova, control=control, )
#'
#' plot(x = coxMPLests_tau, parameter = "theta", funtype="hazard",
#'      xout = seq(0, 36, 0.01), se = TRUE,
#'      cols=c("blue", "red"), ltys=c(1, 2), type="l", lwd=1, cex=1, cex.axis=1, cex.lab=1,
#'      xlab="Time (Month)", ylab="Hazard",
#'      xlim=c(0, 36), ylim=c(0, 0.05)
#'      )
#'      title("MPL Hazard", cex.main=1)
#'      legend( 'topleft',legend = c( expression(tau==0.5), "95% Confidence Interval"),
#'      col = c("blue", "red"),
#'      lty = c(1, 2),
#'      cex = 1)
#'
#'plot(x = coxMPLests_tau, parameter = "theta", funtype="cumhazard",
#'     xout = seq(0, 36, 0.01), se = TRUE,
#'     cols=c("blue", "red"), ltys=c(1, 2),
#'     type="l", lwd=1, cex=1, cex.axis=1, cex.lab=1,
#'     xlab="Time (Month)", ylab="Hazard",
#'     xlim=c(0, 36), ylim=c(0, 1.2)
#')
#'title("MPL Cumulative Hazard", cex.main=1)
#'legend( 'topleft',
#'        legend = c( expression(tau==0.5), "95% Confidence Interval"),
#'        col = c("blue", "red"),
#'        lty = c(1, 2),
#'        cex = 1
#')
#'
#'plot(x = coxMPLests_tau, parameter = "theta", funtype="survival",
#'     xout = seq(0, 36, 0.01), se = TRUE,
#'     cols=c("blue", "red"), ltys=c(1, 2),
#'     type="l", lwd=1, cex=1, cex.axis=1, cex.lab=1,
#'     xlab="Time (Month)", ylab="Hazard",
#'     xlim=c(0, 36), ylim=c(0, 1)
#')
#'title("MPL Survival", cex.main=1)
#'legend( 'bottomleft',
#'        legend = c( expression(tau==0.5), "95% Confidence Interval"),
#'        col = c("blue", "red"),
#'        lty = c(1, 2),
#'        cex = 1
#')
#'
#'}
#'
#' @export

plot.coxph_mpl_dc<-function(x, parameter="theta", funtype="hazard", xout, se=TRUE, ltys, cols, ...)
{
  surv <- x$surv
  Xi<-surv[,1]
  max_Xi<-max(Xi)

  if(missing(parameter)){parameter<-"theta"}
  if(missing(funtype)){funtype<-"hazard"}
  if(missing(xout)){ xout <- seq(0, max_Xi, 0.01) }
  if(missing(se)){se<-FALSE}
  if(missing(cols)){cols<-c("black", "black")}
  if(missing(ltys)){ltys<-c(1, 2)}

  if(parameter=="theta")
  {
    if(funtype=="hazard"){

      mpl_h0 <- x$mpl_h0t
      mpl_h0_sd <- x$mpl_ht0_sd
      mpl_h0i <- approx( Xi, mpl_h0, xout = xout,
                         method="constant", rule = 2, ties = mean)$y
      mpl_h0i_sd <- approx( Xi, mpl_h0_sd, xout = xout,
                            method="constant", rule = 2, ties = mean)$y
      yout <- mpl_h0i
      youtu <- mpl_h0i + 1.96*mpl_h0i_sd
      youtl <- mpl_h0i - 1.96*mpl_h0i_sd
      youtl[youtl<0]<-0

    }else if(funtype=="cumhazard"){

      mpl_H0 <- x$mpl_H0t
      mpl_H0_sd <- x$mpl_Ht0_sd
      mpl_H0i <- approx( Xi, mpl_H0, xout = xout,
                         method="constant", rule = 2, ties = mean)$y
      mpl_H0i_sd <- approx( Xi, mpl_H0_sd, xout = xout,
                            method="constant", rule = 2, ties = mean)$y
      yout <- mpl_H0i
      youtu <- mpl_H0i + 1.96*mpl_H0i_sd
      youtl <- mpl_H0i - 1.96*mpl_H0i_sd
      youtl[youtl<0]<-0

      }else if(funtype=="survival"){

        mpl_S0 <- x$mpl_S0t
        mpl_S0_sd <- x$mpl_St0_sd
        mpl_S0i <- approx( Xi, mpl_S0, xout = xout,
                           method="constant", rule = 2, ties = mean)$y
        mpl_S0i_sd <- approx( Xi, mpl_S0_sd, xout = xout,
                              method="constant", rule = 2, ties = mean)$y
        yout <- mpl_S0i
        youtu <- mpl_S0i + 1.96*mpl_S0i_sd
        youtl <- mpl_S0i - 1.96*mpl_S0i_sd
        youtl[youtl<0]<-0
        youtu[youtu>1]<-1

        }else (stop("Error! Define a proper function type"))

  }else if(parameter=="gamma"){

    if(funtype=="hazard"){

      mpl_h0 <- x$mpl_h0c
      mpl_h0_sd <- x$mpl_hc0_sd
      mpl_h0i <- approx( Xi, mpl_h0, xout = xout,
                         method="constant", rule = 2, ties = mean)$y
      mpl_h0i_sd <- approx( Xi, mpl_h0_sd, xout = xout,
                            method="constant", rule = 2, ties = mean)$y
      yout <- mpl_h0i
      youtu <- mpl_h0i + 1.96*mpl_h0i_sd
      youtl <- mpl_h0i - 1.96*mpl_h0i_sd
      youtl[youtl<0]<-0

    }else if(funtype=="cumhazard"){

      mpl_H0 <- x$mpl_H0c
      mpl_H0_sd <- x$mpl_Hc0_sd
      mpl_H0i <- approx( Xi, mpl_H0, xout = xout,
                         method="constant", rule = 2, ties = mean)$y
      mpl_H0i_sd <- approx( Xi, mpl_H0_sd, xout = xout,
                            method="constant", rule = 2, ties = mean)$y
      yout <- mpl_H0i
      youtu <- mpl_H0i + 1.96*mpl_H0i_sd
      youtl <- mpl_H0i - 1.96*mpl_H0i_sd
      youtl[youtl<0]<-0

    }else if(funtype=="survival"){

      mpl_S0 <- x$mpl_S0c
      mpl_S0_sd <- x$mpl_Sc0_sd
      mpl_S0i <- approx( Xi, mpl_S0, xout = xout,
                         method="constant", rule = 2, ties = mean)$y
      mpl_S0i_sd <- approx( Xi, mpl_S0_sd, xout = xout,
                            method="constant", rule = 2, ties = mean)$y
      yout <- mpl_S0i
      youtu <- mpl_S0i + 1.96*mpl_S0i_sd
      youtl <- mpl_S0i - 1.96*mpl_S0i_sd
      youtl[youtl<0]<-0
      youtu[youtu>1]<-1

    }else (stop("Error! Define a proper function type"))


  }else (stop("Error! Define a proper parameter name"))

  if(se==TRUE){

    plot( x = xout, y = yout, col=cols[1], lty=ltys[1],... )
    lines( x = xout, y = youtu, col=cols[2], lty=ltys[2],... )
    lines( x = xout, y = youtl, col=cols[2], lty=ltys[2],... )

  }else if(se==FALSE){

    plot( x = xout, y = yout, col=cols[1], lty=ltys[1],... )

  }

}
