#' @title Bivariate generator of the bivariate exponential distribution (BED) based on the Moran-Downton model
#'
#' @description This function generates jointly random values from the BED. The required inputs are the n values to be
#' generated, the correlation coefficient, and the scale parameters of the marginal distributions.
#'
#' @usage rBED(rho,Betax,Betay,n)
#'
#' @param n Number of random values to be generated.
#' @param Betax Scale parameter of the marginal distribution of x.
#' @param Betay Scale parameter of the marginal distribution of y.
#' @param rho Correlation coefficient.
#'
#' @details The bivariate generator generates jointly exponential random values based on the conditional distribution
#' of Y given X=x based on Eq.18 described in \insertCite{Nagao1971;textual}{MDBED}. Thus, it first
#' generates random values of X; then, the conditional moments associated with the values of
#' x are computed. Finally, the random values of Y are obtained by drawing a random value from each conditional distribution
#' associated with each value of x.
#'
#' @return A dataframe with n random values generated.
#'
#' @note The equation of the conditional PDF used to generate the bivariate values is based on the Bessel
#' function. Therefore, for very extreme values this function may reach infinity. It may
#' generate NA values. The \code{\link{rBED}} function is set to avoid this problem for values of n lower than
#' 100000.
#'
#' @author Luis F. Duque <lfduquey@@gmail.com> <l.f.duque-yaguache2@@newcastle.ac.uk>
#'
#' @references \insertAllCited{}
#'
#'
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom stats approx punif rexp na.omit
#' @examples rBED(n=100,Betax=1,Betay=1,rho=0.85)
#' @export
rBED<-function(rho,Betax,Betay,n){

# define Global variables
i<-NULL

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
# 1. Function of the bivariate generator.

  EBDUS<-function(n,rho){

#-------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
# 1.1 Function of the conditional distribution of y given x.
# etahat = represent x in the standarized form.
# eta = represent y in the standarized form.

    ConD<-function(etahat,rho){


      # Conditional moments
      CEV<-1+rho*(etahat-1)
      CVar<-(1-rho)^2+2*rho*(1-rho)*etahat
      Csd<-sqrt(CVar)


      # Obtain n values of the conditional distribution of eta given etahat.
      LI<-CEV-4.5*Csd
      LI<-if(LI<0){0}else{LI}
      LS<-CEV+4.5*Csd
      neta<-seq(LI,LS, by=(LS-LI)/1000)
      C_eta<-1/(1-rho)*exp(-1/(1-rho)*(etahat*rho+neta))*besselI(2*sqrt(rho*etahat*neta)/(1-rho),0)
      C_eta[is.infinite(C_eta)]<-NA


      # Building of the CDF of the conditional distribution (numerical integration of C_eta (Riemann Sum))
      dx<-neta[2]-neta[1]
      C <- sum(C_eta,na.rm = TRUE)*dx #normalizing constant
      Fx<-cumsum(C_eta)*dx/C  # the fastest way to find the Fx


      # Generate a value of eta by drawing a ramdon value from the conditional distribution.
      pUn<-stats::punif(stats::runif(1))
      Rvalue<-stats::approx(Fx,neta,pUn)$y

      #-------------------------------------------------------------------------------------------------------------------
      # Export values
      return(Rvalue)
    }


#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
# 1.2 Generate values of etahat and arrange them in ascending order.

    etahat<-stats::rexp(n,rate = 1)
    etahat<-base::sort(etahat)

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
# 1.3 Generate the conditional distribution for each value of etahat and draw a value from this distribution to
# generate the correlated value of eta.

    numCores <- ifelse(parallel::detectCores()>=2,2,1)
    doParallel::registerDoParallel(numCores)
    eta<-foreach::foreach(i=1:length(etahat),.combine=c) %dopar% {
      eta<-ConD(etahat[i],rho=rho)}
    doParallel::stopImplicitCluster()

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
# 1.4 Export results

    Results<-data.frame(eta,etahat)
    return(Results)

  }

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
# 2. Obtain the correlated standarized variables of x and y, i.e. etahat and eta by using the EBDUS function.

  Results<-EBDUS(n,rho)

  # For very extreme values of neta the Bessel reaches infinity (see step 1.1), therefore the bivariate generator
  # might generate NA values. The bivariate generator is thus run several times until it generates the n required pairs
  # (x,y). Has been tested that two additional runs is enough for values of n < 100000.

  Standarized_Simulated_Values<-stats::na.omit(Results)
  if(length(Standarized_Simulated_Values[,1])<n) {
    j<-n-length(Standarized_Simulated_Values[,1])
    Results1<-EBDUS(j,rho)
    Results1<-stats::na.omit(Results1)
    Standarized_Simulated_Values<-rbind(Standarized_Simulated_Values,Results1)
    if(length(Standarized_Simulated_Values[,1])<n){
      k<-n-length(Standarized_Simulated_Values[,1])
      Results2<-EBDUS(k,rho)
      Standarized_Simulated_Values<-rbind(Standarized_Simulated_Values,Results2)
    }
  }

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
# 3. Obtain the real variables.

  Simulated_Values<-Standarized_Simulated_Values
  Simulated_Values$x<-Simulated_Values$eta*Betax
  Simulated_Values$y<-Simulated_Values$etahat*Betay
  Simulated_Values<-Simulated_Values[c(-1,-2)]

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
# 4. Export plots

  return(Simulated_Values)

}

