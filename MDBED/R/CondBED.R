#' @title Conditional distribution of Y given X=x of the bivariate exponential distribution (BED)
#' based on the Moran-Downton model
#'
#' @description This function computes the conditional PDF and CDF of Y given X=x of the BED
#' based on the equations of the conditional moments of the Moran-Downton model.
#'
#' @usage CondBED(rho,Betax,Betay,x)
#'
#' @param rho Correlation coefficient between the marginal distributions of x and y.
#' @param Betax Scale parameter of the marginal distribution of x.
#' @param Betay Scale parameter of the marginal distribution of y.
#' @param x a value or vector of values of the marginal distribution of x.
#'
#' @details This function computes the conditional PDF and CDF of Y given X=x. The conditional density is
#' computed based on the conditional moments of the Moran-Downton model described in Eqs. 21 and 22 in \insertCite{Nagao1971;textual}{MDBED}.
#' The conditional CDF is computed via numerical integration of the conditional PDF based on the Riemann sum method.
#'
#'
#' @return A list with a dataframe, named Conditional_Statistics, and a sublist, named Condyx, is provided.
#'         Conditional_Statistics contains the values of x with its conditional moments, and Condyx contains
#'         several dataframes with the values of the conditional PDF and CDF of each conditional distribution
#'         associated to each value of x. For each conditional distribution, the values of the conditional
#'         quantiles (yc) with its respective density values (fyx) and its associated cumulative conditional probability values (Fyx) are
#'         provided.The first dataframe of Condyx corresponds to the first value of x shown in Conditional_Statistics,
#'         the second dataframe corresponds to the second value of x shown in Conditional_Statistics and so on.
#'
#' @author Luis F. Duque <lfduquey@@gmail.com> <l.f.duque-yaguache2@@newcastle.ac.uk>
#'
#' @references \insertAllCited{}
#'
#' @importFrom stats approx punif rexp na.omit
#'
#' @examples
#' Data<-rBED(n=50,Betax=1,Betay=1,rho=0.85)
#' Conyx<-CondBED(rho=0.85,Betax=1,Betay=1,x=Data[,1])
#' @export
CondBED<-function(rho,Betax,Betay,x){

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
# 1. Function of the conditional distribution of y given x.


  ConD<-function(x1,rho,Betax,Betay){
    # Conditional moments.
    CEV<-Betay*(1-rho+(rho*x1/Betax))
    CVar<-Betay^2*((1-rho)^2+2*rho/Betax*(1-rho)*x1)
    Csd<-sqrt(CVar)

    #Conditional density forecasts
    LI<-CEV-4.5*Csd
    LI<-if(LI<0){0}else{LI}
    LS<-CEV+4.5*Csd
    ny<-seq(LI,LS, by=(LS-LI)/10000)
    fyx<-(1/(1-rho)*Betay)*exp(-1/(1-rho)*((x1/Betax*rho)+ny/Betay))*besselI(2*sqrt(rho*x1*ny/(Betay*Betax))/(1-rho),0)
    fyx[is.infinite(fyx)]<-NA
    fyx<-fyx[!is.na(fyx)]


    # Building of the CDF of the conditional distribution (numerical integration of C_y (Riemann Sum))
    dyx<-ny[2]-ny[1]
    C <- sum(fyx)*dyx #normalizing constant
    Fyx<-cumsum(fyx)*dyx/C


    # Export values
    Cond_mean<-CEV
    Cond_sd<- Csd
    CPDF<-data.frame(ny,fyx,Fyx)
    colnames(CPDF)<-c("yc","fyx","Fyx")
    Statistics<-data.frame(Cond_mean,Cond_sd)
    Results<-list()
    Results[["CPDF"]]<-CPDF
    Results[["Statistics"]]<-Statistics
    return(Results)

  }

#-------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
# 2. Obtain the conditional distribution for each value of x

  ConDs<-list()
  Cmean<-rep(0, length(x))
  Csd<-rep(0, length(x))

  for (i in 1:length(x)){
    Results<-ConD(x1=x[i],rho,Betax,Betay)
    ConDs[[paste("ConDs_",i)]]<-Results$CPDF
    Cmean[i]<-Results$Statistics$Cond_mean
    Csd[i]<-Results$Statistics$Cond_sd
  }

#-------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
# 3. Export results

  Values<-data.frame(x,Cmean,Csd)
  RCond<-list()
  RCond[["Conditional_Statistics"]]<-Values
  RCond[["Condyx"]]<-ConDs
  return(RCond)

}



