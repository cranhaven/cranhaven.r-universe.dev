#' @title Contour plot of the bivariate exponential distribution (BED) based on the Moran-Downton model
#'
#' @description This function builds the contour plot of the BED. The required inputs are the correlation coefficient, the
#' scale parameters of the marginal distributions, and the values of the cumulative joint probabilities associated
#' to the contours (Default values 5\%, 25\%, 75\%, and 95\%). This function also allows pair of values to be plotted on
#' the contour plot.
#'
#'
#' @param rho Correlation coefficient between marginal distributions of x and y.
#' @param Betax Scale parameter of the marginal distribution of x.
#' @param Betay Scale parameter of the marginal distribution of y.
#' @param Pr A value or vector of values specifing the cumulative joint probability associated to the contour plot.
#'           Default values 5\%, 25\%, 75\%, and 95\%.
#' @param xlabel Label of the x-axis.
#' @param ylabel Label of the y-axis.
#' @param title Title of the figure.
#' @param Rvalues True or False variable (Optional). If TRUE is specified, pairs (x,y) are plotted on the contour plot.
#' If FALSE is specifed, only the contours are plotted. Default FALSE.
#' @param n Numbers of pairs (x,y) to be plotted if Rvalues is specified as TRUE. Default value 1000.
#'
#'
#' @return The Contour plot of the BED is provided.
#'
#' @author Luis F. Duque <lfduquey@@gmail.com> <l.f.duque-yaguache2@@newcastle.ac.uk>
#'
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @import ggplot2
#'
#'
#' @examples \donttest{Contour_Plot(rho=0.5,Betax=1,Betay=1)}
#' @export
Contour_Plot<-function(rho,Betax,Betay,Pr=c(5,25,75,95),xlabel="x",ylabel="y",title="BED",
                       Rvalues=FALSE, n=1000){

# define Global variables
i<-d<-NULL

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 1. Theoretical Joint PDF
# Eq. 10.53 "Continuous Bivariate Distributions" Balakrishnan and Lai (2009)
# Eq. 18 Nagao and Kandoya (1971)

  PDF<-function(rho1,x1,y1,Betax1,Betay1){
    A<- 1/((1-rho1)*Betax1*Betay1)
    B<-exp(-((x1/Betax1)+(y1/Betay1))/(1-rho1))
    C<-besselI(2*sqrt(rho1*(x1/Betax1)*(y1/Betay1))/(1-rho1),0)
    d1<-A*B*C
    return(d1)
  }


#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 2. Create pair of values

  x1<-seq(0.01,Betax*5,by=0.01)
  y1<-seq(0.01,Betay*5,by=0.01)

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 3.Apply the PDF function for each pair of values

  x<-mapply(rep,x1,length(y1))
  x<-c(x)
  y<-rep(y1,length(x1))
  df<-data.frame(x,y)
  df$d<-mapply(PDF,rho,df$x,df$y,Betax,Betay)


#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 4. Define the contours to be plotted.

  z <- sort(df$d)
  c<- cumsum(z) * 0.01 * 0.01
  Countours<-stats::approx(c,z, xout = 1 - Pr/100)$y

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
# 5. Genarate random values

  if (Rvalues==TRUE){

#-------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
# 5.1 Function to generate random values.


EBDUS<-function(n,rho){


#------------------------------------------------------------------------------------------------------------------
# 5.1.1 Function of the conditional distribution of y given x.
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
        # based on https://stackoverflow.com/questions/40851328/compute-area-under-density-estimation-curve-i-e-probability/52415208#52415208


        dx<-neta[2]-neta[1]
        C <- sum(C_eta,na.rm = TRUE)*dx #normalizing constant
        Fx<-cumsum(C_eta)*dx/C  # the fastest way to find the Fx



        # Generate a value of eta by drawing a ramdon value from the conditional distribution.

        pUn<-stats::punif(stats::runif(1))
        Rvalue<-stats::approx(Fx,neta,pUn)$y


        # Export values
        Results<-data.frame(CEV,Csd,Rvalue)
        return(Results)
      }

#-------------------------------------------------------------------------------------------------------------------
# 5.1.2 Generate values of etahat and arrange them in ascending order.

etahat<-stats::rexp(n,rate = 1)
etahat<-base::sort(etahat)


#-------------------------------------------------------------------------------------------------------------------
# 5.1.3 Generate the conditional distribution for each value of etahat and draw a value from this distribution to
# generate the correlated value of eta.

      eta<-rep(0,length(etahat))

      numCores <- ifelse(parallel::detectCores()>=2,2,1)
      doParallel::registerDoParallel(numCores)
      eta<-foreach::foreach(i=1:length(etahat),.combine=c) %dopar% {
        Results<-ConD(etahat[i],rho=rho)
        eta<-Results$Rvalue}
      doParallel::stopImplicitCluster()

#-------------------------------------------------------------------------------------------------------------------
# 5.1.4. Export results
Results<-data.frame(eta,etahat)
return(Results)

    }


#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
# 5.2. Obtain the correlated standarized variables of x and y, i.e. etahat and eta by using the EBDUS function.

    Results<-EBDUS(n,rho)

    # For very extreme values of neta the Bessel reaches infinity (see step 1.1),therefore the bivariate generator
    # might generate NA values
    Standarized_Simulated_Values<-stats::na.omit(Results)

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
# 5.3. Obtain the real variables.

    x<-Standarized_Simulated_Values$eta*Betax
    y<-Standarized_Simulated_Values$etahat*Betay
    RData<-data.frame(x,y)

  }

#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
# 6. Plot contours

  if (Rvalues==TRUE){

    ggplot() +
      theme_bw() +
      geom_point(data = RData,aes(x=RData[,1], y=RData[,2]))+
      stat_contour(data = df,aes(x = x, y = y, z = d),size = 0.5,breaks = Countours,colour = "red")+
      theme(legend.position = "none")+
      xlab(xlabel)+
      ylab(ylabel)+
      ggtitle(title)

  } else{

    ggplot() +
      theme_bw() +
      stat_contour(data = df, aes(x = x, y = y, z = d),size = 0.5,breaks = Countours,colour = "black")+
      theme(legend.position = "none")+
      xlab(xlabel)+
      ylab(ylabel)+
      ggtitle(title)

  }


}

