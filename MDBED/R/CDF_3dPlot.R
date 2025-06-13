#' @title 3D plot of the joint CDF of the bivariate exponential distribution (BED) based on the Moran-Downton model
#'
#' @description This function builds a 3D plot of the joint CDF of the BED.
#' The required inputs are the correlation coefficient and the scale parameters of the marginal distributions. This
#' function also allows several characteristics of the plot to be set.
#'
#'
#' @param rho Correlation coefficient between marginal distributions of x and y.
#' @param Betax Scale parameter of the marginal distribution of x.
#' @param Betay Scale parameter of the marginal distribution of y.
#' @param xlabel Label of the x-axis.
#' @param ylabel Label of the y-axis.
#' @param zlabel Label of the z-axis.
#' @param title Title of the figure.
#' @param angle Angle of the 3D projection (Default value 45).
#' @param GS Grid spacing; value between 0 and 1 (Default value 0.5).
#'
#' @details Based on the function \code{\link[lattice]{wireframe}} of the \code{\link{lattice}} package.
#'
#' @return A 3D plot of the joint CDF of the BED is provided.
#'
#' @author Luis F. Duque <lfduquey@@gmail.com> <l.f.duque-yaguache2@@newcastle.ac.uk>
#'
#' @importFrom orthopolynom glaguerre.polynomials
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom foreach foreach %dopar%
#' @importFrom lattice wireframe
#' @importFrom graphics plot
#'
#'
#' @examples \donttest{CDF_3dPlot(rho=0.85,Betax=1,Betay=1)}
#' @export
CDF_3dPlot<-function(rho,Betax,Betay,xlabel="x",ylabel="y",zlabel="Joint CDF",
                     title="BED",angle=45,GS=0.5){

# define Global variables
i<-NULL

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#  1. Theoretical Joint CDF
# Eq. 10.54 "Continuous Bivariate Distributions" Balakrishnan and Lai (2009)


  CDF<-function(rho,x1,y1){
    # Term of infinite sum
    n<-50 # number of sums
    lg<-orthopolynom::glaguerre.polynomials(n,1,normalized=FALSE)
    Term<-rep(0,n)
    k<-0

    for (j in 1:n){
      lgj<-lg[[j]]
      lgj<- as.function(lgj)
      Term[j]<-((rho^(k+1))/(k+1)^2)*lgj(x1)*lgj(y1)*x1*y1*exp(-(x1+y1))
      k<-k+j

    }

    InfSum<-sum(Term)
    P<-(1-exp(-x1))*(1-exp(-y1))+InfSum
    return(P)

  }

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 2. Create pair of values and compute the joint CDF

  x1<-seq(0,Betax*4,by=GS)/Betax
  y1<-seq(0,Betay*4,by=GS)/Betay
  x<-mapply(rep,x1,length(y1))
  x<-c(x)
  y<-rep(y1,length(x1))
  df<-data.frame(x,y)

  numCores <- ifelse(parallel::detectCores()>=2,2,1)
  doParallel::registerDoParallel(numCores)


  cdf<-foreach::foreach(i=1:length(df$x),.combine=c) %dopar% {
    cdf<-CDF(rho,df$x[i],df$y[i])
  }
  doParallel::stopImplicitCluster()

  df$CDF<-cdf



#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 3. 3d plot

  df$x<-df$x*Betax
  df$y<-df$y*Betay

  p = lattice::wireframe(df[, 3] ~df[, 1] * df[, 2],scales = list(z.ticks=5,
                                                                  arrows=FALSE, col="black", tck=1),
                         par.settings = list(axis.line = list(col = "transparent"),
                                             box.3d = list(col=c(1,1,NA,NA,1,NA,1,1,1))),
                         screen = list(z = angle,x = -60, y = 0),
                         xlab=xlabel,ylab=ylabel,zlab=list(zlabel, rot = -90),
                         main=title)
  graphics::plot(p)

}
