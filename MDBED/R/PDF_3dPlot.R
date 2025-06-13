#' @title 3D plot of the joint PDF of the bivariate exponential distribution (BED) based on the Moran-Downton model
#'
#' @description This function builds a 3D plot of the joint PDF of the BED. The required inputs are the correlation
#' coefficient and the scale parameters of the marginal distributions. This function also allows several
#' characteristics of the plot to be set.
#'
#'
#' @param rho Correlation coefficient between the marginals distributions of and y.
#' @param Betax Scale parameter of the marginal distribution of x.
#' @param Betay Scale parameter of the marginal distribution of y.
#' @param xlabel Label of the x-axis.
#' @param ylabel Label of the y-axis.
#' @param zlabel Label of the z-axis.
#' @param title Title of the figure.
#' @param angle Angle of the 3D projection.
#' @param GS Grid spacing; value between 0 and 1 (Default value 0.5)
#'
#' @details Based on the function \code{\link[graphics]{persp}} of the \code{\link{graphics}} package.
#'
#' @return A 3D plot of the joint PDF of the BED is provided.
#'
#' @author Luis F. Duque <lfduquey@@gmail.com> <l.f.duque-yaguache2@@newcastle.ac.uk>
#'
#' @importFrom graphics persp
#'
#' @examples PDF_3dPlot(rho=0.85,Betax=1,Betay=1)
#' @export
PDF_3dPlot<-function(rho,Betax,Betay,xlabel="x",ylabel="y",zlabel="Joint PDF",
                     title="BED",angle=-35,GS=0.5){

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 1. Theoretical Joint CDF
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
# 2. Create values of pairs (x,y)

  x<-seq(0,Betax*4,by=GS)
  y<-seq(0,Betay*4,by=GS)

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 3. Apply the PDF for each pair of values


  z<-matrix(0,nrow=length(x),ncol = length(y))


  for (i in 1:length(x)){

    for (j in 1:length(y)){

      z[i,j]<-PDF(rho1 =rho,x1=x[i],y1=y[j],Betax1=Betax,Betay1=Betay)
    }
  }

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 4. Prepare data for plotting


  density3d<-list()
  density3d[["x"]]<-x
  density3d[["y"]]<-y
  density3d[["z"]]<-z

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
# 5. 3d plot

  graphics::persp(density3d,theta =angle,phi =15,xlab=xlabel,ylab=ylabel,zlab=zlabel,main=title,
                  ticktype = "detailed",expand = 0.5,d=6)

}
