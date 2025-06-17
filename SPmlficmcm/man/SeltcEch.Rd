\name{SeltcEch}
\alias{SeltcEch}
\title{
Resampling
}
\description{
The function draws randomly n1+n0 individuals among N1 + N0 individuals with N0 > n0 representing the numbers of non-cases and N1 > n1 representing the number of cases.
}
\usage{
SeltcEch(outc, n1, n0, id, datf)
}
\arguments{
  \item{outc}{Outcome variable (0,1).
}
  \item{n1}{Numeric value representing the number of cases.
}
  \item{n0}{Numeric value representing the number of controls.
}
  \item{id}{Identifying number of the mother-child pair.
}
  \item{datf}{\code{data.frame} of the database.
}
}
\details{
The function uses the \code{\link{sample}} function to resample the database.
}
\value{
A \code{data.frame} with n0 + n1 rows.
}

\seealso{
\code{\link{sample}}
}
\examples{
set.seed(13200)
   M=5000;
   fl1=outc~Z1+Z2+Gm+Gc+Z2:Gm;
   vpo=c(3,4)
   vprob=c(0.35,0.55)
   vcorr=c(2,1)
   theta=0.3
   beta=c(-0.916,0.857,0.405,-0.693,0.573)
   interc=-2.23
   Dataf<-FtSmlrmCMCM(fl1,M,theta,beta,interc,vpo,vprob,vcorr)        
   # Number of subjects eligible to the study in the population
   N0=dim(Dataf[Dataf["outc"]==0,])[1];
   N1=dim(Dataf[Dataf["outc"]==1,])[1]
   N=c(N0,N1)          
   # Sampling of the study database  
   n0=308
   n1=83 
   DatfE1<-SeltcEch("outc",n1,n0,"obs",Dataf)
   DatfE1[1:10,] 
}