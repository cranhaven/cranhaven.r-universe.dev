\name{Mpre}
\alias{Mpre}
\title{
Time sequence prediction for the population dynamic changes.
}
\description{
The renewal ability of populations was simulated and predicted using the moving average method.
}
\usage{
Mpre(ax,n=c(2,4,6))
}
\arguments{
  \item{ax}{
Population number of within different age class.
}
  \item{n}{
Number of periods to average over. Must be between 1 and nrow(x), inclusive.
}
}
\details{
The renewal ability of populations was simulated and predicted using the moving average method.
}
\value{
Result returns the results of the simulated and predicted the population dynamic changes using the moving average method.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\references{
Zhang Y, Wang J, Wang X, Wang L, Wang Y, Wei J, et al. 2024. Population structures and dynamics of Rhododendron communities with different stages of succession in northwest Guizhou, China. Plants-Basel 13.
}
\examples{
data(Npop)
Mdata<-Mpre(ax=Npop$ax,n=c(2,3,5,6,8,10))
library(reshape2)
Mdata.melt<-reshape2::melt(Mdata,id=c("rank","ageclass"))
Mdata.melt$ageclass<-factor(Mdata.melt$ageclass,levels=unique(Mdata.melt$ageclass))
library(ggplot2)
Mpre.p<-ggplot()+geom_line(aes(x=ageclass,y=value,color=variable,group=variable),
                           linewidth=0.5,data=Mdata.melt)+
  xlab("Age class")+ylab("Number of individuals")+labs(color=" ")
Mpre.p
}
