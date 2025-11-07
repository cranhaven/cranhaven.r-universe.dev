\name{psdfun}
\alias{psdfun}
\title{
Regression analysis for survival curves.
}
\description{
Regression analysis for survival curves between number of individuals and age class.
}
\usage{
psdfun(ax,a=100,b=6,index="Deevey2")
}
\arguments{
  \item{ax}{
Population number of within different age class.
}
  \item{a}{
Initial values for model fitting.
}
  \item{b}{
Initial values for model fitting.
}
  \item{index}{
Forms of survival curves,which includes:Deevey1, Deevey2,Deevey3.Note:Deevey1 is the linear model; Deevey3 is the exponential model;Deevey2 is the power model.
}
}
\details{
Regression analysis for survival curves between number of individuals and age class.
}
\value{
Result returns the results of regression analysis for survival curves.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\references{
Zhang Y, Wang J, Wang X, Wang L, Wang Y, Wei J, et al. 2024. Population structures and dynamics of Rhododendron communities with different stages of succession in northwest Guizhou, China. Plants-Basel 13.
}
\examples{
data(Npop)
psd_D1<-psdfun(ax=Npop$ax,index="Deevey1")
psd_D1
psd_D2<-psdfun(ax=Npop$ax,index="Deevey2")
psd_D2
psd_D3<-psdfun(ax=Npop$ax,index="Deevey3")
psd_D3
library(ggplot2)
psdnls.p<-ggplot()+geom_bar(aes(x=age,y=ax,group=ageclass),data=psd_D2$Data,stat = "identity")+
  geom_line(aes(x=age,y=predict),color="blue",linewidth=1,data=psd_D2$Data)+
  geom_text(aes(x=10,y=7700),label=expression(paste(italic(y),"=aexp(-b",italic(x),")")))+
  geom_text(aes(x=10,y=7300),label=expression(paste(R^2,"=0.987")))+
  scale_x_continuous(breaks=1:11)+
  scale_x_discrete(limits=psd_D2$Data$ageclass)+
  xlab("Age class")+ylab("Number of individuals")
psdnls.p
}
