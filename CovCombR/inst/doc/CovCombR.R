## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=6) 


## ---- cache=TRUE---------------------------------------------------------
library(CovCombR)
data("BarleyPheno")

## ---- cache=TRUE---------------------------------------------------------
library(plyr)
dataall<-rbind.fill(BarleyPheno)
image(as.matrix(dataall[,-c(1,2)]), xlab="trials x genotypes", ylab="traits", axes=FALSE)


## ---- cache=TRUE---------------------------------------------------------
covlist<-lapply(BarleyPheno, function(x){cov(x,use="pairwise.complete.obs")})
covlist<-lapply(covlist,function(x){cov2cor(as.matrix(Matrix::nearPD(x)$mat))})
mean(c(unlist(lapply(covlist,function(x){nrow(x)}))))
BigK<-CovComb(Klist=covlist, maxiter=1000,  loglik = TRUE, plotll = TRUE)

## ------------------------------------------------------------------------
dim(BigK[[1]])

## ------------------------------------------------------------------------
heatmap(as.matrix(BigK[[1]]), cexRow = .2,cexCol = .2)

## ---- cache=TRUE---------------------------------------------------------
Graph_lasso <- qgraph::qgraph(BigK[[1]], graph = "cor", directed=FALSE,details = FALSE,esize = 10,sampleSize=3000, layout="spring",nodeNames = rownames(BigK[[1]]),threshold = "hochberg", legend.cex=.15)


