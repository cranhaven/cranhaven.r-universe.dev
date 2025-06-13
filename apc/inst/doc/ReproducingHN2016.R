### R code from vignette source 'ReproducingHN2016.Rnw'

###################################################
### code chunk number 1: ReproducingHN2016.Rnw:102-105
###################################################
library(apc)
data <- data.loss.TA()
data$response


###################################################
### code chunk number 2: ReproducingHN2016.Rnw:118-121
###################################################
apc.fit.table(data,"od.poisson.response")[,c(1,2,4,6)]
apc.fit.table(data,"od.poisson.response","AC")[,c(1,2,4,6)]
apc.fit.table(data,"od.poisson.response","Ad")[,c(1,2,4,6)]


###################################################
### code chunk number 3: ReproducingHN2016.Rnw:125-130
###################################################
Table2  <- apc.fit.table(data,"od.poisson.response")[c(1:3,5,8),c(2,1,4,6)]
Table2  <- cbind(Table2,rbind(matrix(nrow=3,ncol=2),apc.fit.table(data,"od.poisson.response","AC")[c(2,4),c(4,6)]))
Table2  <- cbind(Table2,rbind(matrix(nrow=4,ncol=2),apc.fit.table(data,"od.poisson.response","Ad")[2,c(4,6)]))
colnames(Table2)<-c("df","Dsub","F_sub,apc","p","F_sub,ac","p","F_sub,ad","p")
Table2


###################################################
### code chunk number 4: ReproducingHN2016.Rnw:147-151
###################################################
apc.fit.model(data,"poisson.response","APC")$coefficients.canonical
apc.fit.model(data,"od.poisson.response","APC")$coefficients.canonical
apc.fit.model(data,"poisson.response","AC")$coefficients.canonical
apc.fit.model(data,"od.poisson.response","AC")$coefficients.canonical


###################################################
### code chunk number 5: ReproducingHN2016.Rnw:155-163
###################################################
Table3  <- apc.fit.model(data,"poisson.response","APC")$coefficients.canonical[,c(1,2)]
Table3  <- cbind(Table3,apc.fit.model(data,"od.poisson.response","APC")$coefficients.canonical[,c(2)])
Tab3  <- apc.fit.model(data,"poisson.response","AC")$coefficients.canonical[,c(1,2)]
Tab3  <- cbind(Tab3,apc.fit.model(data,"od.poisson.response","AC")$coefficients.canonical[,c(2)])
Tab3  <- rbind(Tab3[1:11,],matrix(nrow=8,ncol=3),Tab3[12:19,])
Table3  <- cbind(Table3,Tab3)
colnames(Table3) <- c("apc est","apc se N","apc se t","ac est","ac se N","ac se t")
Table3


###################################################
### code chunk number 6: ReproducingHN2016.Rnw:167-175
###################################################
Table3  <- apc.fit.model(data,"poisson.response","APC",replicate.version.1.3.1=TRUE)$coefficients.canonical[,c(1,2)]
Table3  <- cbind(Table3,apc.fit.model(data,"od.poisson.response","APC",replicate.version.1.3.1=TRUE)$coefficients.canonical[,c(2)])
Tab3  <- apc.fit.model(data,"poisson.response","AC",replicate.version.1.3.1=TRUE)$coefficients.canonical[,c(1,2)]
Tab3  <- cbind(Tab3,apc.fit.model(data,"od.poisson.response","AC",replicate.version.1.3.1=TRUE)$coefficients.canonical[,c(2)])
Tab3  <- rbind(Tab3[1:11,],matrix(nrow=8,ncol=3),Tab3[12:19,])
Table3  <- cbind(Table3,Tab3)
colnames(Table3) <- c("apc est","apc se N","apc se t","ac est","ac se N","ac se t")
Table3


###################################################
### code chunk number 7: ReproducingHN2016.Rnw:184-191
###################################################
ac.fit <- apc.fit.model(data,"od.poisson.response","AC")
ac.forecast <- apc.forecast.ac(ac.fit,quantiles=0.95)
Table4  <- ac.forecast$response.forecast.per[,c(1,6)]
Table4  <- rbind(Table4,ac.forecast$response.forecast.coh[,c(1,6)])
Table4  <- rbind(Table4,ac.forecast$response.forecast.all[,c(1,6)])
rownames(Table4)[19] <- "all"
Table4


###################################################
### code chunk number 8: ReproducingHN2016.Rnw:196-203
###################################################
ac.fit <- apc.fit.model(data,"od.poisson.response","AC",replicate.version.1.3.1=TRUE)
ac.forecast <- apc.forecast.ac(ac.fit,quantiles=0.95)
Table4  <- ac.forecast$response.forecast.per[,c(1,6)]
Table4  <- rbind(Table4,ac.forecast$response.forecast.coh[,c(1,6)])
Table4  <- rbind(Table4,ac.forecast$response.forecast.all[,c(1,6)])
rownames(Table4)[19] <- "all"
Table4


