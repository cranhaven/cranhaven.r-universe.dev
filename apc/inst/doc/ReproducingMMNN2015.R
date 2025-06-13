### R code from vignette source 'ReproducingMMNN2015.Rnw'

###################################################
### code chunk number 1: ReproducingMMNN2015.Rnw:94-96
###################################################
library(apc)
data <- data.asbestos()


###################################################
### code chunk number 2: ReproducingMMNN2015.Rnw:107-108
###################################################
apc.plot.data.all(data)


###################################################
### code chunk number 3: ReproducingMMNN2015.Rnw:119-120
###################################################
apc.plot.data.sums(data)


###################################################
### code chunk number 4: ReproducingMMNN2015.Rnw:123-137
###################################################
data.sums <- apc.data.sums(data)
par(mfrow=c(2,2),oma=c(0,0,2,0),mar=c(4,4,2,0)+0.1)
plot(seq(25,89),data.sums$sums.age,
    main="(a) sums by age",
    type="l",xlab="age",ylab="age data sum")
plot(seq(1967,2007),data.sums$sums.per,
    main="(b) sums by period",
    type="l",xlab="period",ylab="period data sum")
plot(seq(1878,1982),data.sums$sums.coh,
    main="(c) sums by cohort",
    type="l",xlab="cohort",ylab="cohort data sum")
apc.plot.data.within(data,plot.type="pwc",
    thin=5,type="l",main="(d)",lty=1,legend=FALSE)
title("Figure 1",outer=TRUE)    


###################################################
### code chunk number 5: ReproducingMMNN2015.Rnw:144-145
###################################################
apc.fit.table(data,"poisson.response")[1:4,1:6]


###################################################
### code chunk number 6: ReproducingMMNN2015.Rnw:153-154
###################################################
fit.apc <- apc.fit.model(data,"poisson.response","APC")


###################################################
### code chunk number 7: ReproducingMMNN2015.Rnw:158-159
###################################################
apc.plot.fit.all(fit.apc)


###################################################
### code chunk number 8: ReproducingMMNN2015.Rnw:171-172
###################################################
apc.plot.fit.pt(fit.apc,main="Figure 3 - approximately")


###################################################
### code chunk number 9: ReproducingMMNN2015.Rnw:180-181
###################################################
fit.ac <- apc.fit.model(data,"poisson.response","AC")


###################################################
### code chunk number 10: ReproducingMMNN2015.Rnw:203-206
###################################################
forecast.16 <- apc.forecast.ac(fit.ac,sum.per.by.coh=c(42,89))
forecast.30 <- apc.forecast.ac(fit.ac,sum.per.by.coh=c(42,75))
forecast.45 <- apc.forecast.ac(fit.ac,sum.per.by.coh=c(42,60))


###################################################
### code chunk number 11: ReproducingMMNN2015.Rnw:213-214
###################################################
data.sum.per <- apc.data.sums(data.asbestos())$sums.per


###################################################
### code chunk number 12: ReproducingMMNN2015.Rnw:231-242
###################################################
plot(seq(1967,2007),data.sum.per,xlim=c(1967,2047),ylim=c(0,3500),
    xlab="period",ylab="number of cases",
    main="Figure 6")
apc.polygon(forecast.16$response.forecast.per.by.coh,
    2007,TRUE,TRUE,col.line=1)
apc.polygon(forecast.30$response.forecast.per.by.coh,
    2007,TRUE,TRUE,col.line=2)
apc.polygon(forecast.45$response.forecast.per.by.coh,
    2007,TRUE,TRUE,col.line=3)
legend("topleft",legend=c("1966","1952","1937"),
    col=c(1,2,3),lty=1)


###################################################
### code chunk number 13: ReproducingMMNN2015.Rnw:255-258
###################################################
data.1991 <- apc.data.list.subset(data.asbestos(),0,0,0,16,0,0)
fit.ac.1991 <- apc.fit.model(data.1991,"poisson.response","AC")
forecast.1991 <- apc.forecast.ac(fit.ac.1991)


###################################################
### code chunk number 14: ReproducingMMNN2015.Rnw:269-277
###################################################
data.2001 <- apc.data.list.subset(data.asbestos(),0,0,0,6,0,0)
fit.ac.2001 <- apc.fit.model(data.2001,"poisson.response","AC")
forecast.2001 <- apc.forecast.ac(fit.ac.2001)
data.2006 <- apc.data.list.subset(data.asbestos(),0,0,0,1,0,0)
fit.ac.2006 <- apc.fit.model(data.2006,"poisson.response","AC")
forecast.2006 <- apc.forecast.ac(fit.ac.2006)
fit.ac.2007 <- apc.fit.model(data.asbestos(),"poisson.response","AC")
forecast.2007 <- apc.forecast.ac(fit.ac.2007)


###################################################
### code chunk number 15: ReproducingMMNN2015.Rnw:282-297
###################################################
plot(seq(1967,2007),data.sum.per,xlim=c(1967,2047),ylim=c(0,3500),
    xlab="period",ylab="number of cases",
    main="Figure 7")
apc.polygon(forecast.2007$response.forecast.per.ic,
    2007,TRUE,TRUE,col.line=1)
apc.polygon(forecast.2007$response.forecast.per   ,
    2007,FALSE    ,col.line=2)
apc.polygon(forecast.2006$response.forecast.per   ,
    2006,FALSE    ,col.line=3)
apc.polygon(forecast.2001$response.forecast.per   ,
    2001,FALSE    ,col.line=4)
apc.polygon(forecast.1991$response.forecast.per   ,
    1991,FALSE    ,col.line=5)
legend("topleft",legend=c("2007ic","2007","2006","2001","1991"),
    col=c(1,2,3,4,5),lty=1)


###################################################
### code chunk number 16: ReproducingMMNN2015.Rnw:305-307
###################################################
forecast.2007$response.forecast.per[10:14,1]
forecast.2007$response.forecast.per.ic[10:14,1]


###################################################
### code chunk number 17: ReproducingMMNN2015.Rnw:323-329
###################################################
data.coh.1966 <- apc.data.list.subset(data.asbestos(),0,0,0,0,0,16)
fit.ac.coh.1966 <- apc.fit.model(data.coh.1966,"poisson.response","AC")
forecast.coh.1966 <- apc.forecast.ac(fit.ac.coh.1966)
data.age.35 <- apc.data.list.subset(data.asbestos(),10,0,0,0,0,0)
fit.ac.age.35 <- apc.fit.model(data.age.35,"poisson.response","AC")
forecast.age.35 <- apc.forecast.ac(fit.ac.age.35,sum.per.by.coh=c(42,89))


###################################################
### code chunk number 18: ReproducingMMNN2015.Rnw:339-341
###################################################
fit.apc.1966 <- apc.fit.model(data.coh.1966,"poisson.response","APC")
forecast.apc.1966   <- apc.forecast.apc(fit.apc.1966)


###################################################
### code chunk number 19: ReproducingMMNN2015.Rnw:351-370
###################################################
plot(seq(1967,2007),data.sum.per,xlim=c(1967,2047),ylim=c(0,3500),
    xlab="period",ylab="number of cases",
    main="Figure 8")
apc.polygon(forecast.16$response.forecast.per.by.coh    ,
    2007,FALSE,col.line=1)
apc.polygon(forecast.coh.1966$response.forecast.per     ,
    2007,FALSE,col.line=2)
apc.polygon(forecast.age.35$response.forecast.per.by.coh,
    2007,FALSE,col.line=3)
apc.polygon(forecast.16$response.forecast.per.by.coh.ic ,
    2007,FALSE,col.line=4)
apc.polygon(forecast.apc.1966$response.forecast.per     ,
    2007,FALSE,col.line=5)
legend("topleft",legend=c("est: full, fore: coh<=1966",
                          "est: coh<=1966, fore: coh<=1966",
                          "est: age>=35, fore: coh<=1966",
                          "est: full, fore: coh<=1966, ic",
                          "est: apc, I0: coh<=1966"),
    col=c(1,2,3,4,5),lty=1)


###################################################
### code chunk number 20: ReproducingMMNN2015.Rnw:377-381
###################################################
plot(seq(1967,2007),data.sum.per,xlim=c(1967,2047),ylim=c(0,3500),
    xlab="period",ylab="number of cases",
    main="Figure 8")
apc.polygon(forecast.16$response.forecast.per.by.coh.ic,2007,TRUE,TRUE)


